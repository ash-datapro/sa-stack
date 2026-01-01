# ============================================================
# train_sst_model.R
# Production-style sentiment training: Postgres -> TF-IDF -> tuned model
# Saves:
#   reports/metrics_dev.csv
#   reports/metrics_test.csv
#   reports/confusion_dev.csv
#   reports/confusion_test.csv
#   reports/roc_dev.png
#   reports/pr_dev.png
#   model.rds   (bundle w/ fitted workflow + contract metadata)
# ============================================================

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(glue)
  library(ggplot2)
  
  library(rsample)
  library(recipes)
  library(workflows)
  library(parsnip)
  library(tune)
  library(dials)
  library(yardstick)
  
  library(textrecipes)
})

# ---------------------------
# 0) Config
# ---------------------------
set.seed(42)

out_dir = "reports"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# DB settings (env vars recommended)
db_host = Sys.getenv("SST_DB_HOST", unset = "localhost")
db_port = as.integer(Sys.getenv("SST_DB_PORT", unset = "5432"))
db_name = Sys.getenv("SST_DB_NAME", unset = "sst")
db_user = Sys.getenv("SST_DB_USER", unset = "sst_user")
db_pass = Sys.getenv("SST_DB_PASSWORD", unset = "change_me_strong_password")

# Data source: view created earlier (sentence + split + sentiment score)
db_view = Sys.getenv("SST_DB_VIEW", unset = "sst.v_sentence_sentiment")

# Task choice:
# - "sst2" = binary sentiment (drop neutral)
# - "sst5" = 5-class bins
task = Sys.getenv("SST_TASK", unset = "sst2")

# Text pipeline guardrails
max_tokens = as.integer(Sys.getenv("SST_MAX_TOKENS", unset = "40000"))
min_token_count = as.integer(Sys.getenv("SST_MIN_TOKEN_COUNT", unset = "2"))
ngram_max = as.integer(Sys.getenv("SST_NGRAM_MAX", unset = "2"))

# Tuning budget
grid_size = as.integer(Sys.getenv("SST_GRID_SIZE", unset = "25"))

# ---------------------------
# 1) DB connect
# ---------------------------
con = NULL
tryCatch({
  con = dbConnect(
    RPostgres::Postgres(),
    host = db_host,
    port = db_port,
    dbname = db_name,
    user = db_user,
    password = db_pass
  )
  on.exit({ if (!is.null(con) && dbIsValid(con)) dbDisconnect(con) }, add = TRUE)
}, error = function(e) {
  stop("Error creating DB connection: ", conditionMessage(e))
})

# ---------------------------
# 2) Load + validate data
# ---------------------------
load_sst = function(con, view_name) {
  q = glue("
    SELECT
      sentence_index,
      split,
      sentence AS text,
      sentiment
    FROM {`view_name`}
    ORDER BY sentence_index;
  ")
  df = dbGetQuery(con, q)
  
  df %>%
    mutate(
      sentence_index = as.integer(sentence_index),
      split = as.character(split),
      text = as.character(text),
      sentiment = as.double(sentiment)
    ) %>%
    filter(!is.na(sentence_index), !is.na(text), text != "")
}

raw = load_sst(con, db_view)

# If the view join coverage is partial, we only train on labeled rows.
df = raw %>%
  filter(!is.na(sentiment), !is.na(split), split %in% c("train", "dev", "test"))

if (nrow(df) < 1000) {
  warning(glue("Low labeled row count after join ({nrow(df)}). ",
               "If this is unexpectedly small, consider generating labels from PTB tree files ",
               "instead of joining sentence text -> dictionary by string match."))
}

# Create labels based on task
make_labels = function(df, task) {
  if (task == "sst2") {
    # Common binary heuristic: drop neutral
    df %>%
      mutate(
        label = case_when(
          sentiment <= 0.4 ~ "neg",
          sentiment >= 0.6 ~ "pos",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(label)) %>%
      mutate(label = factor(label, levels = c("neg", "pos")))
  } else if (task == "sst5") {
    df %>%
      mutate(
        label = case_when(
          sentiment <= 0.2 ~ "very_neg",
          sentiment <= 0.4 ~ "neg",
          sentiment <= 0.6 ~ "neutral",
          sentiment <= 0.8 ~ "pos",
          TRUE ~ "very_pos"
        )
      ) %>%
      mutate(label = factor(label, levels = c("very_neg", "neg", "neutral", "pos", "very_pos")))
  } else {
    stop("Unknown task: ", task)
  }
}

df = make_labels(df, task)

# Contract columns for training
df = df %>%
  select(sentence_index, split, text, sentiment, label)

# Basic integrity
stopifnot(all(c("train", "dev", "test") %in% unique(df$split)) || length(unique(df$split)) >= 2)

split_counts = df %>% count(split, label) %>% arrange(split, label)
write_csv(split_counts, file.path(out_dir, "split_label_counts.csv"))

# ---------------------------
# 3) Split: use dataset-provided splits (leakage-safe baseline)
# ---------------------------
train_df = df %>% filter(split == "train")
dev_df   = df %>% filter(split == "dev")
test_df  = df %>% filter(split == "test")

if (nrow(dev_df) == 0 || nrow(test_df) == 0) {
  warning("Missing dev/test in DB view. Falling back to rsample initial split from available rows.")
  sp = initial_split(df, prop = 0.8, strata = label)
  train_df = training(sp)
  test_df = testing(sp)
  dev_df = test_df
}

# ---------------------------
# 4) Baselines (always do this)
# ---------------------------
# Baseline 1: majority class (per train distribution)
majority_label = train_df %>%
  count(label, sort = TRUE) %>%
  slice_head(n = 1) %>%
  pull(label) %>%
  as.character()

predict_majority = function(n) rep(majority_label, n)

metric_set_cls = metric_set(accuracy, kap, f_meas, roc_auc, pr_auc, mn_log_loss)

eval_baseline = function(df_eval, name) {
  truth = df_eval$label
  pred_class = factor(predict_majority(nrow(df_eval)), levels = levels(truth))
  
  # for binary tasks, create crude probabilities: 0.5 everywhere except majority 0.51
  if (nlevels(truth) == 2) {
    lvl = levels(truth)
    p_pos = ifelse(pred_class == lvl[2], 0.51, 0.49)
    pred_tbl = tibble(
      .pred_class = pred_class,
      .pred_neg = 1 - p_pos,
      .pred_pos = p_pos
    )
    yard = bind_cols(df_eval %>% select(label), pred_tbl) %>%
      rename(.pred = .pred_class)
  } else {
    yard = tibble(label = truth, .pred = pred_class)
  }
  
  out = tibble(
    model = name,
    dataset = "eval",
    accuracy = accuracy_vec(truth, pred_class),
    kap = kap_vec(truth, pred_class),
    f_meas = f_meas_vec(truth, pred_class)
  )
  
  out
}

baseline_dev = eval_baseline(dev_df, "baseline_majority")
write_csv(baseline_dev, file.path(out_dir, "baseline_dev.csv"))

# ---------------------------
# 5) Recipe: guarded text pipeline -> tf-idf
#    (keep only label + text inside the recipe; keep ids outside)
# ---------------------------
build_recipe = function(df_train) {
  df_train_min = df_train %>%
    transmute(label = label, text = text)
  
  rec = recipe(label ~ text, data = df_train_min) %>%
    step_text_normalization(text) %>%
    step_tokenize(text) %>%
    step_tokenfilter(text, max_tokens = max_tokens, min_times = min_token_count)
  
  # Add n-grams (textrecipes supports one n at a time)
  if (ngram_max >= 2) {
    rec = rec %>% step_ngram(text, num_tokens = 2)
  }
  if (ngram_max >= 3) {
    rec = rec %>% step_ngram(text, num_tokens = 3)
  }
  
  rec %>%
    step_tfidf(text) %>%
    step_zv(all_predictors())
}

rec = build_recipe(train_df)



# ---------------------------
# 6) Models: strong linear baseline + tuned regularization
# ---------------------------
# Logistic regression with elastic net (excellent baseline for TF-IDF)
glmnet_spec = logistic_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_engine("glmnet")

wf = workflow() %>%
  add_recipe(rec) %>%
  add_model(glmnet_spec)

# CV only on TRAIN (dev/test held out)
folds = vfold_cv(train_df, v = 5, strata = label)

grid = grid_random(
  penalty(range = c(-6, 0)),          # 1e-6 to 1
  mixture(range = c(0, 1)),           # ridge -> lasso
  size = grid_size
)

ctrl = control_grid(
  save_pred = TRUE,
  save_workflow = TRUE,
  parallel_over = "resamples",
  verbose = TRUE
)

# Metrics: for multiclass, roc_auc/pr_auc need different estimators; keep stable set.
if (nlevels(train_df$label) == 2) {
  metrics = metric_set(accuracy, kap, f_meas, roc_auc, pr_auc, mn_log_loss)
} else {
  metrics = metric_set(accuracy, kap, f_meas)
}

tuned = tune_grid(
  wf,
  resamples = folds,
  grid = grid,
  metrics = metrics,
  control = ctrl
)

# Choose "best" by log loss if binary; else by accuracy
if (nlevels(train_df$label) == 2) {
  best_params = select_best(tuned, metric = "mn_log_loss")
} else {
  best_params = select_best(tuned, metric = "accuracy")
}

wf_final = finalize_workflow(wf, best_params)

# Fit final on TRAIN only (dev used for thresholding / calibration)
fit_train = fit(wf_final, data = train_df)

# ---------------------------
# 7) Dev evaluation + threshold selection (binary only)
# ---------------------------
predict_eval = function(fitted_wf, df_eval) {
  cls = predict(fitted_wf, df_eval, type = "class")
  if (nlevels(df_eval$label) == 2) {
    prob = predict(fitted_wf, df_eval, type = "prob")
    bind_cols(df_eval %>% select(sentence_index, label), cls, prob)
  } else {
    bind_cols(df_eval %>% select(sentence_index, label), cls)
  }
}

dev_pred = predict_eval(fit_train, dev_df)

# Default threshold is implicit in classifier. For production, choose threshold on dev.
threshold = 0.5
threshold_tbl = NULL

if (nlevels(dev_df$label) == 2) {
  # pick threshold maximizing F1 on dev (common; swap to cost-based if needed)
  thr_grid = tibble(threshold = seq(0.05, 0.95, by = 0.01))
  
  pos_level = levels(dev_df$label)[2]
  prob_pos_col = paste0(".pred_", pos_level)
  
  threshold_tbl = thr_grid %>%
    mutate(
      f1 = map_dbl(threshold, function(t) {
        pred_class = factor(
          ifelse(dev_pred[[prob_pos_col]] >= t, pos_level, levels(dev_df$label)[1]),
          levels = levels(dev_df$label)
        )
        f_meas_vec(dev_pred$label, pred_class, beta = 1)
      })
    )
  
  threshold = threshold_tbl %>%
    slice_max(order_by = f1, n = 1, with_ties = FALSE) %>%
    pull(threshold)
  
  write_csv(threshold_tbl, file.path(out_dir, "threshold_sweep_dev.csv"))
}

# Apply chosen threshold to dev metrics (binary)
score_binary = function(pred_df, threshold) {
  truth = pred_df$label
  pos_level = levels(truth)[2]
  prob_pos_col = paste0(".pred_", pos_level)
  
  pred_class = factor(
    ifelse(pred_df[[prob_pos_col]] >= threshold, pos_level, levels(truth)[1]),
    levels = levels(truth)
  )
  
  tibble(
    accuracy = accuracy_vec(truth, pred_class),
    kap = kap_vec(truth, pred_class),
    f_meas = f_meas_vec(truth, pred_class, beta = 1),
    roc_auc = roc_auc_vec(truth, pred_df[[prob_pos_col]]),
    pr_auc = pr_auc_vec(truth, pred_df[[prob_pos_col]]),
    mn_log_loss = mn_log_loss_vec(truth, pred_df[[prob_pos_col]], event_level = "second")
  )
}

if (nlevels(dev_df$label) == 2) {
  dev_metrics = score_binary(dev_pred, threshold) %>%
    mutate(dataset = "dev", model = "glmnet_tfidf", threshold = threshold)
  write_csv(dev_metrics, file.path(out_dir, "metrics_dev.csv"))
  
  # Save confusion as RDS (no df conversion; version-proof)
  dev_conf = conf_mat(
    tibble(
      truth = dev_pred$label,
      estimate = factor(
        ifelse(dev_pred[[paste0(".pred_", levels(dev_df$label)[2])]] >= threshold,
               levels(dev_df$label)[2], levels(dev_df$label)[1]),
        levels = levels(dev_df$label)
      )
    ),
    truth = truth,
    estimate = estimate
  )
  saveRDS(dev_conf, file.path(out_dir, "confusion_dev.rds"))
  
  # ROC + PR plots (dev)
  roc_df = roc_curve(dev_pred, truth = label, .pred_pos)
  pr_df  = pr_curve(dev_pred, truth = label, .pred_pos)
  
  p_roc = ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
    geom_path() +
    geom_abline(linetype = 2) +
    labs(title = "ROC (dev)", x = "1 - Specificity", y = "Sensitivity")
  
  p_pr = ggplot(pr_df, aes(x = recall, y = precision)) +
    geom_path() +
    labs(title = "Precision-Recall (dev)", x = "Recall", y = "Precision")
  
  ggsave(file.path(out_dir, "roc_dev.png"), p_roc, width = 7, height = 5, dpi = 300)
  ggsave(file.path(out_dir, "pr_dev.png"),  p_pr,  width = 7, height = 5, dpi = 300)
} else {
  # Multiclass: simple metrics + confusion
  dev_metrics = tibble(
    dataset = "dev",
    model = "glmnet_tfidf",
    accuracy = accuracy_vec(dev_pred$label, dev_pred$.pred_class),
    kap = kap_vec(dev_pred$label, dev_pred$.pred_class),
    f_meas = f_meas_vec(dev_pred$label, dev_pred$.pred_class)
  )
  write_csv(dev_metrics, file.path(out_dir, "metrics_dev.csv"))
  
  # Save confusion as RDS (no df conversion; version-proof)
  dev_conf = conf_mat(dev_pred, truth = label, estimate = .pred_class)
  saveRDS(dev_conf, file.path(out_dir, "confusion_dev.rds"))
}

# ---------------------------
# 8) Fit final on TRAIN+DEV, evaluate on TEST
# ---------------------------
train_dev = bind_rows(train_df, dev_df)

fit_final = fit(wf_final, data = train_dev)

test_pred = predict_eval(fit_final, test_df)

if (nlevels(test_df$label) == 2) {
  # Apply threshold chosen on dev
  test_metrics = score_binary(test_pred, threshold) %>%
    mutate(dataset = "test", model = "glmnet_tfidf", threshold = threshold)
  write_csv(test_metrics, file.path(out_dir, "metrics_test.csv"))
  
  test_conf = conf_mat(
    tibble(
      truth = test_pred$label,
      estimate = factor(
        ifelse(test_pred[[paste0(".pred_", levels(test_df$label)[2])]] >= threshold,
               levels(test_df$label)[2], levels(test_df$label)[1]),
        levels = levels(test_df$label)
      )
    ),
    truth = truth,
    estimate = estimate
  )
  
  saveRDS(test_conf, file.path(out_dir, "confusion_test.rds"))
} else {
  test_metrics = tibble(
    dataset = "test",
    model = "glmnet_tfidf",
    accuracy = accuracy_vec(test_pred$label, test_pred$.pred_class),
    kap = kap_vec(test_pred$label, test_pred$.pred_class),
    f_meas = f_meas_vec(test_pred$label, test_pred$.pred_class)
  )
  write_csv(test_metrics, file.path(out_dir, "metrics_test.csv"))
  
  test_conf = conf_mat(test_pred, truth = label, estimate = .pred_class)
  saveRDS(test_conf, file.path(out_dir, "confusion_test.rds"))
}

# ---------------------------
# 9) Bundle + prediction contract + saveRDS
# ---------------------------
contract = list(
  input_schema = list(
    required_cols = c("text"),
    col_types = c(text = "character")
  ),
  output_schema = list(
    required_cols = c("label", "score"),
    label_levels = levels(df$label)
  ),
  notes = list(
    threshold = threshold,
    task = task,
    text_pipeline = list(
      normalization = TRUE,
      tokenization = "textrecipes::step_tokenize",
      tokenfilter = list(max_tokens = max_tokens, min_times = min_token_count),
      ngrams = paste0("1..", ngram_max),
      features = "tf-idf"
    )
  )
)

predict_sentiment = function(model_bundle, new_text) {
  stopifnot(is.character(new_text))
  new_df = tibble(text = new_text)
  
  fitted = model_bundle$fitted_workflow
  cls = predict(fitted, new_df, type = "class")
  out = bind_cols(new_df, cls)
  
  if (length(model_bundle$contract$output_schema$label_levels) == 2) {
    prob = predict(fitted, new_df, type = "prob")
    pos = model_bundle$contract$output_schema$label_levels[2]
    score_col = paste0(".pred_", pos)
    out = bind_cols(out, prob) %>%
      transmute(
        text,
        label = .pred_class,
        score = .data[[score_col]]
      )
  } else {
    out = out %>%
      transmute(
        text,
        label = .pred_class,
        score = NA_real_
      )
  }
  
  out
}

model_bundle = list(
  created_utc = format(Sys.time(), tz = "UTC"),
  data_source = list(
    db = list(host = db_host, port = db_port, name = db_name),
    view = db_view,
    n_rows = nrow(df),
    split_counts = split_counts
  ),
  task = task,
  best_params = best_params,
  threshold = threshold,
  fitted_workflow = fit_final,
  contract = contract,
  predict = predict_sentiment
)

saveRDS(model_bundle, file = "model.rds")

message("✅ Training complete")
message("• Reports: ", normalizePath(out_dir))
message("• Model bundle: ", normalizePath("model.rds"))

smoke = model_bundle$predict(model_bundle, c(
  "This movie was fantastic and I loved it.",
  "Worst thing I have ever watched."
))
print(smoke)
