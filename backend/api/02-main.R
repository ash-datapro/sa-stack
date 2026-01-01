# main.R — Sentiment API (plumber)
# Loads model.rds (your trained bundle) and serves /predict with a stable contract,
# and logs every request to Postgres for Tableau "operational" dashboards.
#
# Run:
#   R -q -e "plumber::plumb('main.R')$run(host='0.0.0.0', port=8000)"
#
# Env vars:
#   SENTIMENT_MODEL_PATH        path to model.rds
#   SENTIMENT_MAX_BATCH         max texts per request (default 64)
#   SENTIMENT_MAX_CHARS         max chars per text (default 5000)
#   SENTIMENT_LOGGING_ENABLED   "1" to enable DB logging (default 1)
#   SENTIMENT_SCHEMA            schema with prediction_logs table (default "sentiment")
#
# DB logging connection (reuse SST_* vars):
#   SST_DB_HOST, SST_DB_PORT, SST_DB_NAME, SST_DB_USER, SST_DB_PASSWORD

suppressPackageStartupMessages({
  library(plumber)
  library(jsonlite)
  library(tibble)
  library(dplyr)
  library(stringr)
  
  library(DBI)
  library(RPostgres)
  
  library(workflows)
  library(parsnip)
  library(recipes)
  library(textrecipes)
})

# ---------------------------
# 0) Load model bundle
# ---------------------------
MODEL_PATH = Sys.getenv("SENTIMENT_MODEL_PATH", unset = "model.rds")

model_bundle = NULL
load_model = function(path = MODEL_PATH) readRDS(path)

tryCatch({
  model_bundle = load_model(MODEL_PATH)
}, error = function(e) {
  model_bundle <<- NULL
  message("Error loading model bundle at ", MODEL_PATH, ": ", conditionMessage(e))
})

# Model version (stable key for Tableau joins)
get_model_version = function(mb) {
  if (is.null(mb)) return(NA_character_)
  
  # Prefer explicit version if you added it in publishing
  if (!is.null(mb$model_version) && is.character(mb$model_version) && nzchar(mb$model_version)) {
    return(mb$model_version)
  }
  
  # Otherwise use created_utc (what you asked for) as the stable identifier
  if (!is.null(mb$created_utc) && is.character(mb$created_utc) && nzchar(mb$created_utc)) {
    return(mb$created_utc)
  }
  
  NA_character_
}

# ---------------------------
# 1) Utilities
# ---------------------------
now_ms = function() as.numeric(Sys.time()) * 1000
now_iso = function() format(Sys.time(), tz = "UTC", usetz = TRUE)

is_scalar_string = function(x) is.character(x) && length(x) == 1 && !is.na(x)

coerce_texts = function(body) {
  if (is.null(body) || !is.list(body)) return(NULL)
  
  if (!is.null(body[["text"]]) && is_scalar_string(body[["text"]])) return(as.character(body[["text"]]))
  if (!is.null(body[["input"]]) && is_scalar_string(body[["input"]])) return(as.character(body[["input"]]))
  
  xs = body[["texts"]]
  if (is.null(xs)) xs = body[["inputs"]]
  
  if (!is.null(xs)) {
    if (is.character(xs) && length(xs) >= 1) return(as.character(xs))
    if (is.list(xs) && length(xs) >= 1) return(as.character(unlist(xs)))
  }
  
  NULL
}

validate_request = function(texts) {
  if (is.null(model_bundle)) stop("Model not loaded")
  
  if (is.null(texts) || !is.character(texts) || length(texts) < 1) {
    stop("Body must include `text` (string) or `texts` (array of strings)")
  }
  
  texts = str_replace_all(texts, "\\s+", " ")
  texts = str_trim(texts)
  
  if (any(is.na(texts)) || any(texts == "")) stop("All inputs must be non-empty strings")
  
  max_n = as.integer(Sys.getenv("SENTIMENT_MAX_BATCH", unset = "64"))
  if (length(texts) > max_n) stop("Batch too large. Max texts = ", max_n)
  
  max_chars = as.integer(Sys.getenv("SENTIMENT_MAX_CHARS", unset = "5000"))
  if (any(nchar(texts) > max_chars)) stop("One or more texts exceed max length (", max_chars, " chars)")
  
  texts
}

# Safe prediction wrapper: uses bundle predict() if present, else fitted_workflow
predict_with_bundle = function(texts) {
  if (is.null(model_bundle)) stop("Model not loaded")
  
  if (!is.null(model_bundle$predict) && is.function(model_bundle$predict)) {
    out = model_bundle$predict(model_bundle, texts)
    if (!all(c("text", "label", "score") %in% names(out))) stop("Model bundle predict() returned unexpected schema")
    return(out)
  }
  
  fitted = model_bundle$fitted_workflow
  if (is.null(fitted)) stop("Model bundle missing fitted_workflow")
  
  new_df = tibble(text = texts)
  cls = predict(fitted, new_df, type = "class")
  out = bind_cols(new_df, cls)
  
  label_levels = model_bundle$contract$output_schema$label_levels
  if (length(label_levels) == 2) {
    prob = predict(fitted, new_df, type = "prob")
    pos = label_levels[2]
    score_col = paste0(".pred_", pos)
    if (!score_col %in% names(prob)) stop("Probability column missing: ", score_col)
    
    out = bind_cols(out, prob) %>%
      transmute(text = text, label = .pred_class, score = .data[[score_col]])
  } else {
    out = out %>% transmute(text = text, label = .pred_class, score = NA_real_)
  }
  
  out
}

# ---------------------------
# 1b) Prediction logging (Postgres)
# ---------------------------
LOG_ENABLED = Sys.getenv("SENTIMENT_LOGGING_ENABLED", unset = "1")
LOG_ENABLED = identical(LOG_ENABLED, "1") || identical(tolower(LOG_ENABLED), "true")

LOG_SCHEMA = Sys.getenv("SENTIMENT_SCHEMA", unset = "sentiment")
LOG_TABLE = Sys.getenv("SENTIMENT_LOG_TABLE", unset = "prediction_logs")

db_host = Sys.getenv("SST_DB_HOST", unset = "localhost")
db_port = as.integer(Sys.getenv("SST_DB_PORT", unset = "5432"))
db_name = Sys.getenv("SST_DB_NAME", unset = "sst")
db_user = Sys.getenv("SST_DB_USER", unset = "sst_user")
db_pass = Sys.getenv("SST_DB_PASSWORD", unset = "change_me_strong_password")

log_con = NULL

db_logging_ready = function() {
  if (!LOG_ENABLED) return(FALSE)
  if (!is.null(log_con) && DBI::dbIsValid(log_con)) return(TRUE)
  
  # Try to open connection lazily
  log_con <<- tryCatch(
    dbConnect(
      RPostgres::Postgres(),
      host = db_host,
      port = db_port,
      dbname = db_name,
      user = db_user,
      password = db_pass
    ),
    error = function(e) NULL
  )
  
  !is.null(log_con) && DBI::dbIsValid(log_con)
}

log_predictions = function(texts, preds_df, latency_ms, source, errored, error_message) {
  if (!db_logging_ready()) return(invisible(FALSE))
  
  model_version = get_model_version(model_bundle)
  
  # log one row per input text (Tableau-friendly)
  # NOTE: storing raw text can be sensitive; you can switch to text_hash only later.
  rows = tibble(
    text = texts,
    text_hash = NA_character_,
    label = NA_character_,
    score = NA_real_,
    model_version = model_version,
    latency_ms = as.numeric(latency_ms),
    source = as.character(source %||% "api"),
    extra_json = NA_character_
  )
  
  # optional hash (useful even if you store raw text)
  rows$text_hash = vapply(
    rows$text,
    function(x) digest::digest(x, algo = "sha1"),
    character(1)
  )
  
  if (!is.null(preds_df) && nrow(preds_df) == length(texts) && !errored) {
    rows$label = as.character(preds_df$label)
    rows$score = as.numeric(preds_df$score)
    rows$extra_json = jsonlite::toJSON(list(errored = FALSE), auto_unbox = TRUE, null = "null")
  } else {
    rows$extra_json = jsonlite::toJSON(
      list(errored = TRUE, error = as.character(error_message %||% "unknown")),
      auto_unbox = TRUE, null = "null"
    )
  }
  
  # Ensure digest is available (loaded via namespace if installed; keep dependency explicit)
  # (If you don't want digest, remove text_hash from schema or set NULL.)
  tryCatch({
    dbAppendTable(log_con, Id(schema = LOG_SCHEMA, table = LOG_TABLE), rows)
    TRUE
  }, error = function(e) {
    FALSE
  })
}

# You used %||% in your training; define it here too
`%||%` = function(x, y) if (is.null(x) || (is.character(x) && length(x) == 1 && is.na(x))) y else x

# ---------------------------
# 2) Plumber setup + error handling
# ---------------------------
#* @plumber
function(pr) {
  pr$setErrorHandler(function(req, res, err) {
    msg = conditionMessage(err)
    
    if (grepl("lexical error|invalid char in json|parse", msg, ignore.case = TRUE)) {
      res$status = 400
      return(list(detail = paste0("Invalid JSON body: ", msg)))
    }
    
    res$status = 500
    return(list(detail = msg))
  })
  
  # close DB on exit
  reg.finalizer(environment(), function(e) {
    if (!is.null(log_con) && DBI::dbIsValid(log_con)) {
      try(DBI::dbDisconnect(log_con), silent = TRUE)
    }
  }, onexit = TRUE)
  
  pr
}

# ---------------------------
# 3) Endpoints
# ---------------------------

#* Health check
#* @get /health
function(res) {
  list(
    status = "ok",
    model_loaded = !is.null(model_bundle),
    model_path = MODEL_PATH,
    task = if (!is.null(model_bundle)) model_bundle$task else NA_character_,
    logging_enabled = LOG_ENABLED,
    logging_ready = db_logging_ready()
  )
}

#* Model metadata + prediction contract
#* @get /meta
function(res) {
  if (is.null(model_bundle)) {
    res$status = 503
    return(list(detail = "Model not loaded"))
  }
  
  list(
    model_version = get_model_version(model_bundle),
    created_utc = model_bundle$created_utc,
    task = model_bundle$task,
    threshold = model_bundle$threshold,
    label_levels = model_bundle$contract$output_schema$label_levels,
    contract = model_bundle$contract,
    best_params = model_bundle$best_params,
    data_source = model_bundle$data_source,
    api = list(
      max_batch = as.integer(Sys.getenv("SENTIMENT_MAX_BATCH", unset = "64")),
      max_chars = as.integer(Sys.getenv("SENTIMENT_MAX_CHARS", unset = "5000")),
      logging = list(
        enabled = LOG_ENABLED,
        schema = LOG_SCHEMA,
        table = LOG_TABLE,
        db = list(host = db_host, port = db_port, name = db_name, user = db_user)
      )
    )
  )
}

#* Reload model.rds (useful during deployment)
#* @post /reload
#* @serializer json
function(req, res) {
  token_required = Sys.getenv("SENTIMENT_RELOAD_TOKEN", unset = "")
  if (nzchar(token_required)) {
    token = req$HTTP_X_RELOAD_TOKEN
    if (is.null(token) || !identical(token, token_required)) {
      res$status = 401
      return(list(detail = "Unauthorized"))
    }
  }
  
  mb = tryCatch(load_model(MODEL_PATH), error = function(e) e)
  if (inherits(mb, "error")) {
    res$status = 500
    return(list(detail = paste0("Reload failed: ", mb$message)))
  }
  
  model_bundle <<- mb
  list(
    status = "reloaded",
    model_path = MODEL_PATH,
    model_version = get_model_version(model_bundle),
    created_utc = model_bundle$created_utc
  )
}

#* Predict sentiment for one text or a batch
#*
#* Request body:
#*  { "text": "I loved it" }
#*  { "texts": ["I loved it", "I hated it"] }
#*
#* Response:
#*  {
#*    "model": {"model_version":"...", "created_utc":"...", "task":"sst2", "threshold":0.53},
#*    "latency_ms": 12.3,
#*    "predictions": [{"text":"...","label":"pos","score":0.91,"model_version":"..."}, ...]
#*  }
#*
#* @post /predict
#* @parser json
#* @serializer json
function(req, res) {
  request_source = req$HTTP_X_SOURCE %||% "api"
  
  if (is.null(model_bundle)) {
    res$status = 503
    # log failed request (no preds)
    log_predictions(texts = character(0), preds_df = NULL, latency_ms = NA_real_,
                    source = request_source, errored = TRUE, error_message = "Model not loaded")
    return(list(detail = "Model not loaded"))
  }
  
  body = req$body
  if (is.null(body)) body = list()
  if (!is.list(body)) {
    res$status = 400
    log_predictions(texts = character(0), preds_df = NULL, latency_ms = NA_real_,
                    source = request_source, errored = TRUE, error_message = "JSON body must be an object")
    return(list(detail = "JSON body must be an object"))
  }
  
  texts = coerce_texts(body)
  texts_checked = tryCatch(validate_request(texts), error = function(e) e)
  if (inherits(texts_checked, "error")) {
    res$status = 400
    log_predictions(texts = if (is.null(texts)) character(0) else as.character(texts),
                    preds_df = NULL, latency_ms = NA_real_,
                    source = request_source, errored = TRUE, error_message = texts_checked$message)
    return(list(detail = texts_checked$message))
  }
  texts = texts_checked
  
  t0 = now_ms()
  
  preds = tryCatch(predict_with_bundle(texts), error = function(e) e)
  latency = now_ms() - t0
  
  if (inherits(preds, "error")) {
    res$status = 500
    log_predictions(texts = texts, preds_df = NULL, latency_ms = latency,
                    source = request_source, errored = TRUE, error_message = preds$message)
    return(list(detail = paste0("Prediction failed: ", preds$message)))
  }
  
  # log success
  log_predictions(texts = texts, preds_df = preds, latency_ms = latency,
                  source = request_source, errored = FALSE, error_message = NULL)
  
  mv = get_model_version(model_bundle)
  
  pred_records = lapply(seq_len(nrow(preds)), function(i) {
    list(
      text = preds$text[[i]],
      label = as.character(preds$label[[i]]),
      score = if (is.na(preds$score[[i]])) NULL else jsonlite::unbox(as.numeric(preds$score[[i]])),
      model_version = mv
    )
  })
  
  list(
    model = list(
      model_version = mv,
      created_utc = model_bundle$created_utc,
      task = model_bundle$task,
      threshold = jsonlite::unbox(as.numeric(model_bundle$threshold))
    ),
    latency_ms = jsonlite::unbox(as.numeric(latency)),
    predictions = pred_records
  )
}
