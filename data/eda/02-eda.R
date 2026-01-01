# --- Professional EDA for SST Treebank (assumes objects already in env) ---
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(ggplot2)
})

# Optional (nice-to-have) packages
has_tidytext = requireNamespace("tidytext", quietly = TRUE)
has_tokenizers = requireNamespace("tokenizers", quietly = TRUE)

stopifnot(
  exists("datasetSentences"),
  exists("datasetSplit"),
  exists("dictionary"),
  exists("sentiment_labels"),
  exists("SOStr"),
  exists("STree")
)

norm_text = function(x) {
  x %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

split_map = c("1" = "train", "2" = "test", "3" = "dev")

# ---------------------------
# 1) Core sentence table + split integrity
# ---------------------------
sent_tbl = datasetSentences %>%
  mutate(
    sentence_index = as.integer(sentence_index),
    sentence_norm = norm_text(sentence)
  )

split_map = c("1" = "train", "2" = "test", "3" = "dev")

split_tbl = datasetSplit %>%
  transmute(
    sentence_index = as.integer(sentence_index),
    split = recode(as.character(splitset_label), !!!split_map, .default = NA_character_)
  ) %>%
  mutate(split = factor(split, levels = c("train", "dev", "test")))

s = sent_tbl %>%
  left_join(split_tbl, by = "sentence_index") %>%
  mutate(
    n_char = nchar(sentence),
    n_char_norm = nchar(sentence_norm)
  )

cat("\n=== Basic counts ===\n")
print(tibble(
  n_sentences = nrow(sent_tbl),
  n_splits = nrow(split_tbl),
  n_joined = nrow(s),
  pct_missing_split = mean(is.na(s$split)) * 100
))

cat("\n=== Split distribution ===\n")
print(s %>% count(split, sort = TRUE) %>% mutate(pct = n / sum(n) * 100))

cat("\n=== Duplicate sentence_index checks ===\n")
print(tibble(
  dup_sentence_index_in_sentences = any(duplicated(sent_tbl$sentence_index)),
  dup_sentence_index_in_splits = any(duplicated(split_tbl$sentence_index))
))

# ---------------------------
# 2) Map sentences -> phrase_id -> sentiment score
# ---------------------------
dict_tbl = dictionary %>%
  transmute(
    phrase_id = as.integer(phrase_id),
    phrase_norm = norm_text(phrase)
  )

lab_tbl = sentiment_labels %>%
  transmute(
    phrase_id = as.integer(phrase_id),
    sentiment = as.double(sentiment)
  )

s_mapped = s %>%
  left_join(dict_tbl, by = c("sentence_norm" = "phrase_norm")) %>%
  left_join(lab_tbl, by = "phrase_id")

cat("\n=== Mapping coverage (sentence -> phrase_id -> sentiment) ===\n")
print(tibble(
  pct_phrase_id_matched = mean(!is.na(s_mapped$phrase_id)) * 100,
  pct_sentiment_matched = mean(!is.na(s_mapped$sentiment)) * 100
))

# ---------------------------
# 3) Label engineering (SST-2 + SST-5-style bins from continuous score)
# ---------------------------
s_labeled = s_mapped %>%
  mutate(
    # SST-2-style (drop neutrals later)
    label_sst2 = case_when(
      sentiment <= 0.4 ~ 0L,
      sentiment >= 0.6 ~ 1L,
      TRUE ~ NA_integer_
    ),
    # SST-5 style bins (common heuristic from [0,1] score)
    label_sst5 = case_when(
      is.na(sentiment) ~ NA_integer_,
      sentiment <= 0.2 ~ 0L,         # very neg
      sentiment <= 0.4 ~ 1L,         # neg
      sentiment <= 0.6 ~ 2L,         # neutral
      sentiment <= 0.8 ~ 3L,         # pos
      TRUE ~ 4L                      # very pos
    )
  )

cat("\n=== Sentiment score summary (matched only) ===\n")
print(s_labeled %>% filter(!is.na(sentiment)) %>% summarise(
  n = n(),
  mean = mean(sentiment),
  sd = sd(sentiment),
  p01 = quantile(sentiment, 0.01),
  p05 = quantile(sentiment, 0.05),
  p50 = quantile(sentiment, 0.50),
  p95 = quantile(sentiment, 0.95),
  p99 = quantile(sentiment, 0.99)
))

cat("\n=== SST-2 (after dropping neutrals) class balance by split ===\n")
print(
  s_labeled %>%
    filter(!is.na(label_sst2), !is.na(split)) %>%
    count(split, label_sst2) %>%
    group_by(split) %>%
    mutate(pct = n / sum(n) * 100) %>%
    arrange(split, label_sst2)
)

cat("\n=== SST-5 class balance by split ===\n")
print(
  s_labeled %>%
    filter(!is.na(label_sst5), !is.na(split)) %>%
    count(split, label_sst5) %>%
    group_by(split) %>%
    mutate(pct = n / sum(n) * 100) %>%
    arrange(split, label_sst5)
)

# ---------------------------
# 4) Leakage / duplication checks across splits
# ---------------------------
dup_cross_split = s_labeled %>%
  filter(!is.na(split)) %>%
  group_by(sentence_norm) %>%
  summarise(n = n(), splits = n_distinct(split), .groups = "drop") %>%
  filter(n > 1, splits > 1)

cat("\n=== Potential leakage: identical normalized sentences appearing in >1 split ===\n")
print(tibble(
  n_cross_split_duplicates = nrow(dup_cross_split),
  pct_of_all_sentences = nrow(dup_cross_split) / nrow(s_labeled) * 100
))

# ---------------------------
# 5) Length / token stats (sentence-level)
# ---------------------------
basic_tokens = function(x) {
  x = str_replace_all(x, "[^[:alnum:]' ]+", " ")
  x = str_replace_all(x, "\\s+", " ")
  x = str_trim(tolower(x))
  ifelse(x == "", 0L, str_count(x, " ") + 1L)
}

s_stats = s_labeled %>%
  mutate(n_token = basic_tokens(sentence_norm))

cat("\n=== Length stats by split ===\n")
print(
  s_stats %>%
    filter(!is.na(split)) %>%
    group_by(split) %>%
    summarise(
      n = n(),
      n_char_median = median(n_char),
      n_token_median = median(n_token),
      n_token_p95 = quantile(n_token, 0.95),
      .groups = "drop"
    )
)

# ---------------------------
# 6) Treebank structure stats (SOStr / STree)
#    (line i corresponds to sentence_index i in the original dataset)
# ---------------------------
tree_tbl = tibble(
  sentence_index = seq_along(SOStr),
  sostr = SOStr,
  stree = STree
) %>%
  mutate(
    n_token_tree = ifelse(is.na(sostr) | sostr == "", 0L, str_count(sostr, "\\|") + 1L),
    n_nodes_tree = ifelse(is.na(stree) | stree == "", 0L, str_count(stree, "\\|") + 1L)
  )

cat("\n=== Tree stats (all lines) ===\n")
print(tree_tbl %>% summarise(
  n = n(),
  token_median = median(n_token_tree),
  token_p95 = quantile(n_token_tree, 0.95),
  nodes_median = median(n_nodes_tree),
  nodes_p95 = quantile(n_nodes_tree, 0.95)
))

# Join tree stats back to sentence table (if indices align)
s_tree = s_stats %>%
  left_join(tree_tbl %>% select(sentence_index, n_token_tree, n_nodes_tree), by = "sentence_index")

# ---------------------------
# 7) Plots (engineer-style quick diagnostics)
# ---------------------------
p1 = ggplot(s_tree %>% filter(!is.na(split)), aes(x = n_token)) +
  geom_histogram(bins = 60) +
  facet_wrap(~split, scales = "free_y") +
  labs(title = "Sentence token length distribution (simple tokenization)", x = "Tokens", y = "Count")

p2 = ggplot(s_labeled %>% filter(!is.na(sentiment), !is.na(split)), aes(x = sentiment)) +
  geom_histogram(bins = 60) +
  facet_wrap(~split, scales = "free_y") +
  labs(title = "Sentiment score distribution", x = "Sentiment score [0,1]", y = "Count")

p3 = ggplot(s_tree %>% filter(!is.na(n_nodes_tree), !is.na(split)), aes(x = n_nodes_tree)) +
  geom_histogram(bins = 60) +
  facet_wrap(~split, scales = "free_y") +
  labs(title = "Parse tree size distribution (nodes)", x = "Nodes", y = "Count")

print(p1); print(p2); print(p3)

# ---------------------------
# 8) Token frequency EDA (optional, but very useful)
# ---------------------------
if (has_tidytext) {
  suppressPackageStartupMessages(library(tidytext))
  
  tokens = s_labeled %>%
    filter(!is.na(split)) %>%
    select(sentence_index, split, sentence_norm, sentiment, label_sst2) %>%
    tidytext::unnest_tokens(word, sentence_norm) %>%
    filter(str_detect(word, "[a-z0-9]")) %>%
    filter(nchar(word) >= 2)
  
  top_overall = tokens %>%
    count(word, sort = TRUE) %>%
    slice_head(n = 30)
  
  cat("\n=== Top 30 tokens overall (no stopword removal) ===\n")
  print(top_overall)
  
  if ("stop_words" %in% ls("package:tidytext")) {
    tokens_nostop = tokens %>% anti_join(tidytext::stop_words, by = "word")
    
    top_by_label = tokens_nostop %>%
      filter(!is.na(label_sst2)) %>%
      count(label_sst2, word, sort = TRUE) %>%
      group_by(label_sst2) %>%
      slice_head(n = 25) %>%
      ungroup()
    
    cat("\n=== Top tokens by SST-2 label (stopwords removed; neutrals dropped) ===\n")
    print(top_by_label)
  }
} else if (has_tokenizers) {
  suppressPackageStartupMessages(library(tokenizers))
  
  words = tokenizers::tokenize_words(s_stats$sentence_norm, lowercase = TRUE)
  word_tbl = tibble(word = unlist(words)) %>%
    filter(str_detect(word, "[a-z0-9]"), nchar(word) >= 2) %>%
    count(word, sort = TRUE) %>%
    slice_head(n = 30)
  
  cat("\n=== Top 30 tokens overall (tokenizers) ===\n")
  print(word_tbl)
} else {
  cat("\n(tidytext/tokenizers not installed) Skipping token-frequency EDA.\n")
}

# ---------------------------
# 9) Exportable EDA objects for downstream modeling
# ---------------------------
eda = list(
  sentences = s_labeled,
  sentences_with_tree = s_tree,
  tree_stats = tree_tbl,
  cross_split_duplicates = dup_cross_split
)

invisible(eda)
