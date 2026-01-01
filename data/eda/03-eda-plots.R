# --- Save EDA plots (assumes you already ran the EDA and have s_labeled / s_tree and p1/p2/p3) ---
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(tidyr)
})

eda_plot_dir = "~/Desktop/Project/sentiment-api/data/eda-plots"
dir.create(eda_plot_dir, showWarnings = FALSE, recursive = TRUE)

save_plot = function(p, name, w = 10, h = 6, dpi = 300) {
  ggsave(filename = file.path(eda_plot_dir, paste0(name, ".png")),
         plot = p, width = w, height = h, dpi = dpi)
  ggsave(filename = file.path(eda_plot_dir, paste0(name, ".pdf")),
         plot = p, width = w, height = h)
  invisible(TRUE)
}

# If you didn't keep p1/p2/p3 from earlier, recreate them safely:
if (!exists("p1")) {
  p1 = ggplot(s_tree %>% filter(!is.na(split)), aes(x = n_token)) +
    geom_histogram(bins = 60) +
    facet_wrap(~split, scales = "free_y") +
    labs(title = "Sentence token length distribution", x = "Tokens", y = "Count")
}
if (!exists("p2")) {
  p2 = ggplot(s_labeled %>% filter(!is.na(sentiment), !is.na(split)), aes(x = sentiment)) +
    geom_histogram(bins = 60) +
    facet_wrap(~split, scales = "free_y") +
    labs(title = "Sentiment score distribution", x = "Sentiment score [0,1]", y = "Count")
}
if (!exists("p3")) {
  p3 = ggplot(s_tree %>% filter(!is.na(n_nodes_tree), !is.na(split)), aes(x = n_nodes_tree)) +
    geom_histogram(bins = 60) +
    facet_wrap(~split, scales = "free_y") +
    labs(title = "Parse tree size distribution (nodes)", x = "Nodes", y = "Count")
}

# Class balance (SST-2) by split (after dropping neutrals)
p_sst2_balance = s_labeled %>%
  filter(!is.na(split), !is.na(label_sst2)) %>%
  count(split, label_sst2) %>%
  group_by(split) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = factor(label_sst2), y = pct)) +
  geom_col() +
  facet_wrap(~split) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "SST-2 class balance by split (neutrals dropped)", x = "Label (0=neg, 1=pos)", y = "Percent")

# Class balance (SST-5) by split
p_sst5_balance = s_labeled %>%
  filter(!is.na(split), !is.na(label_sst5)) %>%
  count(split, label_sst5) %>%
  group_by(split) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = factor(label_sst5), y = pct)) +
  geom_col() +
  facet_wrap(~split) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "SST-5 class balance by split", x = "Class (0..4)", y = "Percent")

# Relationship checks: length vs sentiment, and tree nodes vs tokens
p_len_vs_sent = s_tree %>%
  filter(!is.na(split), !is.na(sentiment)) %>%
  ggplot(aes(x = n_token, y = sentiment)) +
  geom_point(alpha = 0.25) +
  facet_wrap(~split) +
  labs(title = "Sentence length vs sentiment", x = "Tokens", y = "Sentiment [0,1]")

p_tree_nodes_vs_tokens = s_tree %>%
  filter(!is.na(split), !is.na(n_nodes_tree), !is.na(n_token_tree)) %>%
  ggplot(aes(x = n_token_tree, y = n_nodes_tree)) +
  geom_point(alpha = 0.25) +
  facet_wrap(~split) +
  labs(title = "Tree size vs token count", x = "Tree tokens", y = "Tree nodes")

# Save everything
plots = list(
  token_length_by_split = p1,
  sentiment_distribution_by_split = p2,
  tree_nodes_by_split = p3,
  sst2_class_balance = p_sst2_balance,
  sst5_class_balance = p_sst5_balance,
  length_vs_sentiment = p_len_vs_sent,
  tree_nodes_vs_tree_tokens = p_tree_nodes_vs_tokens
)

purrr::iwalk(plots, ~save_plot(.x, .y))

cat("Saved plots to: ", normalizePath(eda_plot_dir), "\n")
list.files(eda_plot_dir)
