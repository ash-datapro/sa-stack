# plots.R — updated
# Goals:
# - Use a consistent ggplot theme for dark cards
# - Avoid forcing axis limits unless appropriate
# - Keep functions pure, safe on empty inputs
# - Make labels readable when embedded in a dark UI

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(ggplot2)
  library(stringr)
})

`%||%` = function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

plot_theme_darkcard = function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", color = "grey92"),
      axis.title = element_text(color = "grey85"),
      axis.text = element_text(color = "grey80"),
      panel.grid.major = element_line(color = "grey25"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = NA, color = NA),
      panel.background = element_rect(fill = NA, color = NA)
    )
}

plot_score_hist = function(pred_df, bins = 25, title = "Score distribution", show_midline = TRUE) {
  if (is.null(pred_df) || nrow(pred_df) == 0 || !"score" %in% names(pred_df) || all(is.na(pred_df$score))) return(NULL)
  
  df = pred_df %>%
    mutate(score = suppressWarnings(as.numeric(score))) %>%
    filter(is.finite(score))
  
  if (nrow(df) == 0) return(NULL)
  
  g = ggplot(df, aes(x = score)) +
    geom_histogram(bins = bins) +
    labs(title = title, x = "score", y = "count") +
    plot_theme_darkcard()
  
  if (isTRUE(show_midline)) {
    g = g + geom_vline(xintercept = 0.5, linetype = "dashed")
  }
  
  g
}

plot_label_counts = function(pred_df, title = "Predicted label counts") {
  if (is.null(pred_df) || nrow(pred_df) == 0 || !"label" %in% names(pred_df)) return(NULL)
  
  cts = pred_df %>%
    mutate(label = as.character(label %||% NA_character_)) %>%
    count(label, sort = TRUE)
  
  if (nrow(cts) == 0) return(NULL)
  
  ggplot(cts, aes(x = reorder(label, n), y = n)) +
    geom_col() +
    coord_flip() +
    labs(title = title, x = NULL, y = "count") +
    plot_theme_darkcard()
}

plot_score_vs_length = function(pred_df, title = "Score vs text length (chars)") {
  if (is.null(pred_df) || nrow(pred_df) == 0 || !"score" %in% names(pred_df)) return(NULL)
  
  df = pred_df %>%
    mutate(
      text = as.character(text %||% ""),
      text_len = nchar(text),
      score = suppressWarnings(as.numeric(score))
    ) %>%
    filter(is.finite(text_len), is.finite(score))
  
  if (nrow(df) == 0) return(NULL)
  
  ggplot(df, aes(x = text_len, y = score)) +
    geom_point(alpha = 0.7) +
    labs(title = title, x = "text length (chars)", y = "score") +
    plot_theme_darkcard()
}
