# data/eda/make-data-snapshot.R
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(stringr)
})

DATA_RDS = "data/sst_treebank.rds"
OUT_PNG = "media/data_snapshot.png"

# Theme colors (match Shiny)
COL_BG = "#071a2f"
COL_FG = "#f8fafc"
COL_MUTED = "rgba(248,250,252,.70)"  # (note: ggplot doesn't support rgba well; we'll use a hex-ish)
COL_GRID = "#16324f"
COL_PRIMARY = "#3b82f6"

sst = readRDS(DATA_RDS)

dict = sst$dictionary
lab = sst$sentiment_labels

phr = dict %>%
  left_join(lab, by = "phrase_id") %>%
  transmute(
    text = phrase,
    score = as.numeric(sentiment),
    n_words = str_count(as.character(phrase), "\\S+"),
    n_chars = nchar(as.character(phrase))
  ) %>%
  filter(!is.na(score))

p = ggplot(phr, aes(x = score)) +
  geom_histogram(
    bins = 40,
    fill = COL_PRIMARY,
    color = COL_PRIMARY,
    alpha = 0.85
  ) +
  labs(
    title = "",
    subtitle = "",
    x = "Sentiment score",
    y = "Count"
  ) +
  scale_x_continuous(limits = c(0, 1)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = COL_BG, color = NA),
    panel.background = element_rect(fill = COL_BG, color = NA),
    legend.background = element_rect(fill = COL_BG, color = NA),
    legend.key = element_rect(fill = COL_BG, color = NA),
    
    text = element_text(color = COL_FG),
    plot.title = element_text(color = COL_FG, face = "bold"),
    plot.subtitle = element_text(color = "#cbd5e1"),
    axis.title = element_text(color = COL_FG),
    axis.text = element_text(color = "#cbd5e1"),
    
    panel.grid.major = element_line(color = COL_GRID, linewidth = 0.3),
    panel.grid.minor = element_blank()
  )

dir.create(dirname(OUT_PNG), recursive = TRUE, showWarnings = FALSE)
ggsave(OUT_PNG, p, width = 10, height = 5.5, dpi = 180, bg = COL_BG)

message("Saved: ", OUT_PNG)
