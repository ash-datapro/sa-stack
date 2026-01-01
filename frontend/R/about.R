# R/about.R — About content for Sentiment Explorer
# This file is meant to be sourced by app.R and used inside the About tab.
#
# Usage in app.R:
#   source("R/about.R", local = TRUE)
#   ... in UI:
#   about_ui(default_data_path = DEFAULT_DATA_RDS, default_model_path = DEFAULT_MODEL_RDS)

about_ui = function(default_data_path = NULL, default_model_path = NULL) {
  suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(bsicons)
  })
  
  small_kv = function(k, v) {
    tags$div(
      class = "small-muted",
      tags$b(paste0(k, ": ")),
      tags$span(v %||% "—")
    )
  }
  
  bullet = function(icon, title, body) {
    tags$div(
      class = "about-bullet",
      tags$div(class = "about-ic", bs_icon(icon)),
      tags$div(
        class = "about-bd",
        tags$div(class = "about-ttl", title),
        tags$div(class = "about-txt", body)
      )
    )
  }
  
  card(
    card_header(
      tags$div(
        style = "display:flex; align-items:center; gap:10px;",
        bs_icon("info-circle"),
        tags$div(
          tags$div(style = "font-weight:800; font-size:18px;", "About this project"),
          tags$div(class = "small-muted", "A production-style sentiment platform in R — data → training → model bundle → user interface.")
        )
      )
    ),
    card_body(
      tags$style(HTML("
        .about-grid { display: grid; grid-template-columns: 1fr; gap: 12px; }
        @media (min-width: 900px) { .about-grid { grid-template-columns: 1fr 1fr; } }

        .about-bullet {
          display:flex; gap:12px; align-items:flex-start;
          background: rgba(255,255,255,.04);
          border: 1px solid rgba(255,255,255,.10);
          border-radius: 16px;
          padding: 12px 12px;
        }
        .about-ic svg { width: 22px; height: 22px; color: rgba(59,130,246,.95); margin-top: 2px; }
        .about-ttl { font-weight: 800; letter-spacing: -.01em; margin-bottom: 4px; }
        .about-txt { color: rgba(248,250,252,.80); font-size: 13px; line-height: 1.35; }

        .about-callout {
          background: rgba(255,255,255,.04);
          border: 1px solid rgba(255,255,255,.10);
          border-radius: 16px;
          padding: 12px 12px;
        }
      ")),
      tags$div(
        class = "about-callout",
        tags$p(
          style = "margin-bottom:10px; color: rgba(248,250,252,.88);",
          "This app is designed so the sentiment model is a clean, swappable backend component. ",
          "The UI stays lightweight and stable while you evolve training, evaluation, and deployment."
        ),
      ),
      tags$div(style = "height: 12px;"),
      tags$div(
        class = "about-grid",
        bullet(
          "collection",
          "What you can do",
          tags$ul(
            style = "margin-bottom:0;",
            tags$li("Browse the Stanford Sentiment Treebank (sentences or phrases)."),
            tags$li("Filter by split/label/score and explore text-length distributions."),
            tags$li("Score any new text using your saved model bundle (the same artifact used by the backend).")
          )
        ),
        bullet(
          "file-earmark-check",
          "Data + labels",
          tags$div(
            "The dataset contains both raw text and sentiment scores. Depending on the task you trained:",
            tags$ul(
              style = "margin:8px 0 0 0;",
              tags$li("sst2: binary labels (neg/pos), neutral examples are dropped during training."),
              tags$li("sst5: 5-class labels (very_neg/neg/neutral/pos/very_pos).")
            )
          )
        )
      ),
      
      tags$div(style = "height: 12px;"),
      
      accordion(
        open = FALSE,
        accordion_panel(
          "Machine learning behind the scenes",
          tags$div(
            class = "about-callout",
            tags$div(style = "font-weight:800; margin-bottom:8px;", "Training workflow"),
            tags$ul(
              style = "margin-bottom:0;",
              tags$li(tags$b("Reality-matching splits:"), " train/dev/test are respected to reduce leakage and keep evaluation honest."),
              tags$li(tags$b("Strong baseline first:"), " a simple majority-class baseline is computed so improvements are measurable."),
              tags$li(tags$b("Guarded feature pipeline:"), " normalization → tokenization → token filtering → optional n-grams → TF-IDF; zero-variance features removed."),
              tags$li(tags$b("Tuned linear model:"), " logistic regression with elastic net (glmnet) is a strong, fast baseline for TF-IDF text features."),
              tags$li(tags$b("Dev-calibrated decision rule:"), " for binary tasks, the threshold is selected on dev (e.g., maximize F1) and then carried forward.")
            )
          ),
          tags$div(style = "height: 12px;"),
          tags$div(
            class = "about-callout",
            tags$div(style = "font-weight:800; margin-bottom:8px;", "Why this is “production style”"),
            tags$ul(
              style = "margin-bottom:0;",
              tags$li(tags$b("Stable contract:"), " inputs are always text; outputs are always label + score in a predictable schema."),
              tags$li(tags$b("Versioned artifact:"), " the trained workflow + metadata + threshold travel together in a single bundle."),
              tags$li(tags$b("Monitoring-friendly outputs:"), " training writes metrics and plots so you can track drift and regressions over time.")
            )
          )
        ),
        accordion_panel(
          "Understanding the score + label",
          tags$div(
            class = "about-callout",
            tags$p(style = "margin-bottom:10px;",
                   "For binary models (sst2), the score is interpretable as the model’s confidence for the positive class."),
            tags$ul(
              style = "margin-bottom:0;",
              tags$li(tags$b("Score near 1:"), " strongly positive sentiment."),
              tags$li(tags$b("Score near 0:"), " strongly negative sentiment."),
              tags$li(tags$b("Score near 0.5:"), " uncertain; these are good candidates for review or additional labeling.")
            )
          )
        ),
        accordion_panel(
          "Next upgrades",
          tags$div(
            class = "about-callout",
            tags$ul(
              style = "margin-bottom:0;",
              tags$li("Add pluggable model sources: built-in bundle vs. a validated local bundle format vs. a Hugging Face model ID."),
              tags$li("Add side-by-side comparison: highlight disagreements and latency differences across model sources."),
              tags$li("Add lightweight monitoring hooks: log score distributions and text-length stats for incoming traffic.")
            )
          )
        )
      )
    )
  )
}
