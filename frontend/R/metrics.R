# metrics.R — updated
# Improvements:
# - Better empty states (avoid “0” before first run)
# - Positive rate detection handles "pos"/"neg" robustly
# - Uncertainty tile shows the actual closest-to-0.5 score + label (more interpretable)
# - Quick insights: friendly placeholders and safer text handling

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(stringr)
  library(tibble)
})

if (!exists("%||%")) {
  `%||%` = function(x, y) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
  }
}

value_box = function(title, value, subtitle = NULL) {
  div(
    class = "value-box",
    div(class = "vb-title", title),
    div(class = "vb-value", value),
    if (!is.null(subtitle)) div(class = "vb-sub", subtitle)
  )
}

metrics_row_ui = function(id) {
  ns = NS(id)
  tagList(
    fluidRow(
      column(3, uiOutput(ns("kpi_model"))),
      column(3, uiOutput(ns("kpi_task"))),
      column(3, uiOutput(ns("kpi_threshold"))),
      column(3, uiOutput(ns("kpi_pos_rate")))
    ),
    fluidRow(
      column(3, uiOutput(ns("kpi_n"))),
      column(3, uiOutput(ns("kpi_avg_score"))),
      column(3, uiOutput(ns("kpi_uncertain"))),
      column(3, uiOutput(ns("kpi_latency")))
    )
  )
}

quick_insights_ui = function(id) {
  ns = NS(id)
  tagList(
    div(class = "insight-row", uiOutput(ns("hi_pos"))),
    div(class = "insight-row", uiOutput(ns("hi_neg"))),
    div(class = "insight-row", uiOutput(ns("uncertain")))
  )
}

metrics_server = function(id, meta_rv, preds_rv, latency_rv) {
  moduleServer(id, function(input, output, session) {
    
    clean_excerpt = function(x, n = 140) {
      x = as.character(x %||% "")
      x = str_squish(x)
      ifelse(nchar(x) > n, paste0(substr(x, 1, n), "…"), x)
    }
    
    has_preds = reactive({
      p = preds_rv()
      !is.null(p) && is.data.frame(p) && nrow(p) > 0
    })
    
    has_scores = reactive({
      p = preds_rv()
      if (is.null(p) || !is.data.frame(p) || nrow(p) == 0) return(FALSE)
      !all(is.na(p$score))
    })
    
    output$kpi_model = renderUI({
      m = meta_rv()
      v = (m$created_utc %||% "—")
      value_box("Model created (UTC)", v)
    })
    
    output$kpi_task = renderUI({
      m = meta_rv()
      v = (m$task %||% "—")
      value_box("Task", v, subtitle = "sst2 (binary) or sst5 (5-class)")
    })
    
    output$kpi_threshold = renderUI({
      m = meta_rv()
      thr = suppressWarnings(as.numeric(m$threshold %||% NA_real_))
      if (!is.finite(thr)) return(value_box("Threshold", "—", "sst2 only"))
      value_box("Threshold", sprintf("%.2f", thr), subtitle = "dev-chosen threshold")
    })
    
    output$kpi_n = renderUI({
      if (!has_preds()) return(value_box("Batch size", "—", "run /predict"))
      p = preds_rv()
      value_box("Batch size", format(nrow(p), big.mark = ","), subtitle = "predictions returned")
    })
    
    output$kpi_avg_score = renderUI({
      if (!has_preds()) return(value_box("Avg score", "—", "sst2 only"))
      p = preds_rv()
      if (!has_scores()) return(value_box("Avg score", "—", "scores unavailable"))
      value_box("Avg score", sprintf("%.3f", mean(p$score, na.rm = TRUE)), subtitle = "mean P(pos) (typical)")
    })
    
    output$kpi_uncertain = renderUI({
      if (!has_preds()) return(value_box("Most uncertain", "—", "sst2 only"))
      p = preds_rv()
      if (!has_scores()) return(value_box("Most uncertain", "—", "sst2 only"))
      
      i = which.min(abs(p$score - 0.5))
      value_box(
        "Most uncertain",
        sprintf("%.3f", as.numeric(p$score[[i]])),
        subtitle = paste0("closest to 0.5 • label=", as.character(p$label[[i]]))
      )
    })
    
    output$kpi_latency = renderUI({
      x = latency_rv()
      if (!is.finite(x)) return(value_box("Latency", "—"))
      value_box("Latency", paste0(sprintf("%.0f", x), " ms"), subtitle = "end-to-end request")
    })
    
    output$kpi_pos_rate = renderUI({
      if (!has_preds()) return(value_box("Positive rate", "—", "sst2 only"))
      p = preds_rv()
      
      labs = unique(as.character(p$label))
      is_binary = all(c("neg", "pos") %in% labs)
      
      if (is_binary) {
        rate = mean(p$label == "pos", na.rm = TRUE) * 100
        value_box("Positive rate", paste0(sprintf("%.1f", rate), "%"), subtitle = "label == pos")
      } else {
        value_box("Positive rate", "—", "not available (multiclass)")
      }
    })
    
    # ---- Quick insights ----
    output$hi_pos = renderUI({
      if (!has_preds() || !has_scores()) {
        return(tags$span(class = "text-muted", "Run /predict to generate insights."))
      }
      
      p = preds_rv()
      i = which.max(p$score)
      div(
        tags$span(class = "insight-k", "High-confidence positive:"),
        tags$span(class = "insight-v",
                  paste0("score=", sprintf("%.3f", p$score[[i]]), " • ", clean_excerpt(p$text[[i]]))
        )
      )
    })
    
    output$hi_neg = renderUI({
      if (!has_preds() || !has_scores()) return(tags$span(class = "text-muted", ""))
      p = preds_rv()
      i = which.min(p$score)
      div(
        tags$span(class = "insight-k", "High-confidence negative:"),
        tags$span(class = "insight-v",
                  paste0("score=", sprintf("%.3f", p$score[[i]]), " • ", clean_excerpt(p$text[[i]]))
        )
      )
    })
    
    output$uncertain = renderUI({
      if (!has_preds() || !has_scores()) return(tags$span(class = "text-muted", ""))
      p = preds_rv()
      i = which.min(abs(p$score - 0.5))
      div(
        tags$span(class = "insight-k", "Most uncertain:"),
        tags$span(class = "insight-v",
                  paste0("score=", sprintf("%.3f", p$score[[i]]), " • ", clean_excerpt(p$text[[i]]))
        )
      )
    })
  })
}
