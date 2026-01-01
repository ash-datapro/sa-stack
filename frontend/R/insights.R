# insights.R — FIXED
# Fixes:
# - Replace plotly_empty()+layout with safe empty plot constructor
# - Guard ggplotly() so it never throws
# - Fix dplyr slice_head(n = min(25, n())) -> slice_head(n = 25)

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(DT)
  library(ggplot2)
  library(plotly)
})

if (!exists("%||%")) {
  `%||%` = function(x, y) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
  }
}

insights_ui = function(id) {
  ns = NS(id)
  
  bslib::card(
    bslib::card_header("Insights"),
    bslib::card_body(
      tags$p(
        class = "text-muted",
        "Lightweight diagnostics computed from the most recent /predict response."
      ),
      
      uiOutput(ns("empty_state")),
      
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        uiOutput(ns("kpi_n")),
        uiOutput(ns("kpi_labels")),
        uiOutput(ns("kpi_avg")),
        uiOutput(ns("kpi_boundary"))
      ),
      
      hr(),
      
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header("Score distribution"),
          bslib::card_body(plotlyOutput(ns("score_plot"), height = "260px"))
        ),
        bslib::card(
          bslib::card_header("Label mix"),
          bslib::card_body(plotlyOutput(ns("label_plot"), height = "260px"))
        )
      ),
      
      hr(),
      
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        bslib::card(
          bslib::card_header("Summary"),
          bslib::card_body(uiOutput(ns("summary_box")))
        ),
        bslib::card(
          bslib::card_header("Most confident"),
          bslib::card_body(uiOutput(ns("most_confident")))
        ),
        bslib::card(
          bslib::card_header("Closest to boundary"),
          bslib::card_body(uiOutput(ns("closest_boundary")))
        )
      ),
      
      hr(),
      
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header("Top labels"),
          bslib::card_body(DTOutput(ns("label_counts_tbl")))
        ),
        bslib::card(
          bslib::card_header("Uncertain items (binary only)"),
          bslib::card_body(DTOutput(ns("uncertain_tbl")))
        )
      )
    )
  )
}

insights_server = function(id, preds_rv) {
  moduleServer(id, function(input, output, session) {
    ns = session$ns
    
    empty_plot = function(msg) {
      plotly::plot_ly(
        type = "scatter",
        mode = "markers",
        x = 0, y = 0,
        marker = list(opacity = 0),
        hoverinfo = "none"
      ) %>%
        plotly::layout(
          annotations = list(list(
            text = msg,
            x = 0.5, y = 0.5,
            xref = "paper", yref = "paper",
            showarrow = FALSE,
            font = list(color = "rgba(248,250,252,0.85)", size = 13)
          )),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          xaxis = list(visible = FALSE, zeroline = FALSE),
          yaxis = list(visible = FALSE, zeroline = FALSE),
          margin = list(l = 10, r = 10, t = 10, b = 10)
        ) %>%
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    }
    
    safe_ggplotly = function(g, ...) {
      p = tryCatch(
        plotly::ggplotly(g, ...),
        error = function(e) empty_plot("Plot unavailable")
      )
      
      p %>%
        plotly::layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)"
        ) %>%
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    }
    
    trunc_text = function(x, n = 220) {
      x = x %||% ""
      x = as.character(x)
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
    
    output$empty_state = renderUI({
      if (has_preds()) return(NULL)
      tags$div(
        class = "callout",
        tags$strong("Run "),
        tags$code("/predict"),
        tags$strong(" to populate insights."),
        tags$span(class = "text-muted", " Charts and tables appear after a successful request.")
      )
    })
    
    kpi_box = function(title, value, sub = NULL) {
      tags$div(
        class = "value-box",
        tags$div(class = "vb-title", title),
        tags$div(class = "vb-value", value),
        if (!is.null(sub)) tags$div(class = "vb-sub", sub)
      )
    }
    
    output$kpi_n = renderUI({
      if (!has_preds()) return(kpi_box("Batch size", "—", "Run /predict"))
      p = preds_rv()
      kpi_box("Batch size", format(nrow(p), big.mark = ","), "predictions returned")
    })
    
    output$kpi_labels = renderUI({
      if (!has_preds()) return(kpi_box("Labels", "—", "Run /predict"))
      p = preds_rv()
      u = sort(unique(as.character(p$label)))
      kpi_box("Labels", length(u), paste(u, collapse = ", "))
    })
    
    output$kpi_avg = renderUI({
      if (!has_preds()) return(kpi_box("Avg score", "—", "sst2 only"))
      if (!has_scores()) return(kpi_box("Avg score", "—", "scores unavailable"))
      p = preds_rv()
      avg = mean(p$score, na.rm = TRUE)
      kpi_box("Avg score", sprintf("%.3f", avg), "higher = more positive (typical)")
    })
    
    output$kpi_boundary = renderUI({
      if (!has_preds()) return(kpi_box("Closest to 0.5", "—", "sst2 only"))
      if (!has_scores()) return(kpi_box("Closest to 0.5", "—", "sst2 only"))
      p = preds_rv()
      d = abs(p$score - 0.5)
      i = which.min(d)
      kpi_box("Closest to 0.5", sprintf("%.3f", p$score[[i]]), paste0("label=", p$label[[i]]))
    })
    
    output$summary_box = renderUI({
      if (!has_preds()) return(div(class = "text-muted", "Run /predict to populate insights."))
      
      p = preds_rv()
      n = nrow(p)
      uniq_lbl = sort(unique(as.character(p$label)))
      score_msg = if (has_scores()) {
        paste0(
          "Avg=", sprintf("%.3f", mean(p$score, na.rm = TRUE)),
          " • Median=", sprintf("%.3f", median(p$score, na.rm = TRUE))
        )
      } else {
        "Scores unavailable for this bundle."
      }
      
      tags$div(
        tags$div(tags$strong("Rows: "), n),
        tags$div(tags$strong("Labels: "), paste(uniq_lbl, collapse = ", ")),
        tags$div(tags$strong("Score: "), score_msg)
      )
    })
    
    output$score_plot = renderPlotly({
      p = preds_rv()
      if (!has_preds()) return(empty_plot("Run /predict to populate"))
      if (!has_scores()) return(empty_plot("Scores unavailable (multiclass bundle)"))
      
      df = p %>%
        mutate(score = as.numeric(score)) %>%
        filter(is.finite(score))
      
      if (nrow(df) == 0) return(empty_plot("Scores unavailable"))
      
      g = ggplot(df, aes(x = score)) +
        geom_histogram(bins = 25) +
        geom_vline(xintercept = 0.5, linetype = "dashed") +
        labs(x = "score", y = "count")
      
      safe_ggplotly(g) %>%
        plotly::layout(margin = list(l = 40, r = 10, t = 10, b = 40))
    })
    
    output$label_plot = renderPlotly({
      p = preds_rv()
      if (!has_preds()) return(empty_plot("Run /predict to populate"))
      
      cts = p %>%
        count(label, sort = TRUE) %>%
        mutate(label = as.character(label))
      
      if (nrow(cts) == 0) return(empty_plot("No labels available"))
      
      g = ggplot(cts, aes(x = reorder(label, n), y = n)) +
        geom_col() +
        coord_flip() +
        labs(x = NULL, y = "count")
      
      safe_ggplotly(g) %>%
        plotly::layout(margin = list(l = 80, r = 10, t = 10, b = 30))
    })
    
    output$most_confident = renderUI({
      if (!has_preds()) return(div(class = "text-muted", "Run /predict to populate."))
      
      p = preds_rv()
      
      if (has_scores()) {
        i = which.max(abs(p$score - 0.5))
        tags$div(
          tags$div(tags$strong("Most confident item")),
          tags$div(class = "text-muted", paste0("score=", sprintf("%.3f", p$score[[i]]), " • label=", p$label[[i]])),
          tags$div(trunc_text(p$text[[i]]))
        )
      } else {
        top_lbl = p %>% count(label, sort = TRUE) %>% slice(1) %>% pull(label)
        i = which(as.character(p$label) == as.character(top_lbl))[1]
        tags$div(
          tags$div(tags$strong("Example from top label")),
          tags$div(class = "text-muted", paste0("label=", top_lbl)),
          tags$div(trunc_text(p$text[[i]]))
        )
      }
    })
    
    output$closest_boundary = renderUI({
      if (!has_preds()) return(div(class = "text-muted", "Run /predict to populate."))
      if (!has_scores()) return(div(class = "text-muted", "Boundary diagnostics are available for binary scoring only (sst2)."))
      
      p = preds_rv()
      i = which.min(abs(p$score - 0.5))
      
      tags$div(
        tags$div(tags$strong("Closest to decision boundary")),
        tags$div(class = "text-muted", paste0("score=", sprintf("%.3f", p$score[[i]]), " • label=", p$label[[i]])),
        tags$div(trunc_text(p$text[[i]]))
      )
    })
    
    output$label_counts_tbl = renderDT({
      p = preds_rv()
      if (!has_preds()) {
        return(DT::datatable(
          data.frame(note = "No predictions yet."),
          rownames = FALSE,
          options = list(dom = "tip")
        ))
      }
      
      cts = p %>%
        count(label, sort = TRUE) %>%
        mutate(pct = n / sum(n)) %>%
        mutate(pct = sprintf("%.1f%%", 100 * pct))
      
      DT::datatable(
        cts,
        rownames = FALSE,
        options = list(pageLength = 8, dom = "tip", scrollX = TRUE),
        class = "compact stripe hover"
      )
    })
    
    output$uncertain_tbl = renderDT({
      p = preds_rv()
      if (!has_preds() || !has_scores()) {
        return(DT::datatable(
          data.frame(note = "Uncertain list available for binary scoring only (sst2)."),
          rownames = FALSE,
          options = list(dom = "tip")
        ))
      }
      
      df = p %>%
        mutate(
          score = as.numeric(score),
          uncertainty = abs(score - 0.5)
        ) %>%
        filter(is.finite(score)) %>%
        arrange(uncertainty) %>%
        slice_head(n = 25) %>%  # <- FIX: constant n
        transmute(
          label = as.character(label),
          score = round(score, 4),
          uncertainty = round(uncertainty, 4),
          text = trunc_text(text, 180)
        )
      
      DT::datatable(
        df,
        rownames = FALSE,
        options = list(pageLength = 8, dom = "tipf", scrollX = TRUE),
        class = "compact stripe hover"
      )
    })
  })
}
