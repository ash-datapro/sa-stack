# prediction.R — FIXED
# Fixes:
# - Replace plotly_empty()+layout with a safe empty plotly constructor
# - Guard ggplotly() with tryCatch so it never crashes the app
# - Keep same exported functions: prediction_panel_ui(), prediction_server()

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tibble)
  library(stringr)
  library(DT)
  library(ggplot2)
  library(plotly)
})

if (!exists("%||%")) {
  `%||%` = function(x, y) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
  }
}

prediction_panel_ui = function(id) {
  ns = NS(id)
  
  bslib::layout_columns(
    col_widths = c(6, 6),
    
    bslib::card(
      height = "420px",
      full_screen = TRUE,
      bslib::card_header("Scores"),
      bslib::card_body(
        uiOutput(ns("scores_empty_ui")),
        plotlyOutput(ns("score_plot"), height = "340px")
      )
    ),
    
    bslib::card(
      height = "420px",
      full_screen = TRUE,
      bslib::card_header("Predictions"),
      bslib::card_body(
        uiOutput(ns("table_empty_ui")),
        DTOutput(ns("pred_table"))
      )
    )
  )
}

prediction_server = function(id, preds_rv, meta_rv = NULL) {
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
    
    
    has_preds = reactive({
      p = preds_rv()
      !is.null(p) && is.data.frame(p) && nrow(p) > 0
    })
    
    has_scores = reactive({
      p = preds_rv()
      if (is.null(p) || !is.data.frame(p) || nrow(p) == 0) return(FALSE)
      !all(is.na(p$score))
    })
    
    get_threshold = function() {
      if (is.null(meta_rv)) return(NA_real_)
      m = meta_rv()
      if (is.null(m)) return(NA_real_)
      suppressWarnings(as.numeric(m$threshold %||% NA_real_))
    }
    
    output$scores_empty_ui = renderUI({
      if (has_preds()) return(NULL)
      div(class = "callout",
          tags$strong("Run "),
          tags$code("/predict"),
          tags$strong(" to see scores."),
          tags$span(class = "text-muted", " The chart updates from the latest response.")
      )
    })
    
    output$table_empty_ui = renderUI({
      if (has_preds()) return(NULL)
      tags$div(class = "text-muted", style = "margin: 8px 0 10px 0;", "No predictions yet.")
    })
    
    output$score_plot = renderPlotly({
      p = preds_rv()
      
      if (!has_preds()) return(empty_plot("Run /predict to populate"))
      
      # Multiclass / no numeric scores: show label counts
      if (!has_scores()) {
        cts = p %>%
          mutate(label = as.character(label)) %>%
          count(label, sort = TRUE)
        
        if (nrow(cts) == 0) return(empty_plot("No labels available"))
        
        g = ggplot(cts, aes(x = reorder(label, n), y = n)) +
          geom_col() +
          coord_flip() +
          labs(title = "Predicted label counts", x = NULL, y = "count")
        
        return(
          safe_ggplotly(g) %>%
            plotly::layout(margin = list(l = 90, r = 10, t = 30, b = 30))
        )
      }
      
      # Binary scoring: histogram with optional threshold line
      df = p %>%
        mutate(score = suppressWarnings(as.numeric(score))) %>%
        filter(is.finite(score))
      
      if (nrow(df) == 0) return(empty_plot("Scores unavailable"))
      
      thr = get_threshold()
      
      g = ggplot(df, aes(x = score)) +
        geom_histogram(bins = 25) +
        geom_vline(xintercept = 0.5, linetype = "dashed") +
        labs(title = "Score distribution (P(pos))", x = "score", y = "count")
      
      if (is.finite(thr)) g = g + geom_vline(xintercept = thr, linetype = "solid")
      
      safe_ggplotly(g) %>%
        plotly::layout(margin = list(l = 40, r = 10, t = 30, b = 40))
    })
    
    output$pred_table = renderDT({
      p = preds_rv()
      
      if (!has_preds()) {
        return(DT::datatable(
          data.frame(note = "No predictions yet."),
          rownames = FALSE,
          options = list(dom = "tip")
        ))
      }
      
      out = p %>%
        mutate(
          row = dplyr::row_number(),
          score = ifelse(is.na(score), NA_real_, round(as.numeric(score), 4)),
          text = {
            tt = as.character(text %||% "")
            tt = stringr::str_squish(tt)
            ifelse(nchar(tt) > 260, paste0(substr(tt, 1, 260), "…"), tt)
          }
        ) %>%
        select(row, label, score, text)
      
      DT::datatable(
        out,
        rownames = FALSE,
        options = list(pageLength = 10, dom = "tipf", scrollX = TRUE),
        class = "compact stripe hover"
      )
    })
  })
}
