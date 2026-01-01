# diagnostics.R — updated
# Goals:
# - Replace “wall of tables” feel with KPI tiles first, details second
# - Improve empty states and messaging
# - Keep tables, but present them as “Details” panels
# - Keep existing expected artifact names (metrics_dev.csv, metrics_test.csv, threshold_sweep_dev.csv, roc_dev.png, pr_dev.png)

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(readr)
  library(dplyr)
})

diagnostics_ui = function(id) {
  ns = NS(id)
  
  bslib::card(
    bslib::card_header("Diagnostics"),
    bslib::card_body(
      uiOutput(ns("diag_path")),
      
      uiOutput(ns("diag_empty_state")),
      
      # KPI-first row
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        uiOutput(ns("kpi_dev_acc")),
        uiOutput(ns("kpi_dev_f1")),
        uiOutput(ns("kpi_test_acc")),
        uiOutput(ns("kpi_test_f1"))
      ),
      
      hr(),
      
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header("Plots"),
          bslib::card_body(uiOutput(ns("diag_imgs")))
        ),
        bslib::card(
          bslib::card_header("Notes"),
          bslib::card_body(
            tags$ul(
              tags$li(tags$strong("Binary (sst2):"), " you may also see a threshold sweep report."),
              tags$li(tags$strong("Missing files:"), " run the training pipeline to regenerate reports."),
              tags$li(tags$strong("Tip:"), " use the Reports dir line above to confirm your app is pointing at the right folder.")
            )
          )
        )
      ),
      
      hr(),
      
      # Details: collapse tables so the page feels lighter
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "Metrics tables (details)",
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              bslib::card_header("metrics_dev.csv"),
              bslib::card_body(DTOutput(ns("metrics_dev_tbl")))
            ),
            bslib::card(
              bslib::card_header("metrics_test.csv"),
              bslib::card_body(DTOutput(ns("metrics_test_tbl")))
            )
          )
        ),
        bslib::accordion_panel(
          "Threshold sweep (dev) (details)",
          bslib::card(
            bslib::card_header("threshold_sweep_dev.csv"),
            bslib::card_body(DTOutput(ns("threshold_tbl")))
          )
        )
      )
    )
  )
}

diagnostics_server = function(id, reports_dir_rv) {
  moduleServer(id, function(input, output, session) {
    ns = session$ns
    
    read_csv_safe_local = function(path) {
      if (is.null(path) || is.na(path) || !file.exists(path)) return(NULL)
      tryCatch(readr::read_csv(path, show_col_types = FALSE), error = function(e) NULL)
    }
    
    # helper: pull a numeric metric from a 1-row metrics csv
    pull_metric = function(df, key) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NA_real_)
      if (!key %in% names(df)) return(NA_real_)
      suppressWarnings(as.numeric(df[[key]][[1]]))
    }
    
    kpi_box = function(title, value, sub = NULL) {
      tags$div(
        class = "value-box",
        tags$div(class = "vb-title", title),
        tags$div(class = "vb-value", value),
        if (!is.null(sub)) tags$div(class = "vb-sub", sub)
      )
    }
    
    reports_dir_ok = reactive({
      rd = reports_dir_rv()
      !is.null(rd) && !is.na(rd) && dir.exists(rd)
    })
    
    metrics_dev = reactive({
      if (!reports_dir_ok()) return(NULL)
      read_csv_safe_local(file.path(reports_dir_rv(), "metrics_dev.csv"))
    })
    
    metrics_test = reactive({
      if (!reports_dir_ok()) return(NULL)
      read_csv_safe_local(file.path(reports_dir_rv(), "metrics_test.csv"))
    })
    
    threshold_dev = reactive({
      if (!reports_dir_ok()) return(NULL)
      read_csv_safe_local(file.path(reports_dir_rv(), "threshold_sweep_dev.csv"))
    })
    
    output$diag_path = renderUI({
      rd = reports_dir_rv()
      if (!reports_dir_ok()) {
        return(div(class = "text-muted",
                   "Could not find reports directory. Set SENTIMENT_REPORTS_DIR or ensure reports/ exists."
        ))
      }
      div(class = "text-muted", paste0("Reports dir: ", rd))
    })
    
    output$diag_empty_state = renderUI({
      if (!reports_dir_ok()) return(NULL)
      
      rd = reports_dir_rv()
      has_any =
        file.exists(file.path(rd, "metrics_dev.csv")) ||
        file.exists(file.path(rd, "metrics_test.csv")) ||
        file.exists(file.path(rd, "threshold_sweep_dev.csv")) ||
        file.exists(file.path(rd, "roc_dev.png")) ||
        file.exists(file.path(rd, "pr_dev.png"))
      
      if (isTRUE(has_any)) return(NULL)
      
      tags$div(
        class = "callout",
        tags$strong("No reports found. "),
        "Expected files in the reports directory (CSV and/or PNG). Run the training pipeline to generate them."
      )
    })
    
    # KPI tiles
    output$kpi_dev_acc = renderUI({
      m = metrics_dev()
      if (is.null(m)) return(kpi_box("Dev accuracy", "—", "metrics_dev.csv"))
      x = pull_metric(m, "accuracy")
      kpi_box("Dev accuracy", if (is.finite(x)) sprintf("%.3f", x) else "—", "metrics_dev.csv")
    })
    
    output$kpi_dev_f1 = renderUI({
      m = metrics_dev()
      if (is.null(m)) return(kpi_box("Dev F1", "—", "metrics_dev.csv"))
      x = pull_metric(m, "f_meas")
      kpi_box("Dev F1", if (is.finite(x)) sprintf("%.3f", x) else "—", "metrics_dev.csv")
    })
    
    output$kpi_test_acc = renderUI({
      m = metrics_test()
      if (is.null(m)) return(kpi_box("Test accuracy", "—", "metrics_test.csv"))
      x = pull_metric(m, "accuracy")
      kpi_box("Test accuracy", if (is.finite(x)) sprintf("%.3f", x) else "—", "metrics_test.csv")
    })
    
    output$kpi_test_f1 = renderUI({
      m = metrics_test()
      if (is.null(m)) return(kpi_box("Test F1", "—", "metrics_test.csv"))
      x = pull_metric(m, "f_meas")
      kpi_box("Test F1", if (is.finite(x)) sprintf("%.3f", x) else "—", "metrics_test.csv")
    })
    
    # Tables
    output$metrics_dev_tbl = renderDT({
      m = metrics_dev()
      if (is.null(m)) return(DT::datatable(data.frame(note = "metrics_dev.csv not found."), rownames = FALSE, options = list(dom = "tip")))
      DT::datatable(m, rownames = FALSE, options = list(pageLength = 10, dom = "tip", scrollX = TRUE), class = "compact stripe hover")
    })
    
    output$metrics_test_tbl = renderDT({
      m = metrics_test()
      if (is.null(m)) return(DT::datatable(data.frame(note = "metrics_test.csv not found."), rownames = FALSE, options = list(dom = "tip")))
      DT::datatable(m, rownames = FALSE, options = list(pageLength = 10, dom = "tip", scrollX = TRUE), class = "compact stripe hover")
    })
    
    output$threshold_tbl = renderDT({
      tdf = threshold_dev()
      if (is.null(tdf)) return(DT::datatable(data.frame(note = "threshold_sweep_dev.csv not found (sst2 only)."), rownames = FALSE, options = list(dom = "tip")))
      DT::datatable(tdf, rownames = FALSE, options = list(pageLength = 10, dom = "tip", scrollX = TRUE), class = "compact stripe hover")
    })
    
    # Plots
    output$diag_imgs = renderUI({
      if (!reports_dir_ok()) return(div(class = "text-muted", "No reports directory configured."))
      
      rd = reports_dir_rv()
      imgs = c("roc_dev.png", "pr_dev.png")
      present = imgs[file.exists(file.path(rd, imgs))]
      
      if (length(present) == 0) {
        return(div(class = "text-muted", "No PNG plots found (expected roc_dev.png / pr_dev.png)."))
      }
      
      tagList(lapply(present, function(f) {
        tags$div(
          style = "margin: 12px 0;",
          tags$div(class = "text-muted", style = "margin-bottom:6px;", f),
          tags$img(src = paste0("reports/", f), class = "diag-img")
        )
      }))
    })
  })
}
