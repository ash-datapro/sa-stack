suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(stringr)
  library(plotly)
  library(DT)
  library(readr)
  library(bsicons)
  library(tibble)
  library(textrecipes)
  
  # Needed so predict() works for tidymodels workflow inside model.rds
  library(workflows)
  library(parsnip)
  library(recipes)
})

# Load About tab content
if (file.exists("R/about.R")) source("R/about.R", local = TRUE)


# ----------------------------
# Config (portable defaults)
# ----------------------------
# Optional env vars:
#   SENTIMENT_DATA_RDS  (path to sst_treebank.rds)
#   SENTIMENT_MODEL_RDS (path to model.rds)
DEFAULT_DATA_RDS = Sys.getenv("SENTIMENT_DATA_RDS", unset = NA_character_)
DEFAULT_MODEL_RDS = Sys.getenv("SENTIMENT_MODEL_RDS", unset = NA_character_)

# ----------------------------
# Helpers
# ----------------------------
`%||%` = function(a, b) if (!is.null(a) && length(a) > 0) a else b

fmt_num = function(x, digits = 2) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_character_)
  formatC(x, format = "f", digits = digits)
}

resolve_first_existing = function(paths) {
  paths = paths[!is.na(paths) & nzchar(paths)]
  for (p in paths) {
    if (file.exists(p)) return(normalizePath(p, mustWork = FALSE))
  }
  NA_character_
}

default_data_path = function() {
  resolve_first_existing(c(
    DEFAULT_DATA_RDS,
    "data/sst_treebank.rds",
    "../data/sst_treebank.rds",
    "../../data/sst_treebank.rds",
    "sst_treebank.rds"
  ))
}

default_model_path = function() {
  resolve_first_existing(c(
    DEFAULT_MODEL_RDS,
    "backend/api/model.rds",
    "../backend/api/model.rds",
    "../../backend/api/model.rds",
    "api/model.rds",
    "model.rds"
  ))
}

safe_read_any = function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) {
    return(list(obj = NULL, err = paste0("File not found: ", path), class = NA_character_))
  }
  
  ext = tolower(tools::file_ext(path))
  obj = NULL
  err = NULL
  
  if (ext == "rds") {
    obj = tryCatch(readRDS(path), error = function(e) { err <<- conditionMessage(e); NULL })
  } else if (ext == "csv") {
    obj = tryCatch(readr::read_csv(path, show_col_types = FALSE), error = function(e) { err <<- conditionMessage(e); NULL })
  } else {
    return(list(obj = NULL, err = paste0("Unsupported file type: .", ext, " (use .rds or .csv)"), class = NA_character_))
  }
  
  if (is.null(obj)) return(list(obj = NULL, err = paste0("Load failed: ", err), class = NA_character_))
  list(obj = obj, err = NULL, class = paste(class(obj), collapse = ", "))
}

make_features = function(df, text_col) {
  d = df
  txt = as.character(d[[text_col]])
  d$feat_char_len = nchar(txt)
  d$feat_word_count = str_count(txt, "\\S+")
  d
}

make_binary = function(score) {
  factor(ifelse(is.na(score), NA, ifelse(score > 0.5, "pos", "neg")), levels = c("neg", "pos"))
}

make_5bin = function(score) {
  cut(
    score,
    breaks = c(-Inf, 0.2, 0.4, 0.6, 0.8, Inf),
    labels = c("very_neg", "neg", "neutral", "pos", "very_pos"),
    ordered_result = TRUE
  )
}

plotly_blank = function(msg = "No data.") {
  plot_ly(x = 0, y = 0, type = "scatter", mode = "markers", hoverinfo = "none") %>%
    layout(
      annotations = list(list(
        text = msg,
        x = 0.5, y = 0.5, xref = "paper", yref = "paper",
        showarrow = FALSE
      )),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      margin = list(l = 10, r = 10, t = 10, b = 10)
    )
}

plotly_hist = function(x, x_title) {
  plot_ly(x = x, type = "histogram") %>%
    layout(
      xaxis = list(title = x_title),
      yaxis = list(title = "Count"),
      margin = list(l = 50, r = 10, t = 10, b = 45)
    )
}

plotly_bar = function(x, y, x_title = "", y_title = "Count") {
  plot_ly(x = x, y = y, type = "bar") %>%
    layout(
      xaxis = list(title = x_title),
      yaxis = list(title = y_title),
      margin = list(l = 50, r = 10, t = 10, b = 70)
    )
}

plotly_scatter = function(x, y, color = NULL, x_title, y_title) {
  if (is.null(color)) {
    plot_ly(x = x, y = y, type = "scatter", mode = "markers") %>%
      layout(
        xaxis = list(title = x_title),
        yaxis = list(title = y_title),
        margin = list(l = 50, r = 10, t = 10, b = 45)
      )
  } else {
    plot_ly(x = x, y = y, color = color, type = "scatter", mode = "markers") %>%
      layout(
        xaxis = list(title = x_title),
        yaxis = list(title = y_title),
        margin = list(l = 50, r = 10, t = 10, b = 45)
      )
  }
}

kpi_card = function(title, value, icon_name = "bar-chart") {
  card(
    class = "kpi-card",
    card_body(
      tags$div(class = "kpi-top",
               tags$div(class = "kpi-icon", bs_icon(icon_name)),
               tags$div(class = "kpi-title", title)
      ),
      tags$div(class = "kpi-value", value)
    )
  )
}

# Detect SST structure
is_sst_treebank = function(obj) {
  is.list(obj) &&
    all(c("datasetSentences", "datasetSplit", "dictionary", "sentiment_labels") %in% names(obj))
}

build_sentence_df = function(sst) {
  ds = sst$datasetSentences
  sp = sst$datasetSplit
  dict = sst$dictionary
  lab = sst$sentiment_labels
  
  # Normalize names
  ds = ds %>% rename(text = sentence)
  dict2 = dict %>% transmute(phrase_id = phrase_id, text = as.character(phrase))
  lab2 = lab %>% transmute(phrase_id = phrase_id, score = as.numeric(sentiment))
  
  # Phrase -> score lookup
  dict_sc = dict2 %>% left_join(lab2, by = "phrase_id")
  
  out = ds %>%
    left_join(sp, by = "sentence_index") %>%
    mutate(
      split = case_when(
        splitset_label == 1 ~ "train",
        splitset_label == 2 ~ "test",
        splitset_label == 3 ~ "dev",
        TRUE ~ "unknown"
      )
    ) %>%
    left_join(dict_sc %>% select(text, score), by = "text")
  
  out$label_bin = make_binary(out$score)
  out$label_5 = make_5bin(out$score)
  out
}

build_phrase_df = function(sst) {
  dict = sst$dictionary
  lab = sst$sentiment_labels
  
  out = dict %>%
    left_join(lab, by = "phrase_id") %>%
    transmute(
      phrase_id = phrase_id,
      text = as.character(phrase),
      score = as.numeric(sentiment)
    )
  
  out$label_bin = make_binary(out$score)
  out$label_5 = make_5bin(out$score)
  out
}

# ----------------------------
# Theme
# ----------------------------
theme_blue = bs_theme(
  version = 5,
  bg = "#071a2f",
  fg = "#f8fafc",
  primary = "#3b82f6",
  secondary = "#60a5fa",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

# ----------------------------
# UI
# ----------------------------
ui = page_navbar(
  title = "Sentiment Explorer",
  theme = theme_blue,
  
  header = tagList(
    tags$style(HTML("
      .app-subtitle { color: rgba(248,250,252,.70); margin-top: 4px; margin-bottom: 12px; }
      .card { border-radius: 18px !important; background: rgba(255,255,255,.04) !important; border: 1px solid rgba(255,255,255,.10) !important; }
      .card-header { background: transparent !important; border-bottom: 1px solid rgba(255,255,255,.08) !important; }
      .sidebar-panel {
        background: rgba(255,255,255,.04);
        border: 1px solid rgba(255,255,255,.10);
        border-radius: 18px;
        padding: 14px 14px;
      }
      .small-muted { color: rgba(248,250,252,.65); font-size: 12px; }
      .section-title { font-weight: 800; letter-spacing: -.02em; margin-bottom: 8px; }
      .soft-divider { border-color: rgba(255,255,255,.10); }
      .btn { border-radius: 12px !important; }
      .nav-link.active { color: #ffffff !important; font-weight: 800 !important; }
      .plotly { border-radius: 14px; overflow: hidden; }
      table.dataTable { color: rgba(248,250,252,.90) !important; }

      .callout {
        background: rgba(255,255,255,.04);
        border: 1px solid rgba(255,255,255,.10);
        border-radius: 14px;
        padding: 10px 12px;
        color: rgba(248,250,252,.92);
      }

      /* KPI styling */
      .kpi-card .card-body { padding: 16px 16px; }
      .kpi-top { display: flex; align-items: center; gap: 10px; margin-bottom: 10px; }
      .kpi-icon svg { width: 28px; height: 28px; color: rgba(59,130,246,.95); }
      .kpi-title { font-size: 14px; color: rgba(248,250,252,.80); font-weight: 600; }
      .kpi-value { font-size: 34px; font-weight: 800; letter-spacing: -.02em; color: rgba(248,250,252,.98); }
    ")),
    div(class = "app-subtitle", "")
  ),
  
  nav_panel(
    "Overview",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        div(
          class = "sidebar-panel",
          h5(class = "section-title", "Filter options"),
          div(class = "small-muted", "Choose analysis unit + filters."),
          hr(class = "soft-divider"),
          radioButtons("unit", "Unit", choices = c("Sentences" = "sent", "Phrases" = "phrase"), selected = "sent"),
          uiOutput("sidebar_filters_ui"),
          hr(class = "soft-divider"),
          actionButton("reset_filters", "Reset filters", class = "btn btn-outline-light"),
          tags$div(style = "height:10px;"),
          downloadButton("download_filtered", "Download filtered CSV", class = "btn btn-primary w-100")
        )
      ),
      
      div(
        class = "p-1",
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          uiOutput("kpi_1"),
          uiOutput("kpi_2"),
          uiOutput("kpi_3"),
          uiOutput("kpi_4")
        ),
        tags$div(style = "height: 12px;"),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Label distribution"), card_body(plotlyOutput("plot_label_dist", height = 320))),
          card(card_header("Sentiment score distribution"), card_body(plotlyOutput("plot_score_dist", height = 320)))
        ),
        tags$div(style = "height: 12px;"),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Text length (words)"), card_body(plotlyOutput("plot_len_words", height = 320))),
          card(card_header("Text length (characters)"), card_body(plotlyOutput("plot_len_chars", height = 320)))
        )
      )
    )
  ),
  
  nav_panel(
    "Exploration",
    layout_sidebar(
      sidebar = sidebar(
        width = 360,
        div(
          class = "sidebar-panel",
          h5(class = "section-title", "Upload data"),
          div(class = "small-muted", "Using the default dataset."),
          tags$div(style = "height: 6px;"),
          hr(class = "soft-divider"),
          h5(class = "section-title", "Quick checks"),
          uiOutput("explore_quick_checks_ui"),
          hr(class = "soft-divider"),
          h5(class = "section-title", "Preview"),
          div(class = "small-muted", "First 50 rows of the filtered dataset.")
        )
      ),
      
      div(
        class = "p-1",
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Interactive scatter: score vs length"),
               card_body(plotlyOutput("plot_scatter_score_len", height = 360))),
          card(card_header("Split breakdown (sentences only)"),
               card_body(plotlyOutput("plot_split_breakdown", height = 360)))
        ),
        tags$div(style = "height: 12px;"),
        card(card_header("Data table"), card_body(DTOutput("data_table")))
      )
    )
  ),
  
  nav_panel(
    "Scoring",
    layout_sidebar(
      sidebar = sidebar(
        width = 360,
        div(
          class = "sidebar-panel",
          h5(class = "section-title", "Model bundle"),
          div(class = "small-muted", "Load a saved model bundle (.rds) and score your text."),
          hr(class = "soft-divider"),
          fileInput("upload_model", "Upload model bundle (.rds)", accept = c(".rds")),
          actionButton("use_default_model", "Use default local model", class = "btn btn-outline-light w-100"),
          hr(class = "soft-divider"),
          uiOutput("model_status_ui"),
          hr(class = "soft-divider"),
          h5(class = "section-title", "Score a text"),
          textAreaInput("pred_text", "Text", placeholder = "Type a sentence...", rows = 4),
          actionButton("run_pred", "Score text", class = "btn btn-primary w-100")
        )
      ),
      
      div(
        class = "p-1",
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Result"), card_body(uiOutput("pred_out_ui"))),
          card(card_header("Raw output (debug)"), card_body(verbatimTextOutput("pred_raw_txt")))
        )
      )
    )
  ),
  
  nav_panel(
    "About",
    about_ui(
      default_data_path = DEFAULT_DATA_RDS,
      default_model_path = DEFAULT_MODEL_RDS
    )
  )
  
)

# ----------------------------
# Server
# ----------------------------
server = function(input, output, session) {
  
  # ----------------------------
  # Data loading
  # ----------------------------
  load_info = reactiveVal(list(obj = NULL, err = "No data loaded yet.", class = NA_character_))
  
  load_from_path = function(path) {
    res = safe_read_any(path)
    load_info(res)
  }
  
  observe({
    p = default_data_path()
    if (!is.na(p)) load_from_path(p)
  })
  
  observeEvent(input$use_default_data, {
    p = default_data_path()
    if (is.na(p)) {
      load_info(list(obj = NULL, err = "Default dataset not found (set SENTIMENT_DATA_RDS or place data/sst_treebank.rds).", class = NA_character_))
    } else {
      load_from_path(p)
    }
  }, ignoreInit = TRUE)
  
  working_df = reactive({
    obj = load_info()$obj
    if (is.null(obj)) return(NULL)
    
    # CSV / plain df
    if (is.data.frame(obj)) {
      d = obj
      # normalize text column
      text_col = if ("sentence" %in% names(d)) "sentence" else if ("text" %in% names(d)) "text" else NA_character_
      if (!is.na(text_col)) d = make_features(d, text_col)
      
      # if score exists, derive labels
      if ("score" %in% names(d)) {
        d$score = suppressWarnings(as.numeric(d$score))
        d$label_bin = make_binary(d$score)
        d$label_5 = make_5bin(d$score)
      }
      return(d)
    }
    
    # SST list
    if (is_sst_treebank(obj)) {
      if (input$unit == "sent") {
        d = build_sentence_df(obj)
        d = make_features(d, "text")
        return(d)
      } else {
        d = build_phrase_df(obj)
        d = make_features(d, "text")
        return(d)
      }
    }
    
    NULL
  })
  
  # ----------------------------
  # Filters
  # ----------------------------
  output$sidebar_filters_ui = renderUI({
    d = working_df()
    if (is.null(d)) return(div(class = "small-muted", "No data loaded."))
    
    tagList(
      if ("split" %in% names(d)) {
        selectInput("f_split", "Split", choices = c("All" = "__all__", sort(unique(d$split))), selected = "__all__")
      },
      if ("label_bin" %in% names(d)) {
        selectInput("f_label", "Label (binary)", choices = c("All" = "__all__", levels(d$label_bin)), selected = "__all__")
      },
      if ("label_5" %in% names(d)) {
        selectInput("f_label5", "Label (5-bin)", choices = c("All" = "__all__", levels(d$label_5)), selected = "__all__")
      },
      if ("score" %in% names(d) && is.numeric(d$score)) {
        sliderInput("f_score", "Score range", min = 0, max = 1, value = c(0, 1), step = 0.01)
      },
      if ("feat_word_count" %in% names(d)) {
        sliderInput(
          "f_words", "Word count",
          min = 0,
          max = max(d$feat_word_count, na.rm = TRUE),
          value = c(0, max(d$feat_word_count, na.rm = TRUE))
        )
      }
    )
  })
  
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "f_split", selected = "__all__")
    updateSelectInput(session, "f_label", selected = "__all__")
    updateSelectInput(session, "f_label5", selected = "__all__")
    updateSliderInput(session, "f_score", value = c(0, 1))
    if (!is.null(working_df()) && "feat_word_count" %in% names(working_df())) {
      updateSliderInput(session, "f_words", value = c(0, max(working_df()$feat_word_count, na.rm = TRUE)))
    }
  }, ignoreInit = TRUE)
  
  filtered_df = reactive({
    d = working_df()
    if (is.null(d)) return(NULL)
    out = d
    
    if ("split" %in% names(out) && !is.null(input$f_split) && input$f_split != "__all__") {
      out = out %>% filter(split == input$f_split)
    }
    
    if (!is.null(input$f_label) && input$f_label != "__all__" && "label_bin" %in% names(out)) {
      out = out %>% filter(label_bin == input$f_label)
    }
    
    if (!is.null(input$f_label5) && input$f_label5 != "__all__" && "label_5" %in% names(out)) {
      out = out %>% filter(label_5 == input$f_label5)
    }
    
    if ("score" %in% names(out) && !is.null(input$f_score)) {
      out = out %>% filter(is.na(score) | (score >= input$f_score[1] & score <= input$f_score[2]))
    }
    
    if ("feat_word_count" %in% names(out) && !is.null(input$f_words)) {
      out = out %>% filter(is.na(feat_word_count) | (feat_word_count >= input$f_words[1] & feat_word_count <= input$f_words[2]))
    }
    
    out
  })
  
  output$download_filtered = downloadHandler(
    filename = function() paste0("sentiment_filtered_", input$unit, "_", Sys.Date(), ".csv"),
    content = function(file) {
      d = filtered_df()
      if (is.null(d)) writeLines("No data.", con = file) else readr::write_csv(d, file)
    }
  )
  
  # ----------------------------
  # Quick checks
  # ----------------------------
  output$explore_quick_checks_ui = renderUI({
    info = load_info()
    obj = info$obj
    
    if (is.null(obj)) {
      return(div(class = "callout",
                 tags$b("Status: "), "No data loaded.", tags$br(),
                 tags$span(class = "small-muted", info$err %||% "")
      ))
    }
    
    if (is_sst_treebank(obj)) {
      return(tagList(
        div(class = "callout",
            tags$b("Detected: "), "SST Treebank object (list)",
            tags$br(),
            tags$span(class = "small-muted", "Sentences and phrases will be assigned scores (0–1).")
        ),
        div(class = "callout mt-2",
            tags$b("Current unit: "), if (input$unit == "sent") "Sentences" else "Phrases"
        )
      ))
    }
    
    if (is.data.frame(obj)) {
      div(class = "callout", tags$b("Loaded data.frame: "), nrow(obj), " rows × ", ncol(obj), " cols")
    } else {
      div(class = "callout", tags$b("Loaded object class: "), info$class %||% "—")
    }
  })
  
  # ----------------------------
  # KPI cards
  # ----------------------------
  output$kpi_1 = renderUI({
    d = filtered_df()
    if (is.null(d)) return(kpi_card("Rows", "—", "database"))
    kpi_card("Rows", format(nrow(d), big.mark = ","), "database")
  })
  
  output$kpi_2 = renderUI({
    d = filtered_df()
    if (is.null(d)) return(kpi_card("Label types", "—", "tags"))
    if ("label_5" %in% names(d)) return(kpi_card("Label types", length(levels(d$label_5)), "tags"))
    if ("label_bin" %in% names(d)) return(kpi_card("Label types", length(levels(d$label_bin)), "tags"))
    kpi_card("Label types", "—", "tags")
  })
  
  output$kpi_3 = renderUI({
    d = filtered_df()
    if (is.null(d) || !"feat_word_count" %in% names(d)) return(kpi_card("Avg words", "—", "chat-text"))
    kpi_card("Avg words", fmt_num(mean(d$feat_word_count, na.rm = TRUE), 1), "chat-text")
  })
  
  output$kpi_4 = renderUI({
    d = filtered_df()
    if (is.null(d) || !"score" %in% names(d)) return(kpi_card("Avg sentiment", "—", "graph-up"))
    kpi_card("Avg sentiment", fmt_num(mean(d$score, na.rm = TRUE), 3), "graph-up")
  })
  
  # ----------------------------
  # Plots
  # ----------------------------
  output$plot_label_dist = renderPlotly({
    d = filtered_df()
    if (is.null(d)) return(plotly_blank("Load data to see charts."))
    
    if ("label_5" %in% names(d)) {
      tab = d %>% count(label_5, sort = TRUE)
      return(plotly_bar(tab$label_5, tab$n, x_title = "Label", y_title = "Count"))
    }
    if ("label_bin" %in% names(d)) {
      tab = d %>% count(label_bin, sort = TRUE)
      return(plotly_bar(tab$label_bin, tab$n, x_title = "Label", y_title = "Count"))
    }
    plotly_blank("No labels available for this dataset.")
  })
  
  output$plot_score_dist = renderPlotly({
    d = filtered_df()
    if (is.null(d)) return(plotly_blank("Load data to see charts."))
    if (!"score" %in% names(d)) return(plotly_blank("No score column available."))
    
    dd = d %>% filter(!is.na(score))
    if (nrow(dd) < 2) return(plotly_blank("Not enough scored rows."))
    plotly_hist(dd$score, "Score (0–1)")
  })
  
  output$plot_len_words = renderPlotly({
    d = filtered_df()
    if (is.null(d) || !"feat_word_count" %in% names(d)) return(plotly_blank("No data."))
    dd = d %>% filter(!is.na(feat_word_count))
    if (nrow(dd) < 2) return(plotly_blank("Not enough rows."))
    plotly_hist(dd$feat_word_count, "Words")
  })
  
  output$plot_len_chars = renderPlotly({
    d = filtered_df()
    if (is.null(d) || !"feat_char_len" %in% names(d)) return(plotly_blank("No data."))
    dd = d %>% filter(!is.na(feat_char_len))
    if (nrow(dd) < 2) return(plotly_blank("Not enough rows."))
    plotly_hist(dd$feat_char_len, "Characters")
  })
  
  output$plot_scatter_score_len = renderPlotly({
    d = filtered_df()
    if (is.null(d)) return(plotly_blank("Load data to see charts."))
    if (!all(c("score", "feat_word_count") %in% names(d))) return(plotly_blank("Score/length not available."))
    
    dd = d %>% filter(!is.na(score), !is.na(feat_word_count))
    if (nrow(dd) < 3) return(plotly_blank("Not enough rows for scatter."))
    
    if (nrow(dd) > 20000) dd = dd[sample.int(nrow(dd), 20000), , drop = FALSE]
    
    if ("label_bin" %in% names(dd)) {
      plotly_scatter(dd$feat_word_count, dd$score, color = dd$label_bin, x_title = "Word count", y_title = "Score")
    } else {
      plotly_scatter(dd$feat_word_count, dd$score, x_title = "Word count", y_title = "Score")
    }
  })
  
  output$plot_split_breakdown = renderPlotly({
    d = filtered_df()
    if (is.null(d) || !"split" %in% names(d)) return(plotly_blank("Split info is available for sentence unit only."))
    tab = d %>% count(split, sort = TRUE)
    plotly_bar(tab$split, tab$n, x_title = "Split", y_title = "Count")
  })
  
  output$data_table = renderDT({
    d = filtered_df()
    if (is.null(d)) return(datatable(data.frame(note = "No data loaded."), rownames = FALSE))
    datatable(head(d, 50), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ----------------------------
  # Model bundle loading + scoring
  # ----------------------------
  model_info = reactiveVal(list(obj = NULL, err = "No model loaded."))
  
  load_model_from_path = function(path) {
    if (is.null(path) || is.na(path) || !file.exists(path)) {
      model_info(list(obj = NULL, err = paste0("Model file not found: ", path)))
      return()
    }
    
    m = tryCatch(readRDS(path), error = function(e) e)
    if (inherits(m, "error")) {
      model_info(list(obj = NULL, err = paste0("Failed to read model RDS: ", conditionMessage(m))))
      return()
    }
    
    if (!is.list(m) || is.null(m$predict) || !is.function(m$predict)) {
      model_info(list(obj = NULL, err = "Loaded object is not a valid model bundle (missing $predict function)."))
      return()
    }
    
    lv = m$contract$output_schema$label_levels %||% NULL
    if (is.null(lv)) {
      model_info(list(obj = NULL, err = "Model bundle missing contract$output_schema$label_levels."))
      return()
    }
    
    model_info(list(obj = m, err = NULL))
  }
  
  observe({
    mp = default_model_path()
    if (!is.na(mp)) load_model_from_path(mp)
  })
  
  observeEvent(input$use_default_model, {
    mp = default_model_path()
    if (is.na(mp)) {
      model_info(list(obj = NULL, err = "Default model not found (set SENTIMENT_MODEL_RDS or place backend/api/model.rds)."))
    } else {
      load_model_from_path(mp)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$upload_model, {
    req(input$upload_model$datapath)
    load_model_from_path(input$upload_model$datapath)
  })
  
  output$model_status_ui = renderUI({
    mi = model_info()
    if (!is.null(mi$err)) {
      div(class = "callout", tags$b("Model: "), mi$err)
    } else {
      m = mi$obj
      lv = m$contract$output_schema$label_levels
      thr = suppressWarnings(as.numeric(m$threshold %||% NA_real_))
      n_rows = m$data_source$n_rows %||% NA_integer_
      div(class = "callout",
          tags$b("Model loaded."), tags$br(),
          tags$span(class = "small-muted", paste0("Task: ", m$task %||% "—")), tags$br(),
          tags$span(class = "small-muted", paste0("Levels: ", paste(lv, collapse = ", "))), tags$br(),
          tags$span(class = "small-muted", paste0("Threshold: ", ifelse(is.na(thr), "—", fmt_num(thr, 2)))), tags$br(),
          tags$span(class = "small-muted", paste0("Training rows: ", ifelse(is.na(n_rows), "—", format(n_rows, big.mark = ","))))
      )
    }
  })
  
  pred_result = reactiveVal(NULL)
  
  observeEvent(input$run_pred, {
    mi = model_info()
    if (!is.null(mi$err) || is.null(mi$obj)) {
      pred_result(list(err = "Load a model bundle first.", out = NULL))
      return()
    }
    
    txt = input$pred_text %||% ""
    txt = trimws(txt)
    if (!nzchar(txt)) {
      pred_result(list(err = "Enter some text to score.", out = NULL))
      return()
    }
    
    m = mi$obj
    
    raw = tryCatch(m$predict(m, txt), error = function(e) e)
    if (inherits(raw, "error")) {
      pred_result(list(err = paste0("Scoring failed: ", conditionMessage(raw)), out = NULL))
      return()
    }
    
    if (!is.data.frame(raw) || !all(c("text", "label", "score") %in% names(raw))) {
      pred_result(list(
        err = "Model returned an unexpected format (expected columns: text, label, score).",
        out = list(raw = raw)
      ))
      return()
    }
    
    lv = m$contract$output_schema$label_levels
    thr = suppressWarnings(as.numeric(m$threshold %||% 0.5))
    score = suppressWarnings(as.numeric(raw$score[[1]]))
    
    # score can be NA for multiclass (by design in your bundle)
    pred_result(list(
      err = NULL,
      out = list(
        text = raw$text[[1]],
        label = as.character(raw$label[[1]]),
        score = score,
        threshold = thr,
        raw = raw
      )
    ))
  }, ignoreInit = TRUE)
  
  output$pred_out_ui = renderUI({
    r = pred_result()
    if (is.null(r)) return(div(class = "small-muted", "No score yet."))
    if (!is.null(r$err)) return(div(class = "callout", tags$b("Error: "), r$err))
    
    o = r$out
    tagList(
      kpi_card("Label", o$label %||% "—", "magic"),
      tags$div(style = "height: 12px;"),
      if (!is.na(o$score)) kpi_card("Score", fmt_num(o$score, 3), "percent") else div(class = "small-muted", "Score not available for this model.")
    )
  })
  
  output$pred_raw_txt = renderPrint({
    r = pred_result()
    if (is.null(r) || is.null(r$out)) return(invisible(NULL))
    r$out$raw
  })
}

shinyApp(ui, server)
