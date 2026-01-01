# sidebar.R — updated (modernized + aligns with new app.R layout)
# - Primary focus: predict workflow
# - Connection moved to "API settings (advanced)"
# - Optional status badge slot at top (app.R can feed it)

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
})

sidebar_ui = function(id) {
  ns = NS(id)
  
  bslib::sidebar(
    width = 420,
    open = "desktop",
    class = "app-sidebar",
    
    h4("Controls"),
    
    # Optional: app.R can render a connection badge here
    uiOutput(ns("conn_badge")),
    
    # Predict (primary)
    div(class = "section-block",
        div(class = "section-title", "Predict request"),
        
        h6("Input type", class = "section-h"),
        selectInput(
          ns("predict_mode"),
          NULL,
          choices = c("Single text" = "single", "Batch (one per line)" = "batch", "Upload CSV" = "upload"),
          selected = "single"
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'single'", ns("predict_mode")),
          textAreaInput(
            ns("text_single"),
            NULL,
            value = "This movie was fantastic and I loved it.",
            width = "100%",
            height = "120px",
            placeholder = "Enter text to score..."
          )
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'batch'", ns("predict_mode")),
          textAreaInput(
            ns("text_batch"),
            NULL,
            value = "Loved it. Great acting.\nWorst movie ever.\nIt was okay, not great.",
            width = "100%",
            height = "160px",
            placeholder = "Each line becomes one prediction..."
          )
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'upload'", ns("predict_mode")),
          fileInput(ns("upload_csv"), "Upload CSV", accept = c(".csv")),
          uiOutput(ns("upload_mapping_ui")),
          checkboxInput(ns("upload_drop_empty"), "Drop empty texts", value = TRUE),
          numericInput(ns("upload_max_n"), "Max rows to score", value = 200, min = 1, step = 50)
        ),
        
        actionButton(ns("btn_predict"), "Run /predict", class = "btn-success w-100"),
        div(style = "margin-top:10px;",
            tags$small(class = "text-muted", textOutput(ns("predict_hint"), inline = TRUE))
        )
    ),
    
    hr(),
    
    # Advanced sections (collapsed)
    bslib::accordion(
      open = FALSE,
      
      bslib::accordion_panel(
        "API settings (advanced)",
        div(class = "section-block",
            div(class = "section-title", "Connection"),
            textInput(
              ns("api_base"),
              "API base URL",
              value = Sys.getenv("SENTIMENT_API_BASE", unset = "http://127.0.0.1:8000")
            ),
            fluidRow(
              column(6, actionButton(ns("btn_health"), "Check /health", class = "btn-outline-light w-100")),
              column(6, actionButton(ns("btn_meta"), "Fetch /meta", class = "btn-outline-light w-100"))
            )
        )
      ),
      
      bslib::accordion_panel(
        "Ops (optional)",
        passwordInput(ns("reload_token"), "Reload token (optional)", value = ""),
        actionButton(ns("btn_reload"), "POST /reload", class = "btn-outline-warning w-100"),
        tags$small(
          class = "text-muted",
          "If your API requires a reload token, it must match SENTIMENT_RELOAD_TOKEN on the server."
        )
      )
    )
  )
}

# Optional: sidebar can manage upload parsing + mapping UI itself
sidebar_server = function(id, conn_badge_ui = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    
    # badge passthrough (optional)
    output$conn_badge = renderUI({
      conn_badge_ui() %||% NULL
    })
    
    # upload df
    upload_df = reactive({
      fi = input$upload_csv
      if (is.null(fi) || is.null(fi$datapath) || !file.exists(fi$datapath)) return(NULL)
      df = tryCatch(readr::read_csv(fi$datapath, show_col_types = FALSE), error = function(e) e)
      if (inherits(df, "error")) {
        showNotification(paste0("CSV read error: ", df$message), type = "error", duration = 8)
        return(NULL)
      }
      df
    })
    
    output$upload_mapping_ui = renderUI({
      df = upload_df()
      if (is.null(df)) return(NULL)
      cols = names(df)
      guess = cols[which(grepl("text|sentence|review|comment|body", tolower(cols)))[1]] %||% cols[1]
      selectInput(session$ns("upload_text_col"), "Text column", choices = cols, selected = guess)
    })
    
    output$predict_hint = renderText({
      base = input$api_base %||% "http://127.0.0.1:8000"
      paste0("POST ", rtrim(base), "/predict")
    })
    
    # Return a small bundle of reactives so app.R can consume cleanly
    list(
      api_base = reactive(input$api_base),
      btn_health = reactive(input$btn_health),
      btn_meta = reactive(input$btn_meta),
      btn_reload = reactive(input$btn_reload),
      btn_predict = reactive(input$btn_predict),
      
      predict_mode = reactive(input$predict_mode),
      text_single = reactive(input$text_single),
      text_batch = reactive(input$text_batch),
      
      upload_df = upload_df,
      upload_text_col = reactive(input$upload_text_col),
      upload_drop_empty = reactive(input$upload_drop_empty),
      upload_max_n = reactive(input$upload_max_n),
      
      reload_token = reactive(input$reload_token)
    )
  })
}
