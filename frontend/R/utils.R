# utils.R — updated (robust API + safer parsing + helpers used across modules)
# Key fixes:
# - Parse JSON with simplify OFF (avoids atomic vectors / surprise data.frames)
# - api_post_json only adds headers if present; requires named character vector
# - Return {ok, status, url, text, parsed} for consistent error handling
# - Adds normalize_predictions() helper used by server modules if desired

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(stringr)
  library(readr)
  library(tibble)
  library(dplyr)
})

`%||%` = function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

rtrim = function(x) sub("/+$", "", x)

safe_json = function(x) {
  tryCatch(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"), error = function(e) "{}")
}

# ---- JSON parsing (simplify OFF) ----
parse_json_safe = function(txt) {
  tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE, simplifyDataFrame = FALSE),
    error = function(e) NULL
  )
}

# ---- API wrappers ----
api_get = function(base, path, timeout_sec = 10) {
  url = paste0(rtrim(base), path)
  r = httr::GET(url, httr::timeout(timeout_sec))
  txt = httr::content(r, "text", encoding = "UTF-8")
  
  list(
    ok = httr::status_code(r) >= 200 && httr::status_code(r) < 300,
    status = httr::status_code(r),
    url = url,
    text = txt,
    parsed = parse_json_safe(txt)
  )
}

api_post_json = function(base, path, body, timeout_sec = 30, headers = NULL) {
  url = paste0(rtrim(base), path)
  
  cfg = list(httr::timeout(timeout_sec))
  
  if (!is.null(headers) && length(headers) > 0) {
    if (is.list(headers)) headers = unlist(headers, use.names = TRUE)
    headers = as.character(headers)
    
    if (is.null(names(headers)) || any(names(headers) == "")) {
      stop("headers must be a named character vector, e.g. c('X-Token' = 'abc').")
    }
    
    cfg = c(cfg, list(httr::add_headers(.headers = headers)))
  }
  
  r = httr::POST(
    url,
    body = body,
    encode = "json",
    config = cfg
  )
  
  txt = httr::content(r, "text", encoding = "UTF-8")
  
  list(
    ok = httr::status_code(r) >= 200 && httr::status_code(r) < 300,
    status = httr::status_code(r),
    url = url,
    text = txt,
    parsed = parse_json_safe(txt)
  )
}

api_health = function(base) api_get(base, "/health")
api_meta   = function(base) api_get(base, "/meta")

api_reload = function(base, reload_token = "") {
  hdr = NULL
  if (nzchar(reload_token)) hdr = c("X-Reload-Token" = reload_token)
  api_post_json(base, "/reload", body = list(), headers = hdr)
}

# body should be list(text="...") or list(texts=c(...))
api_predict = function(base, body) api_post_json(base, "/predict", body = body, timeout_sec = 60)

# ---- input helpers ----
coerce_texts_from_inputs = function(mode, text_single, text_batch, upload_df, upload_text_col,
                                    upload_drop_empty = TRUE, upload_max_n = 200) {
  texts = NULL
  
  if (identical(mode, "single")) {
    t = str_squish(text_single %||% "")
    if (nzchar(t)) texts = t
  }
  
  if (identical(mode, "batch")) {
    lines = str_split(text_batch %||% "", "\n", simplify = FALSE)[[1]]
    lines = str_squish(lines)
    lines = lines[nzchar(lines)]
    if (length(lines) > 0) texts = lines
  }
  
  if (identical(mode, "upload")) {
    if (is.null(upload_df) || is.null(upload_text_col) || !nzchar(upload_text_col)) return(NULL)
    if (!upload_text_col %in% names(upload_df)) return(NULL)
    
    vec = as.character(upload_df[[upload_text_col]])
    vec = str_squish(vec)
    if (isTRUE(upload_drop_empty)) vec = vec[nzchar(vec)]
    if (length(vec) > upload_max_n) vec = vec[seq_len(upload_max_n)]
    if (length(vec) > 0) texts = vec
  }
  
  texts
}

validate_texts = function(texts, max_batch = 256, max_chars = 5000) {
  if (is.null(texts) || !is.character(texts) || length(texts) < 1) {
    return(list(ok = FALSE, message = "No valid text inputs found."))
  }
  
  texts = str_squish(texts)
  texts = texts[nzchar(texts)]
  
  if (length(texts) < 1) {
    return(list(ok = FALSE, message = "All inputs are empty after trimming."))
  }
  
  if (length(texts) > max_batch) {
    return(list(ok = FALSE, message = paste0("Batch too large. Max = ", max_batch)))
  }
  
  if (any(nchar(texts) > max_chars)) {
    return(list(ok = FALSE, message = paste0("One or more texts exceed max length (", max_chars, " chars).")))
  }
  
  list(ok = TRUE, texts = texts)
}

make_predict_body = function(texts) {
  if (length(texts) == 1) list(text = texts[[1]]) else list(texts = texts)
}

# ---- prediction normalization ----
normalize_predictions = function(preds) {
  empty_out = function() tibble::tibble(text = character(), label = character(), score = numeric())
  
  if (is.null(preds)) return(empty_out())
  
  # unwrap a value that might be list-wrapped (even multiple times) into a scalar
  unwrap_scalar = function(v, default = NULL) {
    if (is.null(v)) return(default)
    
    # Unwrap nested lists like list("x") or list(list("x"))
    while (is.list(v)) {
      if (length(v) == 0) return(default)
      v = v[[1]]
      if (is.null(v)) return(default)
    }
    
    if (length(v) == 0) return(default)
    v[[1]]
  }
  
  as_scalar_chr = function(v, default = NA_character_) {
    v = unwrap_scalar(v, default = default)
    if (is.null(v) || length(v) == 0) return(default)
    as.character(v)
  }
  
  as_scalar_num = function(v, default = NA_real_) {
    v = unwrap_scalar(v, default = default)
    if (is.null(v) || length(v) == 0) return(default)
    suppressWarnings(as.numeric(v))
  }
  
  if (is.data.frame(preds)) {
    dfp = tibble::as_tibble(preds)
    if (!"text" %in% names(dfp)) dfp$text = NA_character_
    if (!"label" %in% names(dfp)) dfp$label = NA_character_
    if (!"score" %in% names(dfp)) dfp$score = NA_real_
    
    # Handle list-columns safely (common when JSON is parsed with simplifyVector = FALSE)
    return(dfp %>%
             dplyr::transmute(
               text = vapply(text, as_scalar_chr, character(1)),
               label = vapply(label, as_scalar_chr, character(1)),
               score = vapply(score, as_scalar_num, numeric(1))
             ))
  }
  
  if (is.list(preds)) {
    get_field = function(x, nm) {
      if (is.list(x) && !is.null(x[[nm]])) x[[nm]] else NULL
    }
    
    return(tibble::tibble(
      text  = vapply(preds, function(x) as_scalar_chr(get_field(x, "text")), character(1)),
      label = vapply(preds, function(x) as_scalar_chr(get_field(x, "label")), character(1)),
      score = vapply(preds, function(x) as_scalar_num(get_field(x, "score")), numeric(1))
    ))
  }
  
  empty_out()
}

# ---- reports/artifacts ----
resolve_reports_dir = function() {
  override = Sys.getenv("SENTIMENT_REPORTS_DIR", unset = NA_character_)
  if (!is.na(override) && dir.exists(override)) return(normalizePath(override, winslash = "/", mustWork = FALSE))
  
  candidates = c("reports", "backend/reports", "../backend/reports", "../../backend/reports")
  for (p in candidates) if (dir.exists(p)) return(normalizePath(p, winslash = "/", mustWork = FALSE))
  NA_character_
}

read_csv_safe = function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(readr::read_csv(path, show_col_types = FALSE), error = function(e) NULL)
}

list_report_images = function(reports_dir) {
  if (is.na(reports_dir) || !dir.exists(reports_dir)) return(character())
  imgs = c("roc_dev.png", "pr_dev.png")
  imgs[file.exists(file.path(reports_dir, imgs))]
}
