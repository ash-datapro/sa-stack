library(httr)
library(jsonlite)

base_url = "http://127.0.0.1:8000"

# health
r1 = GET(paste0(base_url, "/health"))
cat(content(r1, "text", encoding = "UTF-8"), "\n\n")

# meta
r2 = GET(paste0(base_url, "/meta"))
cat(content(r2, "text", encoding = "UTF-8"), "\n\n")

# predict: missing body fields (expect 400)
r3 = POST(
  paste0(base_url, "/predict"),
  body = list(),
  encode = "json"
)
cat("predict empty body:\n", content(r3, "text", encoding = "UTF-8"), "\n\n")

# predict: single text
r4 = POST(
  paste0(base_url, "/predict"),
  body = list(text = "This movie was fantastic and I loved it."),
  encode = "json"
)
cat("predict single:\n", content(r4, "text", encoding = "UTF-8"), "\n\n")

# predict: batch
r5 = POST(
  paste0(base_url, "/predict"),
  body = list(texts = c("Loved it. Great acting.", "Worst movie ever.")),
  encode = "json"
)
cat("predict batch:\n", content(r5, "text", encoding = "UTF-8"), "\n\n")

# predict: bad request (wrong type)
r6 = POST(
  paste0(base_url, "/predict"),
  body = list(text = 123),
  encode = "json"
)
cat("predict wrong type:\n", content(r6, "text", encoding = "UTF-8"), "\n\n")
