#!/usr/bin/env bash
set -euo pipefail

# --- tell the API where the model bundle is ---
export SENTIMENT_MODEL_PATH="model.rds"
export SENTIMENT_MAX_BATCH="64"
export SENTIMENT_MAX_CHARS="5000"

# Start API (foreground)
R -q -e "plumber::plumb('main.R')$run(host='0.0.0.0', port=8000)"

#---> another terminal after this point

# health
curl -i http://127.0.0.1:8000/health

# meta
curl -i http://127.0.0.1:8000/meta

# predict (empty body -> should 400)
curl -i -X POST http://127.0.0.1:8000/predict \
-H "Content-Type: application/json" \
-d '{}'

# predict single text
curl -i -X POST http://127.0.0.1:8000/predict \
-H "Content-Type: application/json" \
-d '{"text":"This movie was fantastic and I loved it."}'

# predict batch
curl -i -X POST http://127.0.0.1:8000/predict \
-H "Content-Type: application/json" \
-d '{"texts":["Loved it. Great acting.","Worst movie ever."]}'

# bad json
curl -i -X POST http://127.0.0.1:8000/predict \
-H "Content-Type: application/json" \
-d '{bad json}'

# too-large text (forces 400 if you set max chars low)
curl -i -X POST http://127.0.0.1:8000/predict \
-H "Content-Type: application/json" \
-d '{"text":"'"$(python - <<'PY'
print("a"*6000)
PY
)"'"}'
