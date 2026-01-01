#!/usr/bin/env bash
set -euo pipefail

# --- Postgres connection for training/ingest ---
export SST_DB_HOST="localhost"
export SST_DB_PORT="5432"
export SST_DB_ADMIN_USER="postgres"
export SST_DB_ADMIN_PASSWORD="api1"

# App user/db
export SST_DB_USER="sst_user"
export SST_DB_PASSWORD="api"
export SST_DB_NAME="sst"

# Training config (optional)
export SST_DB_VIEW="sst.v_sentence_sentiment"
export SST_TASK="sst2"               # or sst5
export SST_MAX_TOKENS="40000"
export SST_MIN_TOKEN_COUNT="2"
export SST_NGRAM_MAX="2"
export SST_GRID_SIZE="25"

# --- 1) Create role + database (runs in RStudio/CLI via DBI) ---
#Rscript create-and-load-db.R

# --- 2) Train model and save model.rds ---
Rscript 01-train-model.R
