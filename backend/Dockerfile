# Sentiment backend (Plumber API) — production-style
# Build: docker build -t sentiment-backend .
# Run:   docker run --rm -p 8000:8000 sentiment-backend

FROM rocker/r-ver:4.4.2

# ---- system deps (R packages + Postgres client libs) ----
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

# ---- working directory ----
WORKDIR /app

# ---- install R packages ----
# Keep this separate for docker layer caching.
COPY requirements /app/requirements

RUN R -q -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); \
             pkgs = readLines('requirements'); \
             pkgs = pkgs[nzchar(pkgs)]; \
             install.packages(pkgs, Ncpus = parallel::detectCores());"

# ---- copy API + artifacts ----
COPY api /app/api

# Optional: copy training/utility scripts too (harmless to include)
# If you don't want them in the image, remove these two lines.
COPY 01-train-model.R /app/01-train-model.R
COPY 02-main.R /app/02-main.R

# ---- environment defaults ----
ENV SENTIMENT_MODEL_PATH=/app/api/model.rds
ENV SENTIMENT_MAX_BATCH=64
ENV SENTIMENT_MAX_CHARS=5000

EXPOSE 8000

# ---- run plumber ----
CMD ["R", "-q", "-e", "pr = plumber::plumb('/app/api/main.R'); pr$run(host='0.0.0.0', port=8000)"]
