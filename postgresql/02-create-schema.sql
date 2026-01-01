-- 01_create_sst_db.sql
-- Creates: role sst_user, database sst, schema sst
-- Note: must be executed by a superuser (often "postgres").

DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'sst_user') THEN
    CREATE ROLE sst_user WITH LOGIN PASSWORD 'change_me_strong_password';
  END IF;
END
$$;

DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_database WHERE datname = 'sst') THEN
    CREATE DATABASE sst OWNER sst_user;
  END IF;
END
$$;

-- connect to sst (some clients allow \c; in others, open a new connection to dbname=sst)
-- \c sst

CREATE SCHEMA IF NOT EXISTS sst AUTHORIZATION sst_user;

GRANT ALL PRIVILEGES ON DATABASE sst TO sst_user;
GRANT USAGE, CREATE ON SCHEMA sst TO sst_user;
