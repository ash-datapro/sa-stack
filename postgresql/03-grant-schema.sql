-- run connected as a superuser (postgres)
GRANT CONNECT, TEMP ON DATABASE sst TO sst_user;
GRANT CREATE ON DATABASE sst TO sst_user;

CREATE SCHEMA IF NOT EXISTS sst AUTHORIZATION sst_user;
ALTER SCHEMA sst OWNER TO sst_user;
