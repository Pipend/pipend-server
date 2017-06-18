curl \
  --data '{"queryAndParams":{"executableQueryText":"select 5 * 7","executableQueryParams":{}},"connection":{"tag":"PostgreSQL","contents":{"connectionString":"postgres://127.0.0.1"}}}' \
  ":3000/api/query/arbitrary/task1"
