# Pipe Web Server

## Setup

### Server Configuration

```
mkdir -p ./configs
echo '{
  "localhost": {
    "tag": "PostgreSQL",
    "contents": {
      "connectionString": "postgres://127.0.0.1"
    }
  }
}' > ./configs/connections.json
```

### Build

```
stack setup
stack build
stack exec pipend-server-exe
```

## Usae Examples

Check [test](/test) for more examples.

**Running a Query with pre-defined Connection**
```
curl \
  --data '{"executableQueryText":"select 5 * 7","executableQueryParams":{}}' \
  ":3000/api/query/connection/localhost/taskid/taska"
```

**Killing a Query**
```
curl ":3000/api/kill/taska"
```
