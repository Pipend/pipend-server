curl \
  --data '{"queryAndParams":{"executableQueryText":"curl -IL https://www.google.com/","executableQueryParams":{}},"connection":{"tag":"Curl", "contents": []}}' \
  ":3000/api/query/arbitrary/task1"
