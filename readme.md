```
stack ghci

main
```

Run a cURL query:

```
add a curl https://www.google.com -L
```

run a PostgreSQL query:

```
add b sql ("", "select 2 + 5")
```

Kill a task:

```
add a curl "http://httpbin.org/delay/3" -s
kill a
```

(`a` is the task's name)
