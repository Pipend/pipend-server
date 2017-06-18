#!/bin/sh
':' //; exec "$(command -v nodejs || command -v node)" "$0" "$@"

// echo "select \$[R.pipe(R.prop('fields'), R.join(', '))]\$ from Table" | ./pipe-sql-query-formatter.js "{\"fields\": [\"colA\", \"colB\"]}"

const R = require('ramda')

const params = JSON.parse(process.argv.slice(2)[0])

var input = ''
process.stdin.on('data', d => input = input + d)
process.stdin.on('end', () => {
    const regex = /\$\[(([^\[\$])+)\]\$/ig
    console.log(
      input.replace(regex, m =>
        eval(m.slice(2).slice(0, -2))(params)
      )
    )
})
