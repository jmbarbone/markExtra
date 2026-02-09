# Generate a random password

Make a password and send to the clipboard with
\[mark::write_clipboard()\]

## Usage

``` r
make_password(n = 16L, specials = "!@#$%^&*_+-=?", silent = TRUE)
```

## Arguments

- n:

  The character length desired (default: \`16L\`)

- specials:

  A single string of characters other than all letters (from \`LETTERS\`
  and \`letters\`) and numbers (\`0:9\`)

- silent:

  if \`FALSE\` will call \`cat()\` on the password

## Value

\`NULL\`, invisible
