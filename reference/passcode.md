# passcode

Generate a passcode

## Usage

``` r
passcode(
  len = 14:16,
  sep = "_",
  one = c("adjectives", "colors"),
  two = c("animals", "nouns"),
  numbers = len%/%4L
)
```

## Arguments

- len:

  length of passcode

- sep:

  separator between words

- one, two:

  Type of words to use (see \`?codename::codename\` for \`type\`)

- numbers:

  Number of digits to append

## Value

A passcode, as a string

## Details

See \`?codename::codename\` for more details
