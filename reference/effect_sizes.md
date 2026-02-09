# Effect sizes conversions

Calculate effect sizes and conversions

## Usage

``` r
odds_ratio(a, b = NULL, c = NULL, d = NULL, type = "hits_misses")

odds2d(odds, var = FALSE)

odds2r(odds, n1 = 4, n2 = n1, var = FALSE)

r2cohend(r, var = FALSE)

cohend2r(cohend, n1 = NULL, n2 = n1, var = FALSE)

cohend2odds(cohend, var = FALSE)
```

## Arguments

- a, b, c, d:

  Values in the contingency table; valid if either \`a\`, \`b\`, \`c\`
  and \`d\` are the same length or if \`a\` has length of \`4\` and
  \`b\`, \`c\`, and \`d\` are \`NULL\`.

- type:

  Type of entry for \[odds_ratio()\]

- odds, r, cohend:

  A vector of effect sizes

- var:

  Logical, if \`TRUE\` converts the variance instead

- n1, n2:

  The ns of the groups

## Details

\`odds\` are log odds

## Examples

``` r
x <- c(26, 13, 5, 6)
odds_ratio(x)           # 2.4
#> [1] 2.4
odds2r(2.4)             # 0.2346004
#> [1] 0.2346004
odds2d(2.4)             # 0.4826712
#> [1] 0.4826712
r2cohend(0.2346004)    # 0.4826712
#> [1] 0.4826712
cohend2odds(0.4826712) # 0.8754687
#> [1] 0.8754687
cohend2r(0.4826712)    # 0.2346004
#> [1] 0.2346004
exp(0.8754687)          # 2.4
#> [1] 2.4

```
