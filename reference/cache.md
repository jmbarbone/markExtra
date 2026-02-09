# cache an object

cache an R object

## Usage

``` r
cache(name, expr, overwrite = FALSE, time = 3600, env = parent.frame())
```

## Arguments

- name:

  Name of the object

- expr:

  Expression to evaluate

- overwrite:

  If \`TRUE\` saved over current objects

- time:

  Either time (seconds) for how long to cache the object(s) or the time
  as a \`POSIXt\` object

- env:

  Environment to load object into if found

## Value

Result of \`expr\`, or cached result

## Examples

``` r
res <- .POSIXct(0)
for (i in 1:6) {
  cache("foo", Sys.time(), time = 1)
  Sys.sleep(0.5)
  res[i] <- foo
}
res
#> [1] "2026-02-09 14:22:34 UTC" "2026-02-09 14:22:34 UTC"
#> [3] "2026-02-09 14:22:35 UTC" "2026-02-09 14:22:35 UTC"
#> [5] "2026-02-09 14:22:36 UTC" "2026-02-09 14:22:36 UTC"
```
