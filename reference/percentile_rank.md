# Percentile rank

The bounds of a percentile rank are \> 0 and \< 100.

A percentile rank here is the proportion of scores that are less than
the current score.

\$\$PR = (c_L + 0.5 f_i) / N \* 100\$\$

Where

\\c_L\\ is the frequency of scores less than the score of interest

\\f_i\\ is the frequency of the score of interest

## Usage

``` r
percentile_rank(x)
```

## Arguments

- x:

  A vector of values to rank

## Value

A percentile rank between 0 and 100

## Details

Computes a percentile rank for each score in a set.

## Examples

``` r
percentile_rank(0:9)
#>  0  1  2  3  4  5  6  7  8  9 
#>  5 15 25 35 45 55 65 75 85 95 
x <- c(1, 1, 2, 5, 7, NA_integer_, 7, 10)
percentile_rank(x)
#>     1     1     2     5     7  <NA>     7    10 
#> 12.50 12.50 31.25 43.75 62.50    NA 62.50 81.25 
# compare to dplyr
# dplyr::percent_rank(x) * 100
```
