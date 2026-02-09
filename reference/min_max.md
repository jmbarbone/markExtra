# Min-max normalization

Min-max normalization

## Usage

``` r
min_max(x, lower = 0, upper = 1)
```

## Arguments

- x:

  A vector of \`numeric\` values

- lower, upper:

  The boundaries of the normalized score; defaults to \`0\` and \`1\`,
  respectively. \`upper\` must be greater than \`lower\`.

## Value

A vector of x normalizes so the lower scores is \`lower\` and highest
score is \`upper\`

## Examples

``` r
x <- c(0.34, 0.24, 0.65, 0.21, 0.18, 0.79, 0.13, 0.94, 0.22, 0.1)
min_max(x)
#>  [1] 0.28571429 0.16666667 0.65476190 0.13095238 0.09523810 0.82142857
#>  [7] 0.03571429 1.00000000 0.14285714 0.00000000
min_max(x, upper = 10)
#>  [1]  2.8571429  1.6666667  6.5476190  1.3095238  0.9523810  8.2142857
#>  [7]  0.3571429 10.0000000  1.4285714  0.0000000
min_max(x, lower = -1)
#>  [1] -0.4285714 -0.6666667  0.3095238 -0.7380952 -0.8095238  0.6428571
#>  [7] -0.9285714  1.0000000 -0.7142857 -1.0000000
min_max(x, l = 10, u = 20)
#>  [1] 12.85714 11.66667 16.54762 11.30952 10.95238 18.21429 10.35714 20.00000
#>  [9] 11.42857 10.00000
```
