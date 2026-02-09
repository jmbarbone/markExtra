# Round p-value

Rounds a p-value by decimal places and reports with sig figs

## Usage

``` r
p_round(x, n = 3, sig = n)

p_value_sig(x, cutoffs = c(`***` = 0.001, `**` = 0.01, `*` = 0.05, . = 0.1))
```

## Arguments

- x:

  A vector of p-values

- n:

  Number of digits to round by (if NULL - no rounding occurs)

- sig:

  Number of significant figures (if NULL - not used)

- cutoffs:

  A named vector for significant cutoffs

## Examples

``` r
set.seed(42)
x <- stats::pchisq(abs(runif(25)), 4)
print(data.frame(x = x,
                 p = p_round(x),
                 sigs = p_value_sig(x)),
      digits = 3,
      right = FALSE)
#>    x        p     sigs
#> 1  0.077573 0.078 .   
#> 2  0.080817 0.081 .   
#> 3  0.009309 0.009 **  
#> 4  0.065680 0.066 .   
#> 5  0.041686 0.042 *   
#> 6  0.028384 0.028 *   
#> 7  0.053259 0.053 .   
#> 8  0.002168 0.002 **  
#> 9  0.043475 0.043 *   
#> 10 0.049297 0.049 *   
#> 11 0.022518 0.023 *   
#> 12 0.051049 0.051 .   
#> 13 0.080465 0.080 .   
#> 14 0.007493 0.007 **  
#> 15 0.022934 0.023 *   
#> 16 0.081249 0.081 .   
#> 17 0.086921 0.087 .   
#> 18 0.001659 0.002 **  
#> 19 0.024111 0.024 *   
#> 20 0.032632 0.033 *   
#> 21 0.076018 0.076 .   
#> 22 0.002297 0.002 **  
#> 23 0.088524 0.089 .   
#> 24 0.082228 0.082 .   
#> 25 0.000827 <.001 *** 
```
