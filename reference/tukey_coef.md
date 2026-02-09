# Tukey's coefficient

Returns the coefficient for Tukey's boxplot

## Usage

``` r
tukey_coef(x)
```

## Arguments

- x:

  A vector of values

## Value

The coefficient of the distance from Q1 or Q3 by IQR values

## Examples

``` r
tukey_coef(0:20)
#>  [1] -0.5 -0.4 -0.3 -0.2 -0.1  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0
#> [16]  0.0 -0.1 -0.2 -0.3 -0.4 -0.5
```
