# Assign add

this needs to be tested...

## Usage

``` r
e1 %=+% e2

e1 %=-% e2
```

## Arguments

- e1:

  Object to be updated

- e2:

  right side statement

## Examples

``` r
val <- 1
val %=+% 2
val # 3
#> [1] 3
val %=+% 2
val # 5
#> [1] 5
val %=-% 1
val # 4
#> [1] 4
if (FALSE) { # \dontrun{
val %=+% c(1, 2, 3) # fails
} # }
val <- c(1, 2, 3)
val %=+% c(1, 2, 3) # 2 4 6
```
