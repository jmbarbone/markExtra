# Add Mahalanobis distance

Add a mahalanobis distance to the end of a data frame.

## Usage

``` r
add_mahalanobis(
  df,
  ...,
  .inverted = FALSE,
  .name = "md",
  .p = "p_value",
  tolerance = .Machine$double.eps
)
```

## Arguments

- df:

  A data.frame

- ...:

  Columns to select

- .inverted:

  Logical. If TRUE, covariance matrix (p x p) of the distribution is
  supposed to contain the inverse of the covariance matrix.

- .name:

  The name of the new column for the distance value

- .p:

  The name for the new column for the p-value calculation

- tolerance:

  \`tol\` in \[base::solve()\]

## Examples

``` r
df <- head(iris)
add_mahalanobis(df, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species       md   p_value
#> 1          5.1         3.5          1.4         0.2  setosa 1.901692 0.7538342
#> 2          4.9         3.0          1.4         0.2  setosa 3.634221 0.4577653
#> 3          4.7         3.2          1.3         0.2  setosa 4.064753 0.3973134
#> 4          4.6         3.1          1.5         0.2  setosa 4.064753 0.3973134
#> 5          5.0         3.6          1.4         0.2  setosa 2.167915 0.7049075
#> 6          5.4         3.9          1.7         0.4  setosa 4.166667 0.3839196
df[1, ] <- NA_real_
add_mahalanobis(df, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species  md   p_value
#> 1           NA          NA           NA          NA    <NA>  NA        NA
#> 2          4.9         3.0          1.4         0.2  setosa 3.2 0.5249309
#> 3          4.7         3.2          1.3         0.2  setosa 3.2 0.5249309
#> 4          4.6         3.1          1.5         0.2  setosa 3.2 0.5249309
#> 5          5.0         3.6          1.4         0.2  setosa 3.2 0.5249309
#> 6          5.4         3.9          1.7         0.4  setosa 3.2 0.5249309
```
