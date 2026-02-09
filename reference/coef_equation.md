# Print a model as an equation

Print a model as an equation

## Usage

``` r
coef_equation(mod, FUN = sf2, ...)
```

## Arguments

- mod:

  A model, something which \`stats::coef()\` can extracted a named
  vector

- FUN:

  A function to apply to the values of \`stats::coef(mod)\`, for
  formatting; default formats to 2 significant figures with trailing
  zeros

- ...:

  Additional arguments passed to \`FUN\`

## Examples

``` r
mod <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
coef_equation(mod)
#> [1] "2.2 + Sepal.Width * 0.60 + Petal.Length * 0.47"
# "2.2 + Sepal.Width \* 0.60 + Petal.Length \* 0.47"
```
