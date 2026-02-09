# Plots an ROC model

Quickly plots a model based on my own liking

## Usage

``` r
pROC_quick_plot(
  mod,
  thres_method = c("youden", "closest.topleft"),
  col = "blue",
  ...,
  boots = 0L
)
```

## Arguments

- mod:

  An object with class "roc" (a ROC model made with \[pROC::roc()\])

- thres_method:

  The threshold method to print

- col:

  The color of the curve

- ...:

  Additional arguments passed to \[pROC::plot.roc()\]

- boots:

  Number of bootstrap replications to perform; if \< 2L will not perform
  any

## Details

When adding bootstrapped confidence intervals, the core function of
\`pROC:::ci.sp.roc()\` is replaced with a quicker version that uses
parallel processing to speed up the bootstraps and some other data
manipulation. There will likely be a delay when running with bootstraps.

## Examples

``` r
if (mark::package_available("pROC")) {
  x <- pROC::aSAH
  mod <- pROC::roc(x$outcome, x$s100b, levels = c("Good", "Poor"))
  pROC_quick_plot(mod)
if (FALSE) { # \dontrun{
  pROC_quick_plot(mod, boots = 100)
} # }
}
#> Setting direction: controls < cases
```
