# Optimal threshold from pROC mod

Find the optimal threshold from a pROC mod

## Usage

``` r
pROC_optimal_threshold(mod, method = c("youden", "top_left"), ...)
```

## Arguments

- mod:

  An object with class "roc" (a ROC model made with pROC::roc(.))

- method:

  Method to determine optimal threshold

- ...:

  Additional arguments passed to \[pROC::ci.thresholds()\]

## Examples

``` r
x <- pROC::aSAH
mod <- pROC::roc(x$outcome, x$s100b, levels=c("Good", "Poor"))
#> Setting direction: controls < cases

pROC_optimal_threshold(mod)
#> youden: 0.205
#> Sensitivity = 0.8055556 (95% CI 0.7083333 - 0.8892361)
#> Specificity = 0.6341463 (95% CI 0.4878049 - 0.7804878)
```
