#' Reverse log transformation for ggplot
#'
#' Show a reversed order for a log transformed scale from ggplot. Ripped from:
# nolint next: line_length_linter.
#' https://stackoverflow.com/questions/11053899/how-to-get-a-reversed-log10-scale-in-ggplot2
#'
#' @inheritParams base::log
#'
#' @author Brian Diggs (https://stackoverflow.com/users/892313/brian-diggs)
#'
#' @export
reverse_log_trans <- function(base = exp(1)) {
  require_namespace("scales")
  scales::trans_new(
    name      = paste0("reverselog-", format(base)),
    transform = function(x) -log(x, base),
    inverse   = function(x) base^(-x),
    breaks    = scales::log_breaks(base = base),
    domain    = c(1e-100, Inf)
  )
}
