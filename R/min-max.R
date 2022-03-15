
#' Min-max normalization
#'
#' @param x A vector of `numeric` values
#' @param lower,upper The boundaries of the normalized score; defaults to `0`
#'   and `1`, respectively.  `upper` must be greater than `lower`.
#' @return A vector of x normalizes so the lower scores is `lower` and highest score is `upper`
#' @export
#' @examples
#' x <- c(0.34, 0.24, 0.65, 0.21, 0.18, 0.79, 0.13, 0.94, 0.22, 0.1)
#' min_max(x)
#' min_max(x, upper = 10)
#' min_max(x, lower = -1)
#' min_max(x, l = 10, u = 20)
min_max <- function(x, lower = 0, upper = 1) {
  stopifnot(is.numeric(x), lower < upper)
  min <- min(x, na.rm = TRUE)
  max <- max(x, na.rm = TRUE)
  lower + (x - min) * (upper - lower) / (max - min)
}
