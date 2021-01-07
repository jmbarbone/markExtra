#' Proportions
#'
#' Calculates a proportion from a vector or data.frame/matrix.
#'
#' @param x A data.frame or a vector.
#' @param ... Additional arguments to be passed to methods.
#' @param col A character string of the column name which holds the groups
#'
#' @examples
#' proportion(iris, "Species")
#' proportion(iris$Species)
#'
#' @export

proportion <- function(x, ...) {
  UseMethod("proportion", x)
}

#' @export
#' @rdname proportion
proportion.default <- function(x, ...) {
  if (!inherits(x, "factor")) {
    x <- factor(x, levels = unique(as.character(x)))
  }

  jordan::vap_dbl(split(x, x), length, .nm = TRUE) / length(x)
}

#' @export
#' @rdname proportion
proportion.data.frame <- function(x, col, ...) {
  cn <- colnames(x)
  is_in <- col %in% cn

  stopifnot("Column name not found" = any(is_in),
            "Multiple matches found" = sum(is_in) == 1L)

  props <- proportion.default(x[[col]])
  jordan::vector2df(props, col, "prop")
}


#' Fisher's method for combined probabilities
#'
#' Computes Fisher's method for combined probabilities
#'
#' @details
#' Values greater than 1 and less than 0 are removed from the calculation.
#'   Values of 0 are red
#'
#' @param x A vector of p-values.
#' @param zeros Logical, if `TRUE` will use zeros in calculation, otherwise
#'   recoded as `.Machine$double.xmin`.  This still will produce a very small
#'   result but will be less likely to produce `0`
#' @export

fishers_method <- function(x, zeros = FALSE) {
  x <- x[x <= 1 & x >= 0]

  if (!zeros) {
    x[x == 0] <- .Machine$double.xmin
  }

  stats::pchisq(
    (-2) * sum(log(x)),
    df = 2 * length(x),
    lower.tail = FALSE
  )
}

#' Inter Quartile Ranges
#'
#' Computes the IQR magnitude of a vector
#'
#' @param x A vector of values
#' @param na.rm Logical. If `TRUE`, any `NA` and `NaN`'s are removed from `x` before the median and quantiles are computed.
#'
#' @export
#' @examples
#' iqrs(stats::rchisq(100, 2))

iqrs <- function(x, na.rm = FALSE) {
  (x - stats::median(x, na.rm = na.rm)) /
    diff(stats::quantile(x, c(.25, .75), names = FALSE, na.rm = na.rm))
}

#' Percentile rank
#'
#' Computes a percentile rank for each score in a set.
#' The bounds of a percentile rank are > 0 and < 100.
#' A percentile rank here is the proportion of scores that are less than the
#'   current score.
#'
#' @details
#' This is not a very fast formula, however it is correct.
#'
#' \deqn{PR = (c_L + 0.5 f_i) / N * 100}
#'
#' Where
#'
#'   \eqn{c_L} is the frequency of scores less than the score of interest
#'
#'   \eqn{f_i} is the frequency of the score of interest
#'
#'
#' @param x A vector of values to rank
#' @export
#'
#' @return A percentile rank between 0 and 100
#'
#' @examples
#' x <- c(1, 1, 2, 5, 7, 7, 8, 10)
#' percentile_rank(x)
#' \dontrun{
#' dplyr::percent_rank(x) * 100
#' }

percentile_rank <- function(x) {
  p <- proportion(x)
  (cumsum(p) - p * .5)[match(x, sort.int(unique.default(x)))] * 100
}

#' Pooled standard deviation
#'
#' Computes the pooled standard error
#'
#' @param ns A vector of N values.
#' @param ses A vector of standard errors.
#' @param max Logical value.  Whether to compute the max pooled deviation.
#' @export

sd_pooled <- function(ns, ses, max = FALSE) {
  st_devs <- ses * vap_dbl(ns, sqrt)

  a <- if (max) {
    0
  } else {
    -length(st_devs)
  }

  sqrt(sum(vap_dbl(ns, function(x) x - 1) * vap_dbl(st_devs, function(x) x^2)) / sum(ns, a))
}

#' Standard error
#'
#' Returns the standard error of a vector or columns of a data.frame
#'
#' @param x A vector or data.frame
#' @param na.rm Passed to `stats::sd()`.
#' @return A vector
#' @export

sterr <- function(x, na.rm = F)
{
  UseMethod("sterr", x)
}

sterr.numeric <- function(x, na.rm = F) {
  n <- length(x)
  sd <- stats::sd(x, na.rm = na.rm)
  sd / sqrt(n)
}

sterr.data.frame <- function(x, na.rm = F) {
  jordan::vap_dbl(x, sterr, na.rm = na.rm)
}
