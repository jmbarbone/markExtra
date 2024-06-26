#' Round p-value
#'
#' Rounds a p-value by decimal places and reports with sig figs
#'
#' @param x A vector of p-values
#' @param n Number of digits to round by (if NULL - no rounding occurs)
#' @param sig Number of significant figures (if NULL - not used)
#' @param cutoffs A named vector for significant cutoffs
#'
#' @export
#' @name p_values
#'
#' @examples
#' set.seed(42)
#' x <- stats::pchisq(abs(runif(25)), 4)
#' print(data.frame(x = x,
#'                  p = p_round(x),
#'                  sigs = p_value_sig(x)),
#'       digits = 3,
#'       right = FALSE)

p_round <- function(x, n = 3, sig = n) {
  valid_n <- !is.null(n)

  if (valid_n) {
    below <- x < (1 / (10^(n)))
    stopifnot(n > 1L)
    x <- round(x, n)
  }

  out <- character(length(x))
  nans <- is.nan(x)
  out[nans] <- "(NaN)"

  if (valid_n) {
    out[!nans & below] <- sprintf(
      "<.%s1",
      paste(rep("0", n - 1), collapse = "")
    )
  }

  empty <- out == ""
  out[empty] <- vap_chr(
    x[empty],
    format,
    digits = sig,
    nsmall = sig,
    scientific = FALSE
  )
  out
}

#' @rdname p_values
#' @export
p_value_sig <- function(
    x,
    cutoffs = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.10)
) {
  cutoffs <- sort(cutoffs, decreasing = FALSE)
  cutoffs <- append(cutoffs, c(" " = 1))
  nm <- names(cutoffs)

  if (!is.numeric(cutoffs)) {
    stop("cutoffs must be numeric")
  }

  if (is.null(nm)) {
    stop("cutoffs must be named")
  }

  if (!all(x <= 1, na.rm = TRUE)) {
    stop("x must be 1 or less")
  }

  vap_chr(x, function(p)  nm[min(which(cutoffs >= p), na.rm = TRUE)])
}

ndigits <- function(x) {
  ok <- grep("\\.", x)
  out <- integer(length(x)) # anything not ok will be 0
  out[ok] <- nchar(sub("^[0-9]*[.]([0-9]+$)", "\\1", x[ok]))
  out
}
