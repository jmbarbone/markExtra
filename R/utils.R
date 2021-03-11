`%||%` <- function(x, y) if (is.null(x)) y else x

no_length <- function(x) {
  isTRUE(length(x) == 0L)
}
