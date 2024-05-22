#' Is newer
#'
#' Check if a file or time is newer than another
#'
#' @param x,y, Files (as character paths) or `Date`/`POSIXt` object
#' @returns `TRUE` or `FALSE` for if `x` is newer than `y`.  If file `y` does
#'   not exist, `TRUE` is returned
#' @export

`%newer%` <- function(x, y) {
  # some issues can occur with x > y when they're about the same.  Using
  # all.equal() solves some of this but the tolerance may have to be adjusted
  # for specific use cases

  x_time <- new_as_time(x, must_exist = TRUE)
  y_time <- new_as_time(y, must_exist = FALSE)

  if (is.na(y_time)) {
    # meaning y does not exist
    return(TRUE)
  }

  if (isTRUE(all.equal(
    x_time,
    y_time,
    tolerance = getOption("markExtra.newer.tolerance", 1)
  ))) {
    # check if the times are approx the same
    #
    # seem to get some differences in approximately the same:
    #   "Mean absolute difference: 0.3585339"
    # if approximately example, don't overwrite.  I guess this could cause
    # issues but we'll deal with them when they come up
    return(FALSE)
  }

  # Okay, now check if x is newer than y.  This may not need any additional
  # isTRUE() wrapping -- should always be TRUE/FASE
  x_time > y_time
}

# Returns POSIXct
new_as_time <- function(x, must_exist = FALSE) {
  if (length(x) != 1) {
    stop("x must be length of 1")
  }

  if (inherits(x, c("Date", "POSIXt"))) {
    return(as.POSIXct(x))
  }

  if (!is.character(x)) {
    stop("x must be a date/time or file path", call. = FALSE)
  }

  if (isTRUE(must_exist) && !file.exists(x)) {
    stop("File does not exist: ", x, call. = FALSE)
  }

  file.mtime(x)
}
