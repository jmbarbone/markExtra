passcode_env <- new.env()
passcode_env$failures <- 0L
passcode_env$fail <- function() {
  passcode_env$failures <- passcode_env$failures + 1L
  invisible(passcode_env)
}
passcode_env$reset <- function() {
  assign("failures", 0L, passcode_env)
  invisible(passcode_env)
}

#' passcode
#'
#' Generate a passcode
#'
#' See `?codename::codename` for more details
#'
#' @param len length of passcode
#' @param sep separator between words
#' @param one,two Type of words to use (see `?codename::codename` for `type`)
#' @param numbers Number of digits to append
#' @return A passcode, as a string
passcode <- function(
    len = 14:16,
    sep = "_",
    one = c("adjectives", "colors"),
    two = c("animals", "nouns"),
    numbers = len %/% 4L
) {
  fuj::require_namespace("codename")
  reset_fail <- FALSE
  on.exit(if (reset_fail) passcode_env$reset(), add = TRUE)

  restart <- function() {
    e <- sys.call(-1)
    p <- parent.frame(3)
    passcode_env$fail()

    if (passcode_env$failures > 100L) {
      passcode_env$reset()
      stop("Too many failures")
    }

    verboseMessage(
      "restarting passcode() due to failure ... ",
      passcode_env$failures,
      call. = e
    )
    eval(e, p)
  }

  len <- as.integer(len)
  len <- unique(c(min(len), max(len)))
  if (length(len) > 1L) {
    len <- sample(len, 1L)
  }

  nbase <- len - numbers - nchar(sep)

  numbers <- as.integer(numbers)
  if (numbers >= 1) {
    nums <- sample.int(10L^numbers - 1L, 1L)
    nums <- formatC(nums, format = "fg", width = numbers, flag = "0")
  } else {
    nums <- ""
  }

  one <- match.arg(one)
  two <- match.arg(two)
  one <- getExportedValue("codename", one)[["value"]]
  two <- getExportedValue("codename", two)[["value"]]
  one <- one[nchar(one) <= (nbase - 2L)]
  two <- two[nchar(two) <= (nbase - 2L)]

  nc_one <- nchar(one)
  nc_two <- nchar(two)

  ok <-
    nc_one <= (nbase - min(nc_two)) &
    nc_one >= (nbase - max(nc_two))

  if (!any(ok)) {
    return(restart())
  }

  one <- sample(one[ok], 1L)
  ok <- nchar(two) == (nbase - nchar(one))

  if (!any(ok)) {
    return(restart())
  }

  two <- sample(two[ok], 1L)
  reset_fail <- TRUE
  paste0(one, sep, two, nums)
}
