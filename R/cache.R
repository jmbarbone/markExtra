cache_env <- new.env()
cache_time_env <- new.env()

#' cache an object
#'
#' cache an R object
#'
#' @param name Name of the object
#' @param expr Expression to evaluate
#' @param overwrite If `TRUE` saved over current objects
#' @param time Either time (seconds) for how long to cache the object(s) or the
#'   time as a `POSIXt` object
#' @param env Environment to load object into if found
#' @returns Result of `expr`, or cached result
#' @examples
#' res <- .POSIXct(0)
#' for (i in 1:6) {
#'   cache("foo", Sys.time(), time = 1)
#'   Sys.sleep(0.5)
#'   res[i] <- foo
#' }
#' res
#' @export

cache <- function(name, expr, overwrite = FALSE, time = 3600, env = parent.frame()) {
  stopifnot(nzchar(name))

  if (!inherits(time, "POSIXt")) {
    time <- Sys.time() + time
  }

  found <- get0(name, cache_env)
  expired <- isTRUE(get0(name, cache_time_env) < Sys.time())

  if (overwrite | expired | is.null(found)) {
    res <- force(expr)
    assign(name, res, cache_env)
    assign(name, time, cache_time_env)
  } else {
    res <- found
  }

  assign(name, res, env)
  res
}
