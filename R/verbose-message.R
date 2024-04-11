verboseMessage <- function(..., call. = NULL) {
  cond <- structure(
    list(message = .makeMessage(..., appendLF = TRUE), call = call.),
    class = c("verboseMessage", "message", "condition")
  )

  if (getOption("verbose")) {
    message(cond)
  }

  invisible()
}
