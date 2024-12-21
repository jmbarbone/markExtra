verbose_message <- function(..., call = NULL) {
  cond <- structure(
    list(message = .makeMessage(..., appendLF = TRUE), call = call),
    class = c("verbose_message", "message", "condition")
  )

  if (getOption("verbose")) {
    message(cond)
  }

  invisible()
}
