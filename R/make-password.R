#' Generate a random password
#'
#' Make a password and send to the clipboard with [mark::write_clipboard()]
#'
#' @param n The character length desired (default: `16L`)
#' @param specials A single string of characters other than all letters (from
#'   `LETTERS` and `letters`) and numbers (`0:9`)
#' @param silent if `FALSE` will call `cat()` on the password
#'
#' @returns `NULL`, invisible
#' @export
make_password <- function(n = 16L, specials = "!@#$%^&*_+-=?", silent = TRUE) {
  chars <- c(letters, LETTERS, 0:9, mark::chr_split(specials))
  pass <- collapse(sample(chars, n, replace = TRUE))
  message("Copying to clipboard")
  try(mark::write_clipboard(pass))
  if (!silent) cat(pass, "\n")
  invisible(NULL)
}
