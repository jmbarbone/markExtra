#' Generate a random password
#'
#' Make a password and send to the clipboard with [jordan::write_clipboard()]
#'
#' @param n The character length desired (default: `16L`)
#' @param specials A single string of characters other than all letterrs (from
#'   `LETTERS` and `letters`) and numbers (`0:9`)
#' @param silent if `FALSE` will call `cat()` on the password
#'
#' @returns `NULL`, invisible
#' @export
make_password <- function(n = 16L, specials = "!@#$%^&*_+-=?", silent = TRUE) {
  chars <- c(letters, LETTERS, 0:9, chr_split(specials))
  pass <- collapse0(sample(chars, n, replace = TRUE))
  message("Copying to clipboard")
  tryCatch(write_clipboard(pass),
           error = function(e) {
             warning(e, call = FALSE)
           })
  if (!silent) {
    cat(pass, "\n")
  }
  invisible(NULL)
}
