#' Print a model as an equation
#'
#' @param mod A model, something which `stats::coef()` can extracted a named
#'  vector
#' @param FUN A function to apply to the values of `stats::coef(mod)`, for
#'   formatting; default formats to 2 significant figures with trailing zeros
#' @param ... Additional arguments passed to `FUN`
#'
#' @examples
#' mod <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
#' coef_equation(mod)
#' # "2.2 + Sepal.Width \* 0.60 + Petal.Length \* 0.47"
#' @export

coef_equation <- function(mod, FUN = sf2, ...) {
  UseMethod("coef_equation", mod)
}

#' @export
coef_equation.default <- function(mod, FUN = sf2, ...) {
  stop("There is no method for class: ", class(mod), call. = FALSE)
}

#' @export
coef_equation.lm <- function(mod, FUN = sf2, ...) {
  values <- stats::coef(mod)
  nm <- names(values)
  if (is.null(nm)) {
    stop("Cannot extract names from `stats::coef(mod)`", call. = FALSE)
  }
  out <- paste(nm, FUN(values, ...), sep = " * ", collapse = " + ")
  sub("^[(]Intercept[)]\\s[*]\\s+", "", out)
}

sf2 <- function(x) {
  x <- formatC(
    x,
    digits         = 2,
    format         = "fg",
    flag           = "#",
    drop0trailing  = FALSE,
    preserve.width = "individual",
    width          = NULL
  )
  sub("\\.$", "", x)
}
