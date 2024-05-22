#' Default environment
#'
#' Creates an environment with just default packages
#'
#' @details
#' A new environment is created with the parent of
#'
#' @param key A key for which environments to use
#'
#' @export
default_env <- function(key = "default") {
  e <- new.env(parent = baseenv())
  switch(
    key,
    default = {
      eval({
        substitute({
          requireNamespace("methods")
          requireNamespace("stats")
          requireNamespace("graphics")
          requireNamespace("grDevices")
          requireNamespace("utils")
          requireNamespace("datasets")
        },
        env = e)
      },
      envir = e)
    },
    options = {
      eval({
        substitute({
          # nolint next: object_usage_linter. False positive for 'i'
          for (i in getOption("defaultPackages")) {
            requireNamespace(i)
          }
        },
        env = e)
      },
      envir = e)
    },
    stop(key, " is not a valid key", call. = FALSE)
  )
  e
}
