#' Test a git repository
#'
#' @param x A project directory
#' @param ... Additional arguments passed to test_fun
#' @param branch The branch to copy from the repository (default: "master")
#' @param test_fun A function to apply for testing (default: "devtools::test");
#'   Note: the copied directory for testing is passed as the first argument
#'
#' @export

test_git_repo <- function(x, ..., branch = "master", test_fun = devtools::test) {
  require_namespace("devtools")

  stopifnot(
    "No git directory detected" = dir.exists(jordan::file_path(x, ".git")),
    "No .Rproj found in project_dir" = file.exists(jordan::file_path(x, ".Rproj"))
    )

  temp_dir <- jordan::file_path(
    tempdir(check = TRUE),
    "test-git-repo",
    basename(x)
  )

  on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE, force = TRUE)
  }

  cmd <- sprintf(
    " clone --branch %s --depth=1 %s %s",
    branch,
    get_project_git_url(x),
    temp_dir
  )

  system2("git", cmd)

  FUN <- match.fun(test_fun)
  FUN(temp_dir, ...)
}

get_project_git_url <- function(path) {
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(path)
  system2("git", "remote get-url origin", stdout = TRUE)
}
