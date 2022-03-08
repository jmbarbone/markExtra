#' Test a git repository
#'
#' @param x A project directory
#' @param ... Additional arguments passed to test_fun
#' @param branch The branch to copy from the repository (default: "master")
#' @param test_fun A function to apply for testing (default: "devtools::test");
#'   Note: the copied directory for testing is passed as the first argument
#' @param options A named list of `options()` to temporarily set
#'
#' @export

test_git_repo <- function(x, ..., branch = "master", test_fun = devtools::test, options = NULL) {
  require_namespace("devtools")
  op <- options()
  on.exit(options(op), add = TRUE)
  options(options)

  x <- norm_path(x, check = TRUE)

  if (!has_git(x)) {
    stop("N git directory detected")
  }

  if (!has_rproj(x)) {
    stop("No .Rproj found in project_dir")
  }

  temp_dir <- mark::file_path(
    tempdir(check = TRUE),
    "test-git-repo",
    basename(x)
  )

  on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE, force = TRUE)
  }

  args <- sprintf(
    " clone --branch %s --depth=1 %s %s",
    branch,
    get_project_git_url(x),
    temp_dir
  )

  system2("git", args)
  (match.fun(test_fun))(temp_dir, ...)
}

# TODO Maybe add a FAIL argument for mark::norm_path?
# Passed to normalizePath(., mustWork = fail)
# norm_path(x, check = fail, remove = check, fail = FALSE)

get_project_git_url <- function(path) {
  git_folder <- tryCatch(
    file_path(path, ".git", check = TRUE),
    warning = function(e) {
      stop(e$message, call. = FALSE)
    }
  )
  args <- sprintf(' --git-dir %s remote get-url origin', git_folder)
  system2("git", args, stdout = TRUE)
}

has_git <- function(x) {
  dir.exists(mark::file_path(x, ".git"))
}

has_rproj <- function(x) {
  file.exists(file_path(x, paste0(basename(x), ".Rproj")))
}
