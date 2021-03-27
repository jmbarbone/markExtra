#' Start up functions
#'
#' Functions that try to assist with start ups
#'
#' @details
#' These functions can be used to create an environment for startup.
#'
#' @section Start up:
#' For more information on start up:
#'   * https://rstats.wtf/r-startup.html
#'   * `?Startup`
#'
#' @param env The environment to save objects
#' @name startup_funs
#' @family startup_utils
NULL


#' @export
#' @rdname startup_funs
.LoadFunctionsFromJordan <- function(env = parent.frame()) {
  e <- new.env()
  e$op <- options()
  r_profile_env <- new.env(parent = env)
  j <- asNamespace("jordanExtra")
  sf <- get("startup_funs", envir = j)

  for (f in sf) {
    fun <- get(f, envir = j)
    assign(f, fun, envir = r_profile_env)
  }

  assign(".pe", e, envir = r_profile_env)
  assign(".ResetOptions", .ResetOptions, envir = r_profile_env)

  FUN <- "base" %colons% "attach"
  cat(crayon::cyan("jordan:rprofile"), "will be attach()'d\n")
  FUN(r_profile_env, name = "jordan:rprofile", warn.conflicts = FALSE)
  invisible(NULL)
}

#' @param keep_prompt Logical, if `FALSE` will not reset the prompt
#' @export
#' @rdname startup_funs
.ResetOptions <- function(keep_prompt = TRUE) {
  # Resets options
  # Calls the created .pe environment made in .LoadFunctionsFromJordan()
  # As far as I can tell, this has to be created inside the function to work
  #   properly.  I mean, I could always be wrong and not be good enough?
  op <- get("op", envir = get(".pe", pos = "jordan:rprofile"))

  if (keep_prompt) {
    op$prompt <- jordanExtra::.GitBranchPrompt()
  }

  on.exit(options(op), add = TRUE)
  invisible(NULL)
}

#' @export
#' @rdname startup_funs
.RemoveAll <- function(env = parent.frame()) {
  rm(list = ls(all.names = TRUE, envir = env, sorted = FALSE), envir = env)
}

#' @export
#' @rdname startup_funs
startup_funs <- c(
  '.SendAttachedPackagesToREnviron',
  '.RemoveAttachedPackages',
  '.Reload',
  '.Restart',
  '.GitBranchPrompt',
  '.NiceMessage',
  '.LoadPipe',
  '.CharacterIndex',
  '.RemoveAll',
  NULL
)


# Accessory ---------------------------------------------------------------

#' Manage attached packages
#'
#' Clears out attached packages and properly loads
#'
#' @param attached A character vector of packages - if NULL will find packages
#'
#' @export
#' @family startup_utils
#' @name attached_packages
.SendAttachedPackagesToREnviron <- function() {
  attached <- grep("^package[:]", search(), value = TRUE)
  attached2 <- rev(gsub("^package[:]", "", attached))
  attached2 <- setdiff(attached2, .default_packages)

  file <- ".Renviron"
  tag <- jtag()

  # If file exists, see if we need to remove the previous statement
  fe <- file.exists(file)

  if (fe) {
    remove_tag_and_save(file, tag, warn = FALSE)
  }

  pkgs <- collapse0(c(.default_packages, attached2), sep = ",")
  line <- sprintf("%s\nR_DEFAULT_PACKAGES='%s'\n", tag, pkgs)
  cat(line, sep = "", file = file, append = fe)

  .RemoveAttachedPackages()
}


#' @export
#' @rdname attached_packages
.RemoveAttachedPackages <- function(attached = NULL) {
  if (is.null(attached)) {
    attached <- grep("^package[:]", search(), value = TRUE)
  } else {
    bad <- grep("^package[:]", attached, invert = TRUE)
    attached[bad] <- paste0("package:", attached[bad])
  }

  attached <- setdiff(attached, rev(names(.default_packages)))

  for (a in attached) {
    tryCatch({
      unloadNamespace(a)
      detach(a, character.only = TRUE)
    },
      error = function(e) {
        warning("`", a, "` was not found in `detach()`")
      }
    )
  }
}


.default_packages <- c("base", "datasets", "utils", "grDevices", "graphics", "stats", "methods")
names(.default_packages) <- paste0("package:", .default_packages)

#' Reload
#'
#' Reloads your session
#'
#' @param remove_objects Logical, if `TRUE` will remove all objects found
#' @param loud Logical, if `TRUE` will present a message on removed objects
#'
#' @export
#' @family startup_utils
#' @name Reload
.Reload <- function(remove_objects = TRUE, loud = FALSE, env = parent.frame()) {
  cat(crayon::cyan("\nPreparing Restart ...\n"))
  objs <- ls(envir = .GlobalEnv, all.names = TRUE, sorted = FALSE)

  if (remove_objects) {
    if (loud & length(objs) > 0L) {
      message("Removing all objects in the Global Environment:\n",
        collapse0(objs, sep = " ... "))
    }
    rm(list = objs, envir = .GlobalEnv)
  }

  .RemoveAttachedPackages()
  .Restart()
}

#' @export
#' @rdname Reload
.Restart <- function() {
  ("jordan" %colons% "rn_soft")("rstudioapi")
  on.exit(unloadNamespace("jordanExtra"), add = TRUE)
  .RemoveAll()
  invisible(rstudioapi::restartSession("jordanExtra::.CleanUp()"))
}

#' @export
#' @rdname Reload
.CleanUp <- function() {
  while (sum(inds <- search() %in% "jordan:rprofile") > 1) {
    # Remove last first
    detach(pos = max(which(inds)))
  }
}

#' @rdname startup_funs
#' @export
.GitBranchPrompt <- function() {
  ("jordan" %colons% "rn_soft")("prompt")
  branch <- prompt::prompt_git()

  if (branch != getOption("prompt", "> ") & branch != "> ") {
    branch_prompt <-  paste0("[", sub(" >", "] >", branch))
    prompt::set_prompt(branch_prompt)
  }
}


#' Adds a nice message to the start
#'
#' @param x A vector of integers to select from.  If the package isn't available
#'   another random message is generated -- or not
#'
#' @family startup_utils
#' @export
.NiceMessage <- function(x = 1:2) {
  if (length(x) == 0L) return(invisible())
  x <- as.integer(x)

  switch(
    sample(x, 1),
    if (("jordan" %colons% "rn")("praise")) cat_praise() else .NiceMessage(x[-1L]),
    if (("jordan" %colons% "rn")("fortunes")) cat_fortune() else .NiceMessage(x[-2L])
  )
}

cat_praise <- function() {
  cat("\n", crayon::yellow(praise::praise()), "\n\n")
}

cat_fortune <- function() {
  # setting width high as it is adjusted later
  x <- paste(utils::capture.output(fortunes::fortune(width = 1000)))
  cat(crayon::yellow(x), sep = "\n")
}

#' @rdname startup_funs
#' @export
.LoadPipe <- function(env = parent.frame()) {
  if (!"%>%" %in% ls(envir = env)) {
    assign("%>%", magrittr::`%>%`, envir = env, inherits = TRUE)
  }
}



#' Character index
#'
#' Splits a vector into each character and shows the index
#'
#' @param x A character vector
#'
#' @references
#' Inspired by: https://www.r-bloggers.com/2020/12/little-helpers-character-index-counter/
#'
#' @examples
#' x <- c("Split this apart", "and count each character.")
#' .CharacterIndex(x)
#'
#' @export
.CharacterIndex <- function(x = read_clipboard()) {
  if (!is.vector(x)) {
    stop("x must be a vector", call. = FALSE)
  }
  x <- as.character(x)
  lapply(x, function(x) {
    if (length(x) == 0) {
      return(NA_real_)
    }

    nm <- chr_split(x)
    set_names0(1:length(nm), nm)
  })
}

# Environment -------------------------------------------------------------

.startup_env_file <- function() {
  x <- file_path(Sys.getenv("R_USER"), "startup_files/jordan_startup_env.rds")
  dir.create(dirname(x), recursive = TRUE, showWarnings = FALSE)
  x
}

.set_startup_env <- function() {
  e <- new.env(parent = baseenv())
  e$op <- options()
  f <- .startup_env_file()
  if (file.exists(f)) file.remove(f)
  con <- file(f)
  suppressWarnings(saveRDS(e, file = con))

  on.exit(close(con))
  invisible(e)
}

.remove_startup_env <- function() {
  op <- .getstartup_env()$op

  on.exit(options(op))
  file.remove(.startup_env_file())
  invisible()
}

.getstartup_env <- function() {
  readRDS(file = .startup_env_file())
}


# FUNS --------------------------------------------------------------------

remove_tag_and_save <- function(file, tag, warn = TRUE) {
  # Finds where tag is in file and removes the line and the line after
  # Assumes that the 'tag' is from this package and starts with
  #   '@jordan_'
  # setting "tag" to NULL will remove any instances
  x <- readLines(file)

  tag0 <- if (is.null(tag)) {
    jtag("")
  } else {
    jtag(tag)
  }

  pattern <- paste0("^(", tag0, ")")
  line <- grep(pattern, x, fixed = FALSE)

  if (length(line) == 0L) {
    if (warn) {
      warning("Tag `", tag0, "` not found in ", file, call. = FALSE)
    }
    return(invisible())
  }

  # Remove line of tag and next line
  out <- x[-c(line, line + 1L)]
  out <- collapse0(out, sep = "\n")
  out <- gsub("\n?$", "\n", out)
  writeLines(out, file)
}

jtag <- function(x = NULL) {
  # Creates a tag using the system name
  if (is.null(x)) {
    x <- as.character(sys.call(1L))
    # Remove package name
    x <- sub("^.*[:]", "", x)

    if (no_length(x)) {
      stop("x has a length of 0", call. = FALSE)
    }
  }

  if (!is.character(x)) {
    stop("x must be a character", call. = FALSE)
  }

  if (!grepl("^# @jordan ", x)) {
    x <- paste0("# @jordan ", x)
  }

  x
}
