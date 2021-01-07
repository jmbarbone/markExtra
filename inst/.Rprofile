
# ~/.Rprofile -------------------------------------------------------------
# Jordan's personal .Rprofile
# file.copy(system.file(".Rprofile", package = "jordanExtra"), "~/.Rprofile")

# Other option setting
# These should be relatively safe and not case any differences in processes

# You can turn off with Sys.setenv(JORDAN_RPROFILE = FALSE)

.rprofile <- new.env()

.rprofile$op <- list(
  tidyverse.quiet = TRUE,
  devtools.name = "Jordan Mark Barbone",

  # For jordan::use_author()
  jordan.author = list(
    given = "Jordan Mark",
    family = "Barbone",
    role = c("aut", "cph", "cre"),
    email = "jmbarbone@gmail.com",
    comment = c(ORCID = "0000-0001-9788-3628")
  ),

  # if FALSE will not activate ~/.Rprofile
  jordan.rprofile = TRUE,

  # if TRUE will remove the .rprofile object
  jordan_remove_.rprofile = FALSE
)

options(.rprofile$op[!names(.rprofile$op) %in% names(options())])

.rprofile$file <- system.file(".Rprofile", package = "jordanExtra")
.rprofile$exists <- .rprofile$file != ""
.rprofile$local <- tryCatch(readLines("~/.Rprofile", ok = FALSE),
                            error = function(e) NULL)

# Load my functions
if (interactive() & .rprofile$op$jordan.rprofile) {
  cat(crayon::cyan("Sourcing ~/.Rprofile...\n"))
  jordan::.LoadFunctionsFromJordan()
  .RunDefaultFunctionsFromJordan()
  .git_branch_prompt()
  cat(crayon::cyan("... Done!\n"))
}

if (.rprofile$op$jordan_remove_.rprofile) {
  remove(.rprofile)
}
