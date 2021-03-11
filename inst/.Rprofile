
# ~/.Rprofile -------------------------------------------------------------
# Jordan's personal .Rprofile
# file.copy(system.file(".Rprofile", package = "jordanExtra"), "~/.Rprofile")

# Other option setting
# These should be relatively safe and not case any differences in processes

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

  usethis.description = list(
    `Authors@R` = 'person("Jordan Mark", "Barbone", email = "jmbarbone@gmail.com", role = c("aut", "cre"),
                          comment = c(ORCID = "0000-0001-9788-3628"))',
    License = "MIT + file LICENSE",
    Language =  "en-US"
  ),

  # if FALSE will not activate ~/.Rprofile
  jordan.rprofile = TRUE
)

# Load my functions
if (interactive() & isTRUE(.rprofile$op$jordan.rprofile)) {
  cat(crayon::cyan("Sourcing ~/.Rprofile...\n"))
  jordan::.LoadFunctionsFromJordan()
  .NiceMessage()
  .LoadPipe()
  .GitBranchPrompt()
}

options(.rprofile$op[!names(.rprofile$op) %in% names(options())])
.rprofile$file <- system.file(".Rprofile", package = "jordanExtra")
.rprofile$exists <- .rprofile$file != ""
.rprofile$local <- tryCatch(readLines("~/.Rprofile", ok = FALSE), error = function(e) NULL)

remove(.rprofile)
