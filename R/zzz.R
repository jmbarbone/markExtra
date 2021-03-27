
.onUnload <- function(libpath) {
  if ("jordan:rprofile" %in% search()) {
    detach("jordan:rprofile", character.only = TRUE)
  }
}

.onDetach <- function(libpath) {
  if ("jordan:rprofile" %in% search()) {
    detach("jordan:rprofile", character.only = TRUE)
  }
}
