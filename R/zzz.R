.pkgenv <- new.env(parent=emptyenv())

.onLoad  <- function(libname, pkgname) {
  infomap_req <- requireNamespace("infomapecology", quietly = TRUE)
  .pkgenv[["infomap_req"]] <- infomap_req
}

.onAttach <- function(libname, pkgname) {
  if (!.pkgenv$infomap_req) {
    msg <- paste("Please Note: To use this package in conjunction with Infomap",
                 "you must manually install the infomapecology and emln R packages and the standalone Infomap file.",
                 "See the `PhysMove: Space-Use Patterns` vignette for further details.")
    msg <- paste(strwrap(msg), collapse="\n")
    packageStartupMessage(msg)
  }
}
