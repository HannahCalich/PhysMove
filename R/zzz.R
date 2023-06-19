.pkgenv <- new.env(parent=emptyenv())

.onLoad  <- function(libname, pkgname) {
  infomap_req <- requireNamespace("infomapecology", quietly = TRUE)
  .pkgenv[["infomap_req"]] <- infomap_req
}

.onAttach <- function(libname, pkgname) {
  if (!.pkgenv$infomap_req) {
    msg <- paste("To use this package in conjunction with Infomap,",
                 "you must install the infomapecology R package and the standalone Infomap file.",
                 "See the `PhysMove: Space-Use Patterns` vignette for further details.")
    msg <- paste(strwrap(msg), collapse="\n")
    packageStartupMessage(msg)
  }
}
