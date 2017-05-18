.onLoad <- function(libname, pkgname) {
  if ( "rJava" %in% loadedNamespaces() ) {
    rJava::.jpackage(pkgname, lib.loc=libname)
  }
}

