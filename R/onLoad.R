.onLoad <- function(libname, pkgname) {
  if ( "package:rJava" %in% search() ) {
    rJava::.jpackage(pkgname, lib.loc=libname)
  }
}

