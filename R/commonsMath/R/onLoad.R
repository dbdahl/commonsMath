.onLoad <- function(libname, pkgname) {
  if ( isNamespaceLoaded("rJava") ) {
    rJava::.jpackage(pkgname, lib.loc=libname)
  }
}
