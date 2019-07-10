## This is a slightly modified version of a function of the same name from the
## rscala package.  It is copied here to avoid a dependency on the rscala
## package.

#' @importFrom utils download.file
#' 
scalaDevelDownloadJARs <- function(description, scalaMajorVersion="", prefix="https://search.maven.org/remotecontent?filepath=") {
  if ( identical(Sys.getenv("R_INSTALL_PKG"),"") ) {
    if ( interactive() ) message("This function only takes effect during package installation.")
    return(invisible())
  }
  if ( ( ! is.vector(scalaMajorVersion) ) || ( ! is.character(scalaMajorVersion) ) || ( length(scalaMajorVersion) != 1L ) ) {
    stop("Unexpected value for 'scalaMajorVersion'.")
  }
  destDir <- file.path(Sys.getenv("R_PACKAGE_DIR"), "java")
  if ( nchar(scalaMajorVersion) > 0 ) destDir <- file.path(destDir, paste0("scala-", scalaMajorVersion))
  dir.create(destDir, FALSE, TRUE)
  for ( w in description ) {
    cells    <- strsplit(w, ":", fixed=TRUE)[[1]]
    group    <- gsub("\\.", "/", cells[1])
    artifact <- cells[2]
    version  <- cells[3]
    jar <- sprintf("%s%s/%s/%s/%s.jar",prefix,group,artifact,version,paste(artifact,version,sep="-"))
    download.file(jar, file.path(destDir, basename(jar)), mode="wb")
  }
}

