\name{commonsMath-package}
\alias{commonsMath-package}
\alias{commonsMath}
\docType{package}
\title{
\packageTitle{commonsMath}
}
\description{
\packageDescription{commonsMath}
}
\details{
The DESCRIPTION file:
\packageDESCRIPTION{commonsMath}

See the examples below to see how to access the Apache Commons Math Library using this package.
}
\examples{
\dontshow{
# This makes it so the example code below runs as a user would user it.
# R's package checking loads the commonsMath package before rJava, so
# it commonsMath does ever load its JAR file.
detach("package:commonsMath",unload=TRUE)
}
\dontrun{
################################################
###  How to use commonsMath in an R script.  ###
################################################

# Using the rscala package.
library("rscala")
s <- scala(classpath.packages="commonsMath")
rng1 <- s$.org.apache.commons.math3.random.RandomDataGenerator$new()
rng1$nextGaussian(0.0,1.0)

# Using the rJava package.
library("rJava")
library("commonsMath")  # Make sure you load commonsMath *after* rJava.
rng2 <- .jnew("org.apache.commons.math3.random.RandomDataGenerator")
rng2$nextGaussian(0.0,1.0)

################################################
###  How to use commonsMath in an R package. ###
################################################

# Using the rscala package.
#    The DESCRIPTION has 'Imports: rscala, commonsMath'
#    The NAMESPACE has 'import(rscala)'.
.onLoad <- function(libname, pkgname) {
  .rscalaPackage(pkgname,classpath.packages="commonsMath")
}

rstdnorm <- function() {
  rng1 <- s$.org.apache.commons.math3.random.RandomDataGenerator$new()
  rng1$nextGaussian(0.0,1.0)
}

# Using the rJava package.
#    The DESCRIPTION has 'Imports: rJava, commonsMath'
#    The NAMESPACE has first 'import(rJava)' and then 'import(commonsMath)'.
.onLoad <- function(libname, pkgname) {
  .jpackage(pkgname, lib.loc=libname)
}

rstdnorm <- function() {
  rng2 <- .jnew("org.apache.commons.math3.random.RandomDataGenerator")
  rng2$nextGaussian(0.0,1.0)
}
}
\dontshow{
# Undo the previous dontshow code.
library("commonsMath")
}
}

