rscalaTranscompileHeader <- c("import org.ddahl.commonsmath.Transcompile._")

rscalaTranscompileSubstitute <- function(x) {
  if ( x %in% c("gamma","lgamma","beta","lbeta","factorial","lfactorial","choose","lchoose","stirlingS2","dnorm","pnorm","qnorm","rnorm") ) paste0("_",x)
  else NULL
}

