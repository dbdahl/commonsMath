rscalaTranscompileHeader <- c("import org.ddahl.commonsmath.Transcompile._")

rscalaTranscompileSubstitute <- function(x) {
  if ( x %in% c("gamma","lgamma","beta","lbeta","factorial","lfactorial","choose","lchoose","stirlingS2","sample",
                "runif",
                "dnorm","pnorm","qnorm","rnorm",
                "dgamma","pgamma","qgamma","rgamma",
                "dbeta","pbeta","qbeta","rbeta",
                NULL) ) paste0("_",x)
  else NULL
}

