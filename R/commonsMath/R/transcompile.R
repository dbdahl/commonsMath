rscalaTranscompileHeader <- c("import org.ddahl.commonsmath.Transcompile._")

rscalaTranscompileSubstitute <- function(x) {
  if ( x %in% c("gamma","lgamma","beta","lbeta","factorial","lfactorial","choose","lchoose",
                "sample",
                "dunif","punif","qunif","runif",
                "dnorm","pnorm","qnorm","rnorm",
                "dgamma","pgamma","qgamma","rgamma",
                "dexp","pexp","qexp","rexp",
                "dbeta","pbeta","qbeta","rbeta",
                "dt","pt","qt","rt",
                "dchisq","pchisq","qchisq","rchisq",
                "df","pf","qf","rf",
                "dgeom","pgeom","qgeom","rgeom",
                "dbinom","pbinom","qbinom","rbinom",
                "dnbinom","pnbinom","qnbinom","rnbinom",
                "dpois","ppois","qpois","rpois",
                NULL) ) paste0("_",x)
  else NULL
}

