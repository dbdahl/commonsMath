context("transcompile")

if ( skipall ) skip("transcompile")

myExpect <- function(f,args=list(),identical=TRUE, tolerance=sqrt(.Machine$double.eps)) {
  g <- s^f
  h <- attr(g,"rscalaReferenceEnvironment")[["original"]]
  if ( ! identical(f,h) ) stop("Not identical.")
  trans <-  do.call(g,args)
  native <- do.call(f,args)
  if ( identical ) expect_identical(trans, native)
  else expect_equal(trans, native, tolerance=tolerance)
}

myExpectLength <- function(f,args=list()) {
  g <- s^f
  h <- attr(g,"rscalaReferenceEnvironment")[["original"]]
  if ( ! identical(f,h) ) stop("Not identical.")
  trans <-  do.call(g,args)
  native <- do.call(f,args) 
  expect_identical(length(trans), length(native))
}
  
test_that("sample function",{
  
  myExpectLength(function() sample(c(3,2,1)))
  myExpectLength(function() sample(c(3,2,1),2))
  myExpectLength(function() sample(c(3,2,1),4,replace=TRUE))
  myExpectLength(function() sample(10))
  myExpectLength(function() sample(10,3))
  myExpectLength(function() sample(10,replace=TRUE))
  myExpectLength(function() sample(10,3,replace=TRUE))
  myExpectLength(function() sample(10,3,replace=TRUE,prob=as.numeric(1:10)))
  myExpectLength(function() sample(10,3,prob=as.numeric(1:10)))
  myExpectLength(function() sample(10,prob=as.numeric(1:10)))
  myExpectLength(function() sample(c(2,3,4,5,6),2,replace=TRUE,prob=as.numeric(1:5)))
  myExpectLength(function() sample(c(2,3,4,5,6),5,prob=as.numeric(1:5)))
  
})

test_that("special functions",{

  myExpect(function(x=stD0) { gamma(x) }, list(3.72), FALSE)
  myExpect(function(x=stD1) { gamma(x) }, list(3.72), FALSE)
  myExpect(function(x=stD1) { gamma(x) }, list(c(3.72,5)), FALSE)
  myExpect(function(x=stD1) { gamma(x) }, list(c(3L,5L)), FALSE)
  myExpect(function(x=stD0) { lgamma(x) }, list(5.26), FALSE)
  myExpect(function(x=stD1) { lgamma(x) }, list(5.26), FALSE)
  myExpect(function(x=stD1) { lgamma(x) }, list(c(5.26,3)), FALSE)
  myExpect(function(x=stD1) { lgamma(x) }, list(c(5L,3L)), FALSE)
  myExpect(function(x=stD0) { factorial(x) }, list(3.72), FALSE)
  myExpect(function(x=stD1) { factorial(x) }, list(3.72), FALSE)
  myExpect(function(x=stD1) { factorial(x) }, list(c(3.72,4)), FALSE)
  myExpect(function(x=stD1) { factorial(x) }, list(c(3L,4L)), FALSE)
  myExpect(function(x=stD0) { lfactorial(x) }, list(5.26), FALSE)
  myExpect(function(x=stD1) { lfactorial(x) }, list(5.26), FALSE)
  myExpect(function(x=stD1) { lfactorial(x) }, list(c(5.26,3)), FALSE)
  myExpect(function(x=stD1) { lfactorial(x) }, list(c(5L,3L)), FALSE)
  myExpect(function(x=stD0,y=stD0) { beta(x,y) }, list(5L,3L), FALSE)
  myExpect(function(x=stI0,y=stI0) { beta(x,y) }, list(5L,3L), FALSE)
  myExpect(function(x=stD1,y=stD1) { beta(x,y) }, list(5L,3L), FALSE)
  myExpect(function(x=stI1,y=stI1) { beta(x,y) }, list(5L,3L), FALSE)
  myExpect(function(x=stI1,y=stD1) { beta(x,y) }, list(5L,3L), FALSE)
  myExpect(function(x=stI1,y=stD1) { beta(x,y) }, list(5L,3L), FALSE)
  myExpect(function(x=stD0,y=stD0) { lbeta(x,y) }, list(5L,3L), FALSE)
  myExpect(function(x=stI0,y=stI0) { lbeta(x,y) }, list(5L,3L), FALSE)
  myExpect(function(x=stD1,y=stD1) { lbeta(x,y) }, list(5L,3L), FALSE)
  myExpect(function(x=stI1,y=stI1) { lbeta(x,y) }, list(5L,3L), FALSE)
  myExpect(function(x=stI1,y=stD1) { lbeta(x,y) }, list(5L,3L), FALSE)
  myExpect(function(x=stI1,y=stD1) { lbeta(x,y) }, list(5L,3L), FALSE)
  myExpect(function(x=stD0,y=stD0) { choose(x,y) }, list(5L,3L), FALSE)
  myExpect(function(x=stI0,y=stI0) { choose(x,y) }, list(5L,3L), FALSE)
  myExpect(function(x=stD1,y=stD1) { choose(x,y) }, list(5L,3L), FALSE)
  myExpect(function(x=stI1,y=stI1) { choose(x,y) }, list(5L,3L), FALSE)
  myExpect(function(x=stI1,y=stD1) { choose(x,y) }, list(5L,3L), FALSE)
  myExpect(function(x=stI1,y=stD1) { choose(x,y) }, list(5L,3L), FALSE)
  myExpect(function(x=stD0,y=stD0) { lchoose(x,y) }, list(5L,3L), FALSE)
  myExpect(function(x=stI0,y=stI0) { lchoose(x,y) }, list(5L,3L), FALSE)
  myExpect(function(x=stD1,y=stD1) { lchoose(x,y) }, list(5L,3L), FALSE)
  myExpect(function(x=stI1,y=stI1) { lchoose(x,y) }, list(5L,3L), FALSE)
  myExpect(function(x=stI1,y=stD1) { lchoose(x,y) }, list(5L,3L), FALSE)
  myExpect(function(x=stI1,y=stD1) { lchoose(x,y) }, list(5L,3L), FALSE)

})

n  <- 1000
nStdDev <- 4

test_that("uniform distribution",{

  myExpect(function(x=stD1) { dunif(x,-3.0,2) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { dunif(x,-3.0,2,log=TRUE) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { punif(x,0.2,5) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { punif(x,0.2,5,lower.tail=FALSE) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { qunif(x,0.3,3) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { qunif(x,0.3,3,lower.tail=FALSE) }, list(0.25), FALSE)
  myExpect(function(n=stD0) { mean(runif(n,1.0,2.0)) }, list(n), FALSE, tolerance=nStdDev*sqrt(1/12)/sqrt(n))
  
})

test_that("normal distribution",{

  myExpect(function(x=stD1) { dnorm(x,-3.0,2) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { dnorm(x,-3.0,2,log=TRUE) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { pnorm(x,0.2,5) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { pnorm(x,0.2,5,lower.tail=FALSE) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { qnorm(x,0.3,3) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { qnorm(x,0.3,3,lower.tail=FALSE) }, list(0.25), FALSE)
  myExpect(function(n=stD0) { mean(rnorm(n,1.0,2.0)) }, list(n), FALSE, tolerance=nStdDev*2/sqrt(n))
  
})

test_that("gamma distribution",{

  myExpect(function(x=stD1) { dgamma(x,3.0,2) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { dgamma(x,3.0,2,log=TRUE) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { pgamma(x,0.2,5) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { pgamma(x,0.2,5,lower.tail=FALSE) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { qgamma(x,0.3,3) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { qgamma(x,0.3,3,lower.tail=FALSE) }, list(0.25), FALSE)
  myExpect(function(n=stD0) { mean(rgamma(n,1.0,2.0)) }, list(n/2), FALSE, tolerance=nStdDev*sqrt((1/4)/n))
  
})

test_that("exponential distribution",{

  myExpect(function(x=stD1) { dexp(x,3.0) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { dexp(x,3.0,log=TRUE) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { pexp(x,0.2) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { pexp(x,0.2,lower.tail=FALSE) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { qexp(x,0.3) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { qexp(x,0.3,lower.tail=FALSE) }, list(0.25), FALSE)
  myExpect(function(n=stD0) { mean(rexp(n,2.0)) }, list(n), FALSE, tolerance=nStdDev*sqrt((1/4)/n))
  
})

test_that("beta distribution",{

  myExpect(function(x=stD1) { dbeta(x,3.0,2) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { dbeta(x,3.0,2,log=TRUE) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { pbeta(x,0.2,5) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { pbeta(x,0.2,5,lower.tail=FALSE) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { qbeta(x,0.3,3) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { qbeta(x,0.3,3,lower.tail=FALSE) }, list(0.25), FALSE)
  myExpect(function(n=stD0) { mean(rbeta(n,3.0,2.0)) }, list(n), FALSE, tolerance=nStdDev*sqrt((1/25)/n))
  
})

test_that("t distribution",{

  myExpect(function(x=stD1) { dt(x,3) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { dt(x,3,0,log=TRUE) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { pt(x,4) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { pt(x,4,0,lower.tail=FALSE) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { qt(x,2) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { qt(x,2,0,lower.tail=FALSE) }, list(0.25), FALSE)
  myExpect(function(n=stD0) { mean(rt(n,3)) }, list(n), FALSE, tolerance=nStdDev*sqrt(3/n))
  
})

test_that("chi squared distribution",{

  myExpect(function(x=stD1) { dchisq(x,3) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { dchisq(x,3,0,log=TRUE) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { pchisq(x,4) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { pchisq(x,4,0,lower.tail=FALSE) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { qchisq(x,2) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { qchisq(x,2,0,lower.tail=FALSE) }, list(0.25), FALSE)
  myExpect(function(n=stD0) { mean(rchisq(n,3)) }, list(n), FALSE, tolerance=nStdDev*sqrt(6/n))
  
})

test_that("F distribution",{

  myExpect(function(x=stD1) { df(x,3,2) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { df(x,3,2,0,log=TRUE) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { pf(x,4,5) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { pf(x,4,5,0,lower.tail=FALSE) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { qf(x,2,3) }, list(0.25), FALSE)
  myExpect(function(x=stD1) { qf(x,2,3,0,lower.tail=FALSE) }, list(0.25), FALSE)
  myExpect(function(n=stD0) { mean(rf(n,3,5)) }, list(n), FALSE, tolerance=nStdDev*sqrt((100/9)/n))
  
})

test_that("geometric distribution",{

  myExpect(function(x=stD1) { dgeom(x,0.73) }, list(3.0), FALSE)
  myExpect(function(x=stD1) { dgeom(x,0.73,log=TRUE) }, list(3.0), FALSE)
  myExpect(function(x=stD1) { pgeom(x,0.32) }, list(5.0), FALSE)
  myExpect(function(x=stD1) { pgeom(x,0.32,lower.tail=FALSE) }, list(5.0), FALSE)
  myExpect(function(x=stD1) { qgeom(x,0.40) }, list(0.59), FALSE)
  myExpect(function(x=stD1) { qgeom(x,0.40,lower.tail=FALSE) }, list(0.59), FALSE)
  myExpect(function(n=stD0) { mean(rgeom(n,0.17)) }, list(n), FALSE, tolerance=nStdDev*sqrt(((1-0.17)/0.17)/n))
  
})

test_that("binomial distribution",{

  myExpect(function(x=stD1) { dbinom(x,10,0.73) }, list(13.0), FALSE)
  myExpect(function(x=stD1) { dbinom(x,10,0.73,log=TRUE) }, list(13.0), FALSE)
  myExpect(function(x=stD1) { pbinom(x,6,0.32) }, list(5.0), FALSE)
  myExpect(function(x=stD1) { pbinom(x,6,0.32,lower.tail=FALSE) }, list(5.0), FALSE)
  myExpect(function(x=stD1) { qbinom(x,8,0.40) }, list(0.59), FALSE)
  myExpect(function(x=stD1) { qbinom(x,8,0.40,lower.tail=FALSE) }, list(0.59), FALSE)
  myExpect(function(n=stD0) { mean(rbinom(n,34,0.17)) }, list(n), FALSE, tolerance=nStdDev*sqrt(34*0.17*(1-0.17)/n))
  
})

test_that("negative binomial distribution",{

  myExpect(function(x=stD1) { dnbinom(x,10,0.73) }, list(13.0), FALSE)
  myExpect(function(x=stD1) { dnbinom(x,10,0.73,log=TRUE) }, list(13.0), FALSE)
  myExpect(function(x=stD1) { pnbinom(x,6,0.32) }, list(5.0), FALSE)
  myExpect(function(x=stD1) { pnbinom(x,6,0.32,lower.tail=FALSE) }, list(5.0), FALSE)
  myExpect(function(x=stD1) { qnbinom(x,8,0.40) }, list(0.59), FALSE)
  myExpect(function(x=stD1) { qnbinom(x,8,0.40,lower.tail=FALSE) }, list(0.59), FALSE)
  myExpect(function(n=stD0) { mean(rnbinom(n,34,0.17)) }, list(n), FALSE, tolerance=nStdDev*sqrt(34*(1-0.17)/0.17^2/n))
  
})

test_that("poisson distribution",{

  myExpect(function(x=stD1) { dpois(x,7.73) }, list(13.0), FALSE)
  myExpect(function(x=stD1) { dpois(x,7.73,log=TRUE) }, list(13.0), FALSE)
  myExpect(function(x=stD1) { ppois(x,2.32) }, list(9.0), FALSE)
  myExpect(function(x=stD1) { ppois(x,2.32,lower.tail=FALSE) }, list(9.0), FALSE)
  myExpect(function(x=stD1) { qpois(x,1.40) }, list(0.59), FALSE)
  myExpect(function(x=stD1) { qpois(x,1.40,lower.tail=FALSE) }, list(0.59), FALSE)
  myExpect(function(n=stD0) { mean(rpois(n,3.5)) }, list(n), FALSE, tolerance=nStdDev*sqrt(3.5/n))
  
})
