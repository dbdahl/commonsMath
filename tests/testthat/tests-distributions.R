context("distributions")

# skip("distributions")

myExpect <- function(f,args=list(),identical=TRUE, tolerance=sqrt(.Machine$double.eps)) {
  g <- s^f
  h <- attr(g,"rscalaReferenceEnvironment")[["original"]]
  if ( ! identical(f,h) ) stop("Not identical.")
  trans <-  do.call(g,args)
  native <- do.call(f,args)
  if ( identical ) expect_identical(trans, native)
  else expect_equal(trans, native, tolerance=tolerance)
}

n <- 100

test_that("normal distribution",{

  myExpect(function(x=scalaType("D1")) { dnorm(x,-3.0,2) }, list(0.25), FALSE)
  myExpect(function(x=scalaType("D1")) { pnorm(x,0.2,5) }, list(0.25), FALSE)
  myExpect(function(x=scalaType("D1")) { qnorm(x,0.3,3) }, list(0.25), FALSE)
  myExpect(function(n=scalaType("D0")) { mean(rnorm(n,1.0,2.0)) }, list(n), FALSE, tolerance=4*sqrt(2/n))
  
})
