rstdnorm <- function() {
  rng <- .jnew("org.apache.commons.math3.random.RandomDataGenerator")
  rng$nextGaussian(0.0,1.0)
}

