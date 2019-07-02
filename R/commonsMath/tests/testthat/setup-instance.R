skipall <- if (requireNamespace("rscala", quietly = TRUE)) {
  cat("Starting instance. ********************************\n")
  library(rscala)
  s <- scala("commonsMath")
  FALSE
} else {
  cat("rscala is not installed. ********************************\n")
  TRUE
}

