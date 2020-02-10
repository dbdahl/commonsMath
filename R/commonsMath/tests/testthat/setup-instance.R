skipall <- if (requireNamespace("rscala", quietly = TRUE)) {
  if ( ! isTRUE(tryCatch({rscala::scalaConfig(reconfig="offline")}, error=function(e) TRUE)) ) {
    cat("Starting instance. ********************************\n")
    library(rscala)
    s <- scala("commonsMath")
    FALSE
  } else {
    cat("Scala does not seem to be available. ********************************\n")
    TRUE
  }
} else {
  cat("rscala is not installed. ********************************\n")
  TRUE
}

