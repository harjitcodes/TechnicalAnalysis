# stdev.R - Standard Deviation (base R only, no libraries)
# Author: Harjit Gill | BDA400 Assignment 5
#
# stdev(data)
# Returns the POPULATION standard deviation (divides by n, not n - 1), so
# stdev(x) = sd(x) * sqrt((n - 1) / n).
# Steps: mean, distance of each point from the mean, dot product of the distances with themselves, divide by n, square root.

stdev <- function(data) {
  if (!is.numeric(data)) stop("data must be a numeric vector", call. = FALSE)
  n <- length(data)
  if (n == 0) stop("data must contain at least one value", call. = FALSE)

  centre <- sum(data) / n
  centred <- data - centre
  variance <- drop(crossprod(centred)) / n         # sum of squared distances / n

  return(sqrt(variance))
}
