# linreg.R - Simple Linear Regression (base R only, no libraries)
# Author: Harjit Gill | BDA400 Assignment 5
#
# linreg(regressionSource, regressionLength, regressionOffset)
# Window from the pseudocode:
#   start = max(1, n - regressionLength + regressionOffset)
#   end   = min(n, n - regressionOffset)
# x = 1, 2, 3, ... inside the window. Least squares with the closed-form
#   slope = sum((x - mean x) * (y - mean y)) / sum((x - mean x)^2)
#   intercept = mean y - slope * mean x
# Returns list(slope, intercept, predicted_values).

linreg <- function(regressionSource, regressionLength, regressionOffset) {
  if (!is.numeric(regressionSource)) {
    stop("regressionSource must be a numeric vector", call. = FALSE)
  }
  if (length(regressionLength) != 1 || length(regressionOffset) != 1 ||
      !is.numeric(regressionLength) || !is.numeric(regressionOffset) ||
      regressionOffset < 0) {
    stop("regressionLength and regressionOffset must be single numbers (offset >= 0)", call. = FALSE)
  }

  n <- length(regressionSource)
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource",
         call. = FALSE)
  }
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength", call. = FALSE)
  }

  first <- max(1, n - regressionLength + regressionOffset)
  last  <- min(n, n - regressionOffset)
  y <- regressionSource[first:last]
  if (length(y) < 2) stop("At least two data points are needed for a regression", call. = FALSE)

  x <- seq_along(y)
  x_dev <- x - mean(x)
  slope <- sum(x_dev * (y - mean(y))) / sum(x_dev^2)
  intercept <- mean(y) - slope * mean(x)

  return(list(slope = slope, intercept = intercept,
              predicted_values = intercept + slope * x))
}
