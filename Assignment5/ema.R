# ema.R - Exponential Moving Average (base R only, no libraries)
# Author: Harjit Gill | BDA400 Assignment 5
#
# ema(data, period)
# Returns a vector as long as `data`. The first value is the first data point;
# every later value is  alpha * new point + (1 - alpha) * previous EMA,
# with alpha = 2 / (period + 1).

ema <- function(data, period) {
  if (!is.numeric(data)) stop("data must be a numeric vector", call. = FALSE)
  if (length(period) != 1 || !is.numeric(period) || is.na(period) || period < 1 || period %% 1 != 0) {
    stop("period must be a single positive whole number", call. = FALSE)
  }
  if (length(data) == 0) return(numeric(0))

  # Calculate the multiplier for EMA
  alpha <- 2 / (period + 1)

  smoothed <- data
  for (t in seq_along(data)[-1]) {
    smoothed[t] <- alpha * data[t] + (1 - alpha) * smoothed[t - 1]
  }

  return(smoothed)
}
