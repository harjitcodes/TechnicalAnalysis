# rsi.R - Relative Strength Index (base R only, no libraries)
# Author: Harjit Gill | BDA400 Assignment 5
#
# rsi(data, period)
# Returns a vector as long as `data`; the first `period` values are NA.
# Wilder smoothing starts at position period + 1:
#   average = (previous average * (period - 1) + latest move) / period
# RSI = 100 - 100 / (1 + average gain / average loss).
# No losses -> 100; no gains and no losses -> 50. With fewer than period + 1
# points the result is all NA, with a warning.

rsi <- function(data, period) {
  if (!is.numeric(data)) stop("data must be a numeric vector", call. = FALSE)
  if (length(period) != 1 || !is.numeric(period) || is.na(period) || period < 1 || period %% 1 != 0) {
    stop("period must be a single positive whole number", call. = FALSE)
  }

  n <- length(data)
  if (n < period + 1) {
    warning("Need at least period + 1 data points; returning NA", call. = FALSE)
    return(rep(NA_real_, n))
  }

  change <- diff(data)                             # n - 1 price changes
  up   <- ifelse(change > 0, change, 0)
  down <- ifelse(change < 0, -change, 0)

  gain_avg <- sum(up[1:period]) / period
  loss_avg <- sum(down[1:period]) / period

  out <- rep(NA_real_, n)
  for (k in period:(n - 1)) {                      # change k leads to position k + 1
    gain_avg <- (gain_avg * (period - 1) + up[k]) / period
    loss_avg <- (loss_avg * (period - 1) + down[k]) / period
    out[k + 1] <- if (gain_avg == 0 && loss_avg == 0) {
      50
    } else if (loss_avg == 0) {
      100
    } else {
      100 - 100 / (1 + gain_avg / loss_avg)
    }
  }

  return(out)
}
