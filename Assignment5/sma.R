# sma.R - Simple Moving Average (base R only, no libraries)
# Author: Harjit Gill | BDA400 Assignment 5
#
# sma(data, period)
# Returns length(data) - period + 1 values: the mean of every run of `period`
# consecutive points. The result is not padded, as in the pseudocode.
# Method: vapply() over the start position of every window.

sma <- function(data, period) {
  if (!is.numeric(data)) stop("data must be a numeric vector", call. = FALSE)
  if (length(period) != 1 || !is.numeric(period) || is.na(period) || period < 1 || period %% 1 != 0) {
    stop("period must be a single positive whole number", call. = FALSE)
  }

  # Check if the length of data is less than the specified period
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period", call. = FALSE)
  }

  # Sum each window of `period` points on its own (a missing value then only
  # affects the windows that contain it)
  starts <- seq_len(length(data) - period + 1)
  window_sums <- vapply(starts, function(i) sum(data[i:(i + period - 1)]), numeric(1))

  return(window_sums / period)
}
