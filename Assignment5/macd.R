# macd.R - Moving Average Convergence Divergence (base R only, no libraries)
# Author: Harjit Gill | BDA400 Assignment 5
#
# macd(data, short_period, long_period, signal_period)
# Returns list(macd_line, signal_line, histogram), each as long as `data`.
# Uses ema(); if it is not loaded, ema.R is sourced from the working folder.

if (!exists("ema", mode = "function")) source("ema.R")

macd <- function(data, short_period, long_period, signal_period) {
  if (!is.numeric(data)) stop("data must be a numeric vector", call. = FALSE)
  periods <- c(short_period, long_period, signal_period)
  if (!is.numeric(periods) || anyNA(periods) || any(periods < 1)) {
    stop("short_period, long_period and signal_period must be positive numbers", call. = FALSE)
  }

  # Calculate the short-term and long-term EMAs
  fast <- ema(data, short_period)
  slow <- ema(data, long_period)

  # MACD line, signal line and histogram
  line   <- fast - slow
  signal <- ema(line, signal_period)
  hist   <- line - signal

  return(list(macd_line = line, signal_line = signal, histogram = hist))
}
