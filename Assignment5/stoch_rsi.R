# stoch_rsi.R - Stochastic RSI (base R only, no libraries)
# Author: Harjit Gill | BDA400 Assignment 5
#
# stoch_rsi(data, period, k_period, d_period)
# Pseudocode:  stoch = (RSI - min RSI) / (max RSI - min RSI) over all RSI values
#              k_line = sma(stoch, k_period);  d_line = sma(k_line, d_period)
# Returns list(k_line, d_line). sma() does not pad, so k_line has
# length(data) - k_period + 1 values and d_line is shorter again. Values that
# depend on the NA warm-up of the RSI are NA. Needs rsi() and sma().

if (!exists("sma", mode = "function")) source("sma.R")
if (!exists("rsi", mode = "function")) source("rsi.R")

stoch_rsi <- function(data, period, k_period, d_period) {
  if (!is.numeric(data)) stop("data must be a numeric vector", call. = FALSE)

  strength <- rsi(data, period)
  known <- strength[!is.na(strength)]

  if (length(known) == 0 || max(known) == min(known)) {
    scaled <- rep(NA_real_, length(strength))      # nothing to rescale
  } else {
    scaled <- (strength - min(known)) / (max(known) - min(known))
  }

  k_line <- sma(scaled, k_period)
  d_line <- sma(k_line, d_period)

  return(list(k_line = k_line, d_line = d_line))
}
