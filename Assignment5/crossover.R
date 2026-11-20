# crossover.R - Crossover signals (base R only, no libraries)
# Author: Harjit Gill | BDA400 Assignment 5
#
# crossover(arr1, arr2)
# Character vector as long as the inputs: "Up" when arr1 crosses above arr2
# (arr1[i] > arr2[i] and arr1[i-1] <= arr2[i-1]), "Down" when it crosses below
# (arr1[i] < arr2[i] and arr1[i-1] >= arr2[i-1]), otherwise "None". The first
# position is always "None". Missing values never create a signal.

crossover <- function(arr1, arr2) {
  # Check that both arrays have the same length
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length", call. = FALSE)
  }

  n <- length(arr1)
  signal <- rep("None", n)
  if (n < 2) return(signal)

  now  <- 2:n
  prev <- 1:(n - 1)
  up   <- which(arr1[now] > arr2[now] & arr1[prev] <= arr2[prev])
  down <- which(arr1[now] < arr2[now] & arr1[prev] >= arr2[prev])

  signal[up + 1]   <- "Up"
  signal[down + 1] <- "Down"

  return(signal)
}
