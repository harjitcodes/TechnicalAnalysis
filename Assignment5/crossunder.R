# crossunder.R - Crossunder signals (base R only, no libraries)
# Author: Harjit Gill | BDA400 Assignment 5
#
# crossunder(arr1, arr2)
# Character vector as long as the inputs. The first element is "None". Each
# later element is "True" when arr1 crosses under arr2
# (arr1[i] < arr2[i] and arr1[i-1] >= arr2[i-1]) and "False" otherwise.
# Missing values never create a signal.

crossunder <- function(arr1, arr2) {
  # Check that both arrays have the same length
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length", call. = FALSE)
  }

  n <- length(arr1)
  signal <- rep("None", n)
  if (n < 2) return(signal)

  now  <- 2:n
  prev <- 1:(n - 1)
  crossed <- arr1[now] < arr2[now] & arr1[prev] >= arr2[prev]
  signal[now] <- ifelse(!is.na(crossed) & crossed, "True", "False")

  return(signal)
}
