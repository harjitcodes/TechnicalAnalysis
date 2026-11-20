# run_all_tests.R - tests for all Assignment 5 indicator functions
# Author: Harjit Gill | BDA400 Assignment 5
#
# HOW TO RUN
#   1. Keep this file and the nine indicator scripts in one folder.
#   2. In RStudio: Session > Set Working Directory > To Source File Location.
#   3. Click Source (or run source("run_all_tests.R")).
# Expected values were calculated independently in Python from the assignment
# pseudocode. Base R helpers (sd, mean) appear only in this test file as
# cross-checks; the indicator scripts themselves use no libraries.

for (f in c("sma", "ema", "macd", "stdev", "linreg", "rsi", "stoch_rsi",
            "crossover", "crossunder")) source(paste0(f, ".R"))

passed <- 0; failed <- 0
check <- function(label, ok) {
  ok <- isTRUE(ok)
  if (ok) passed <<- passed + 1 else failed <<- failed + 1
  cat(sprintf("%-64s %s\n", label, if (ok) "PASS" else "FAIL"))
}
same <- function(a, b) isTRUE(all.equal(a, b, tolerance = 1e-8))
fails <- function(expr) inherits(try(expr, silent = TRUE), "try-error")

# My own price series (15 closing prices) and two short series for the crossovers
prices <- c(152, 155, 151, 158, 163, 160, 166, 170, 167, 174, 171, 177, 173, 181, 184)
a <- c(5, 3, 4, 2, 3, 6, 7, 5, 4, 6, 8)
b <- c(4, 4, 3, 3, 4, 5, 6, 6, 5, 5, 7)

cat("\n--- SMA ---\n"); print(sma(prices, 5))
check("sma: period 5 on my series", same(sma(prices, 5), c(155.8, 157.4, 159.6, 163.4, 165.2, 167.4, 169.6, 171.8, 172.4, 175.2, 177.2)))
check("sma: 11 values for 15 points and period 5", length(sma(prices, 5)) == 11)
check("sma: first value = mean of first 5 points", same(sma(prices, 5)[1], mean(prices[1:5])))
check("sma: period 1 returns the data", same(sma(prices, 1), prices))
check("sma: period = length gives the mean", same(sma(prices, 15), mean(prices)))
check("sma: hand calculation c(2, 4, 6, 8), period 2", same(sma(c(2, 4, 6, 8), 2), c(3, 5, 7)))
check("sma: error if data is shorter than the period", fails(sma(c(1, 2), 3)))
check("sma: error for text data", fails(sma("a", 1)))
check("sma: error for period 0", fails(sma(prices, 0)))

cat("\n--- EMA ---\n"); print(ema(prices, 4))
check("ema: period 4 on my series", same(ema(prices, 4), c(152, 153.2, 152.32, 154.592, 157.9552, 158.77312, 161.663872, 164.9983232, 165.79899392, 169.079396352, 169.8476378112, 172.7085826867, 172.825149612, 176.0950897672, 179.2570538603)))
check("ema: first value is the first data point", ema(prices, 4)[1] == prices[1])
check("ema: same length as the data", length(ema(prices, 4)) == length(prices))
check("ema: hand calculation c(10, 20), period 3 gives 10, 15", same(ema(c(10, 20), 3), c(10, 15)))
check("ema: period 1 returns the data", same(ema(prices, 1), prices))
check("ema: empty data gives an empty vector", length(ema(numeric(0), 3)) == 0)
check("ema: error for period 1.5", fails(ema(prices, 1.5)))

cat("\n--- MACD ---\n"); m <- macd(prices, 3, 6, 3); print(m$macd_line)
check("macd: MACD line (3, 6, 3)", same(m$macd_line, c(0, 0.6428571429, -0.0765306122, 1.1774781341, 2.5285558101, 2.0070041501, 2.8197351072, 3.5643197194, 2.678197121, 3.4791251757, 2.6252958844, 3.2310288684, 2.1286436671, 3.1451284285, 3.7017117821)))
check("macd: signal line (3, 6, 3)", same(m$signal_line, c(0, 0.3214285714, 0.1224489796, 0.6499635569, 1.5892596835, 1.7981319168, 2.308933512, 2.9366266157, 2.8074118684, 3.143268522, 2.8842822032, 3.0576555358, 2.5931496015, 2.869139015, 3.2854253985)))
check("macd: histogram = MACD line - signal line", same(m$histogram, m$macd_line - m$signal_line))
check("macd: MACD line = short EMA - long EMA", same(m$macd_line, ema(prices, 3) - ema(prices, 6)))
check("macd: three named vectors", identical(names(m), c("macd_line", "signal_line", "histogram")))
check("macd: every vector as long as the data", all(lengths(m) == length(prices)))
check("macd: error for a negative period", fails(macd(prices, -3, 6, 3)))

cat("\n--- Standard deviation ---\n"); print(stdev(prices))
check("stdev: my series", same(stdev(prices), 9.8873656754))
check("stdev: textbook example 2,4,4,4,5,5,7,9 gives 2", same(stdev(c(2, 4, 4, 4, 5, 5, 7, 9)), 2))
check("stdev: equals sd() * sqrt((n-1)/n)", same(stdev(prices), sd(prices) * sqrt(14 / 15)))
check("stdev: constant data gives 0", stdev(rep(3, 6)) == 0)
check("stdev: single value gives 0", stdev(42) == 0)
check("stdev: error for empty data", fails(stdev(numeric(0))))

cat("\n--- Linear regression ---\n")
l1 <- linreg(prices, 7, 0); print(c(l1$slope, l1$intercept))
check("linreg: (7, 0) slope", same(l1$slope, 2.0357142857))
check("linreg: (7, 0) intercept", same(l1$intercept, 165.4642857143))
check("linreg: (7, 0) predicted values", same(l1$predicted_values, c(167.5, 169.5357142857, 171.5714285714, 173.6071428571, 175.6428571429, 177.6785714286, 179.7142857143, 181.75)))
l2 <- linreg(prices, 9, 3)
check("linreg: (9, 3) predicted values", same(l2$predicted_values, c(168.2, 170.9, 173.6, 176.3)))
check("linreg: perfect line y = 3x + 1 gives slope 3 and intercept 1",
      same(unname(unlist(linreg(3 * (1:9) + 1, 9, 1)[c("slope", "intercept")])), c(3, 1)))
check("linreg: error if length is more than the data", fails(linreg(1:5, 6, 0)))
check("linreg: error if offset >= length", fails(linreg(1:5, 3, 3)))
check("linreg: error for text data", fails(linreg(c("a", "b", "c"), 2, 0)))

cat("\n--- RSI ---\n"); r <- rsi(prices, 5); print(r)
check("rsi: period 5, positions 6 to 15", same(r[6:15], c(58.2524271845, 69.3950177936, 74.9636098981, 64.0397917152, 74.7651445946, 64.4649328282, 73.5684562489, 60.6256724689, 72.6533195722, 76.0785949534)))
check("rsi: first 5 values are NA, the rest are not", all(is.na(r[1:5])) && !anyNA(r[6:15]))
check("rsi: always between 0 and 100", all(r[6:15] >= 0 & r[6:15] <= 100))
check("rsi: only rises gives 100", all(rsi(1:12, 5)[6:12] == 100))
check("rsi: only falls gives 0", all(rsi(12:1, 5)[6:12] == 0))
check("rsi: flat prices give 50", all(rsi(rep(8, 12), 5)[6:12] == 50))
check("rsi: too little data gives NA and a warning",
      all(is.na(suppressWarnings(rsi(1:3, 5)))) && inherits(tryCatch(rsi(1:3, 5), warning = function(w) w), "warning"))

cat("\n--- Stochastic RSI ---\n"); s <- stoch_rsi(prices, 5, 3, 3); print(s$k_line)
check("stoch_rsi: %K line from position 6", same(s$k_line[6:13], c(0.5208405546, 0.6290590756, 0.7294756256, 0.5331597899, 0.711337262, 0.4469418274, 0.6000573081, 0.6469946483)))
check("stoch_rsi: %D line from position 6", same(s$d_line[6:11], c(0.6264584186, 0.6305648303, 0.6579908925, 0.5638129597, 0.5861121325, 0.5646645946)))
check("stoch_rsi: lengths are 13 and 11", length(s$k_line) == 13 && length(s$d_line) == 11)
ok_vals <- c(s$k_line, s$d_line); ok_vals <- ok_vals[!is.na(ok_vals)]
check("stoch_rsi: values stay between 0 and 1", all(ok_vals >= 0 & ok_vals <= 1))
check("stoch_rsi: flat prices give only NA", all(is.na(unlist(stoch_rsi(rep(8, 20), 5, 3, 3)))))

cat("\n--- Crossover and crossunder ---\n"); print(crossover(a, b)); print(crossunder(a, b))
check("crossover: my arrays", identical(crossover(a, b), c("None", "Down", "Up", "Down", "None", "Up", "None", "Down", "None", "Up", "None")))
check("crossunder: my arrays", identical(crossunder(a, b), c("None", "True", "False", "True", "False", "False", "False", "True", "False", "False", "False")))
check("crossover: first position is always None", crossover(a, b)[1] == "None")
check("crossover: equal arrays never cross", all(crossover(1:6, 1:6) == "None"))
check("crossover: NA values give no signal", all(crossover(c(1, NA, 3), c(2, 2, 2)) == "None"))
check("crossunder: NA values give no signal", all(crossunder(c(1, NA, 3), c(2, 2, 2))[-1] == "False"))
check("crossover: error for different lengths", fails(crossover(1:3, 1:2)))
check("crossunder: error for different lengths", fails(crossunder(1:3, 1:2)))

cat(sprintf("\nTests passed: %d | failed: %d\n", passed, failed))
if (failed == 0) cat("All tests passed.\n") else stop("Some tests failed.")
