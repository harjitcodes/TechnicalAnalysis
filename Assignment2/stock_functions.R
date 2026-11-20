# stock_functions.R
# BDA400 Assignment 2 - Technical Analysis using R (Preliminary Stage)
# Author: Harjit Gill
# Reads portfolio.txt, imports daily prices from Yahoo Finance and
# calculates descriptive statistics for each stock.

pkgs_needed <- c("quantmod", "TTR", "ggplot2")

# Install any missing package, then attach all of them.
setup_packages <- function(pkgs = pkgs_needed) {
  absent <- setdiff(pkgs, rownames(installed.packages()))
  if (length(absent)) install.packages(absent)
  for (p in pkgs) suppressPackageStartupMessages(library(p, character.only = TRUE))
  invisible(TRUE)
}

# ---- Importing ----------------------------------------------------------------

# Ticker symbols from the text file: one per line, blank lines and # lines skipped.
get_symbols_from_file <- function(path = "portfolio.txt") {
  if (!file.exists(path)) stop("Cannot find the portfolio file: ", path, call. = FALSE)
  lines <- trimws(readLines(path, warn = FALSE))
  lines <- lines[lines != "" & !startsWith(lines, "#")]
  if (!length(lines)) stop("The portfolio file has no ticker symbols.", call. = FALSE)
  unique(toupper(lines))
}

# Turn the xts object from quantmod into an ordinary data frame.
xts_to_df <- function(x) {
  df <- as.data.frame(zoo::coredata(x))
  names(df) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  df <- cbind(Date = as.Date(zoo::index(x)), df)
  df[stats::complete.cases(df), ]
}

# Download every symbol in the portfolio file.
# Returns a named list of data frames and, when to_global = TRUE, also creates
# one variable per ticker (AAPL, MA, XOM ...) in the global environment.
load_stock_data <- function(path = "portfolio.txt", from = "2024-01-01",
                            to = Sys.Date(), to_global = TRUE) {
  out <- list()
  for (sym in get_symbols_from_file(path)) {
    x <- try(getSymbols(sym, src = "yahoo", from = from, to = to, auto.assign = FALSE),
             silent = TRUE)
    if (inherits(x, "try-error")) {
      message("Skipped ", sym, ": download failed.")
      next
    }
    out[[sym]] <- xts_to_df(x)
    if (to_global) assign(sym, out[[sym]], envir = globalenv())
  }
  if (!length(out)) stop("Nothing was downloaded. Check the internet connection.", call. = FALSE)
  out
}

# ---- Statistics ---------------------------------------------------------------

# Mode of the prices after rounding (exact repeats are rare for prices).
mode_value <- function(x, digits = 2) {
  r <- round(x, digits)
  tab <- tabulate(match(r, unique(r)))
  unique(r)[which.max(tab)]
}

# Statistics of one stock's Close price, plus the daily returns.
calculate_statistics <- function(df, window = 50) {
  ma <- SMA(df$Close, n = window)
  ret <- c(NA, 100 * diff(df$Close) / head(df$Close, -1))
  list(
    summary = data.frame(
      Days      = nrow(df),
      Mean      = mean(df$Close),
      Median    = median(df$Close),
      Mode      = mode_value(df$Close),
      Std_Dev   = sd(df$Close),
      Min       = min(df$Close),
      Max       = max(df$Close),
      Latest_MA = tail(ma, 1),
      MA_Days   = window
    ),
    detail = data.frame(Date = df$Date, Close = df$Close, MA = ma, Return = ret)
  )
}

# Run calculate_statistics() for every stock and stack the results.
calculate_all_statistics <- function(stocks, window = 50) {
  res <- lapply(stocks, calculate_statistics, window = window)
  summary_tbl <- do.call(rbind, Map(function(s, nm) data.frame(Ticker = nm, s$summary),
                                    res, names(res)))
  detail_tbl  <- do.call(rbind, Map(function(s, nm) data.frame(Ticker = nm, s$detail),
                                    res, names(res)))
  rownames(summary_tbl) <- rownames(detail_tbl) <- NULL
  list(summary = summary_tbl, detail = detail_tbl)
}
