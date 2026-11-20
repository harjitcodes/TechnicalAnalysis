# stock_display.R
# BDA400 Assignment 2 - displaying the imported data and the statistics.
# Author: Harjit Gill. Load stock_functions.R first.

# Quote box for the most recent trading day.
show_quote <- function(df, symbol) {
  n <- nrow(df); today <- df[n, ]; before <- df[n - 1, ]
  chg <- today$Close - before$Close
  last_year <- df[df$Date >= today$Date - 365, ]
  cat("\n==", symbol, "-", format(today$Date, "%d %b %Y"), "==\n")
  cat(sprintf("Close    : %9.2f   change %+.2f (%+.2f%%)\n", today$Close, chg, 100 * chg / before$Close))
  cat(sprintf("Open     : %9.2f   day range %.2f - %.2f\n", today$Open, today$Low, today$High))
  cat(sprintf("52 weeks : %9.2f - %.2f\n", min(last_year$Low), max(last_year$High)))
  cat(sprintf("Volume   : %s\n", format(today$Volume, big.mark = ",")))
}

# First and last rows plus a numeric summary.
show_table <- function(df, symbol, rows = 5) {
  cat("\nFirst", rows, "rows of", symbol, "\n"); print(head(df, rows), row.names = FALSE)
  cat("\nLast", rows, "rows of", symbol, "\n");  print(tail(df, rows), row.names = FALSE)
  cat("\nSummary of", symbol, "\n"); print(summary(df[, c("Open", "High", "Low", "Close", "Volume")]))
}

show_all <- function(stocks, rows = 5) {
  for (nm in names(stocks)) { show_quote(stocks[[nm]], nm); show_table(stocks[[nm]], nm, rows) }
  invisible(stocks)
}

# Price with its moving average, one panel per stock.
chart_ma <- function(detail, window = 50) {
  ggplot(detail, aes(Date)) +
    geom_line(aes(y = Close, colour = "Close")) +
    geom_line(aes(y = MA, colour = paste0(window, "-day MA")), linewidth = 1, na.rm = TRUE) +
    facet_wrap(~ Ticker, scales = "free_y", ncol = 2) +
    scale_colour_manual(values = setNames(c("#64748b", "#be123c"), c("Close", paste0(window, "-day MA")))) +
    labs(title = "Close price and moving average", x = NULL, y = "USD", colour = NULL) +
    theme_minimal() + theme(legend.position = "bottom")
}

# Distribution of daily percentage returns, one panel per stock.
chart_returns <- function(detail) {
  ggplot(detail[!is.na(detail$Return), ], aes(Return)) +
    geom_histogram(bins = 40, fill = "#4f46e5", colour = "white") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    facet_wrap(~ Ticker, scales = "free_y", ncol = 2) +
    labs(title = "Daily returns (%)", x = "Return (%)", y = "Days") +
    theme_minimal()
}

# Spread of the close prices, indexed to the first day so stocks are comparable.
chart_spread <- function(detail) {
  first <- ave(detail$Close, detail$Ticker, FUN = function(v) v[1])
  detail$Indexed <- 100 * detail$Close / first
  ggplot(detail, aes(Ticker, Indexed, fill = Ticker)) +
    geom_boxplot(show.legend = FALSE) +
    geom_hline(yintercept = 100, linetype = "dashed", colour = "grey40") +
    labs(title = "Close price indexed to 100 on the first day", x = NULL, y = "Index") +
    theme_minimal()
}

# Standard deviation of each stock as a share of its mean price.
chart_stats <- function(tbl) {
  tbl$Variation <- 100 * tbl$Std_Dev / tbl$Mean
  ggplot(tbl, aes(reorder(Ticker, Variation), Variation)) +
    geom_col(fill = "#0d9488", width = 0.6) +
    geom_text(aes(label = sprintf("%.1f%%", Variation)), hjust = -0.1) +
    coord_flip() + expand_limits(y = max(tbl$Variation) * 1.15) +
    labs(title = "Standard deviation as % of the mean close", x = NULL, y = "%") +
    theme_minimal()
}
