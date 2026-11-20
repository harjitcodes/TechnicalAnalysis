# =============================================================================
# app.R - Stock Portfolio Dashboard (R Shiny)
# BDA400 - Assignment 6: Technical Analysis using R, Visualization Phase
# Author: Harjit Gill
#
# What the app does
#   * downloads prices from Yahoo Finance (or uses offline demo data)
#   * draws candlestick, OHLC-bar or line charts for a date range and a time frame
#   * overlays Moving Averages and Bollinger Bands, with Volume, RSI and MACD
#     panels underneath (indicator functions are my Assignment 5 functions)
#   * turns a chosen trading rule into a Buy / Sell / Hold signal for each bar
#     and marks the signals on the chart
#   * compares every stock listed in portfolio.txt on a Portfolio tab
#
# How to run
#   1. Open app.R in RStudio and click "Run App" (or shiny::runApp("app.R")).
#   2. The first run installs missing packages (shiny, ggplot2, quantmod).
#   3. Choose "Yahoo Finance" (needs internet) or "Demo data (offline)".
#
# Sections follow the assignment: Step 1 data and setup, Step 2 visualization,
# Step 3 technical indicators, Step 4 trading rules and annotations.
# =============================================================================


# =============================================================================
# STEP 1: DATA COLLECTION AND SETUP
# =============================================================================

# ---- packages ---------------------------------------------------------------
needed <- c("shiny", "ggplot2", "quantmod")
missing_pkgs <- setdiff(needed, rownames(installed.packages()))
if (length(missing_pkgs)) install.packages(missing_pkgs)
suppressPackageStartupMessages({ library(shiny); library(ggplot2); library(quantmod) })

# ---- indicator functions (Assignment 5, base R) ------------------------------
sma <- function(data, period) {
  if (length(data) < period) stop("Data length should be greater than or equal to the period")
  starts <- seq_len(length(data) - period + 1)
  vapply(starts, function(i) sum(data[i:(i + period - 1)]), numeric(1)) / period
}

ema <- function(data, period) {
  alpha <- 2 / (period + 1)
  smoothed <- data
  for (t in seq_along(data)[-1]) smoothed[t] <- alpha * data[t] + (1 - alpha) * smoothed[t - 1]
  smoothed
}

stdev <- function(data) {
  centred <- data - sum(data) / length(data)
  sqrt(drop(crossprod(centred)) / length(data))
}

macd <- function(data, short_period, long_period, signal_period) {
  line <- ema(data, short_period) - ema(data, long_period)
  signal <- ema(line, signal_period)
  list(macd_line = line, signal_line = signal, histogram = line - signal)
}

rsi <- function(data, period) {
  n <- length(data)
  if (n < period + 1) return(rep(NA_real_, n))
  change <- diff(data)
  up <- ifelse(change > 0, change, 0); down <- ifelse(change < 0, -change, 0)
  gain_avg <- sum(up[1:period]) / period; loss_avg <- sum(down[1:period]) / period
  out <- rep(NA_real_, n)
  for (k in period:(n - 1)) {
    gain_avg <- (gain_avg * (period - 1) + up[k]) / period
    loss_avg <- (loss_avg * (period - 1) + down[k]) / period
    out[k + 1] <- if (gain_avg == 0 && loss_avg == 0) 50
                  else if (loss_avg == 0) 100
                  else 100 - 100 / (1 + gain_avg / loss_avg)
  }
  out
}

# Helpers: sma() is shorter than its input, so pad the front with NA to line up with dates
pad_front <- function(x, total) c(rep(NA_real_, total - length(x)), x)

moving_average <- function(x, period, type = "SMA") {
  if (length(x) < period) return(rep(NA_real_, length(x)))
  if (type == "EMA") {
    out <- ema(x, period); out[seq_len(period - 1)] <- NA_real_; out
  } else pad_front(sma(x, period), length(x))
}

rolling_sd <- function(x, period) {
  if (length(x) < period) return(rep(NA_real_, length(x)))
  pad_front(vapply(seq_len(length(x) - period + 1),
                   function(i) stdev(x[i:(i + period - 1)]), numeric(1)), length(x))
}

# ---- data sources -------------------------------------------------------------
HISTORY_YEARS <- 8

symbols_in_file <- local({
  fallback <- c("AAPL", "MA", "XOM", "COST", "TSLA")
  if (!file.exists("portfolio.txt")) return(fallback)
  s <- toupper(trimws(readLines("portfolio.txt", warn = FALSE)))
  s <- s[s != "" & !startsWith(s, "#")]
  if (length(s)) unique(s) else fallback
})

download_yahoo <- function(symbol, from, to) {
  xt <- tryCatch(
    suppressWarnings(getSymbols(symbol, src = "yahoo", from = from, to = to, auto.assign = FALSE)),
    error = function(e) stop("Yahoo Finance did not return data for '", symbol, "' (",
                             conditionMessage(e), "). Check the symbol and the internet connection, ",
                             "or switch to 'Demo data (offline)'.", call. = FALSE))
  df <- data.frame(Date = as.Date(index(xt)), Open = as.numeric(Op(xt)), High = as.numeric(Hi(xt)),
                   Low = as.numeric(Lo(xt)), Close = as.numeric(Cl(xt)), Volume = as.numeric(Vo(xt)))
  df[complete.cases(df), ]
}

# Synthetic prices for offline use (the same numbers every time for a symbol)
make_demo <- function(symbol, from, to) {
  days <- seq(as.Date(from), as.Date(to), by = "day")
  days <- days[as.integer(format(days, "%u")) <= 5]
  seed <- sum(utf8ToInt(symbol) * seq_along(utf8ToInt(symbol))) + 11
  set.seed(seed)
  n <- length(days)
  trend <- sin(seq(0, 7, length.out = n)) * 0.0008
  close <- (60 + seed %% 140) * exp(cumsum(rnorm(n, 0.0002, 0.014) + trend))
  open <- c(close[1], close[-n]) * exp(rnorm(n, 0, 0.004))
  data.frame(Date = days, Open = open,
             High = pmax(open, close) * (1 + abs(rnorm(n, 0, 0.007))),
             Low = pmin(open, close) * (1 - abs(rnorm(n, 0, 0.007))),
             Close = close, Volume = round(runif(n, 4e6, 6e7)))
}

check_prices <- function(df) {
  if (is.null(df) || nrow(df) < 5) stop("Not enough price data came back.", call. = FALSE)
  if (anyNA(df$Close)) df <- df[!is.na(df$Close), ]
  df
}

# Daily bars -> weekly (Monday start) or monthly bars
change_time_frame <- function(df, frame) {
  if (frame == "Daily") return(df)
  key <- if (frame == "Weekly") format(df$Date - (as.integer(format(df$Date, "%u")) - 1))
         else format(df$Date, "%Y-%m")
  parts <- split(df, factor(key, levels = unique(key)))
  out <- do.call(rbind, lapply(parts, function(g)
    data.frame(Date = g$Date[nrow(g)], Open = g$Open[1], High = max(g$High), Low = min(g$Low),
               Close = g$Close[nrow(g)], Volume = sum(g$Volume))))
  rownames(out) <- NULL
  out
}


# =============================================================================
# STEP 4 (helper): TRADING RULES - every bar is "Buy", "Sell" or "Hold"
# =============================================================================
RULES <- c("Trend: short MA vs long MA", "RSI turn-around", "Majority vote (MA + RSI + MACD)")

make_signal <- function(df, rule, s) {
  n <- nrow(df)
  if (rule == RULES[1]) {
    short <- moving_average(df$Close, s$ma_short, s$ma_type)
    long  <- moving_average(df$Close, s$ma_long, s$ma_type)
    sig <- ifelse(short > long, "Buy", ifelse(short < long, "Sell", "Hold"))
    sig[is.na(sig)] <- "Hold"
    return(sig)
  }
  r <- rsi(df$Close, s$rsi_period)
  if (rule == RULES[2]) {
    # Buy once the RSI climbs back above the buy level, Sell once it falls back
    # under the sell level; the signal stays until the opposite event happens.
    sig <- rep("Hold", n); state <- "Hold"
    for (i in seq_len(n)[-1]) {
      if (!is.na(r[i]) && !is.na(r[i - 1])) {
        if (r[i - 1] <= s$rsi_low  && r[i] > s$rsi_low)  state <- "Buy"
        if (r[i - 1] >= s$rsi_high && r[i] < s$rsi_high) state <- "Sell"
      }
      sig[i] <- state
    }
    return(sig)
  }
  # Majority vote: each of the three indicators votes +1 (bullish), -1 (bearish) or 0
  short <- moving_average(df$Close, s$ma_short, s$ma_type)
  long  <- moving_average(df$Close, s$ma_long, s$ma_type)
  m <- macd(df$Close, s$macd_fast, s$macd_slow, s$macd_signal)
  v_ma   <- sign(short - long)
  v_rsi  <- ifelse(r < s$rsi_low, 1, ifelse(r > s$rsi_high, -1, 0))
  v_macd <- sign(m$macd_line - m$signal_line)
  v_macd[seq_len(min(s$macd_slow, n))] <- 0                    # ignore the EMA warm-up
  votes <- rowSums(cbind(v_ma, v_rsi, v_macd), na.rm = TRUE)
  ifelse(votes >= 2, "Buy", ifelse(votes <= -2, "Sell", "Hold"))
}

# TRUE on the bars where the signal changes to Buy or Sell
signal_flips <- function(sig) sig != c("Hold", head(sig, -1)) & sig %in% c("Buy", "Sell")


# =============================================================================
# STEP 2: SHINY SKELETON - USER INTERFACE
# =============================================================================
INDICATORS <- c("Moving Averages", "Bollinger Bands", "Volume", "RSI", "MACD")

ui <- navbarPage(
  title = "Stock Portfolio Dashboard", id = "tabs", collapsible = TRUE,
  header = tags$head(tags$style(HTML("
    body { background: #f4f6fb; }
    .navbar-default { background: #1e2a4a; border: 0; }
    .navbar-default .navbar-brand, .navbar-default .navbar-nav > li > a { color: #e6ebff; }
    .navbar-default .navbar-nav > .active > a { background: #324376 !important; color: #fff !important; }
    .well { background: #fff; border: 1px solid #d9deea; }
    .stat-strip { display: flex; flex-wrap: wrap; gap: 10px; margin-bottom: 12px; }
    .stat { background: #fff; border: 1px solid #d9deea; border-top: 3px solid #4f46e5;
            border-radius: 4px; padding: 6px 14px; min-width: 120px; }
    .stat .lab { font-size: 11px; color: #64708f; text-transform: uppercase; letter-spacing: .04em; }
    .stat .val { font-size: 19px; font-weight: 600; color: #1e2a4a; }
    .up { color: #15803d !important; } .down { color: #b91c1c !important; }
    .note { color: #64708f; font-size: 12px; }
  "))),

  # ---------------------------------------------------------------- Market tab
  tabPanel("Market",
    sidebarLayout(
      sidebarPanel(width = 3,
        h4("Stock and period"),
        selectInput("source", "Data source", c("Yahoo Finance", "Demo data (offline)")),
        selectizeInput("symbol", "Stock symbol", choices = symbols_in_file,
                       selected = symbols_in_file[1], options = list(create = TRUE)),
        # ---- the two required interactive widgets ----
        dateRangeInput("date_range", "Select Date Range:",
                       start = Sys.Date() - 730, end = Sys.Date()),
        selectInput("time_frame", "Select Time Frame:", c("Daily", "Weekly", "Monthly")),
        hr(), h4("Chart"),
        selectInput("chart_type", "Chart type", c("Candlestick", "OHLC bars", "Line")),
        checkboxGroupInput("indicators", "Show on the chart", INDICATORS,
                           selected = c("Moving Averages", "Volume")),
        hr(), h4("Trading rule"),
        selectInput("rule", NULL, RULES),
        checkboxInput("shade", "Shade Buy / Sell periods", TRUE),
        checkboxInput("arrows", "Mark the signal changes", TRUE),
        hr(), h4("Indicator settings"),
        conditionalPanel(
          "input.indicators.indexOf('Moving Averages') > -1 || input.rule != 'RSI turn-around'",
          selectInput("ma_type", "Moving average type", c("SMA", "EMA")),
          fluidRow(column(6, numericInput("ma_short", "Short MA", 20, min = 2, max = 200)),
                   column(6, numericInput("ma_long", "Long MA", 50, min = 3, max = 400)))),
        conditionalPanel("input.indicators.indexOf('Bollinger Bands') > -1",
          fluidRow(column(6, numericInput("bb_period", "Bollinger days", 20, min = 3, max = 200)),
                   column(6, numericInput("bb_width", "Std devs", 2, min = 0.5, max = 4, step = 0.5)))),
        conditionalPanel(
          "input.indicators.indexOf('RSI') > -1 || input.rule != 'Trend: short MA vs long MA'",
          numericInput("rsi_period", "RSI period", 14, min = 2, max = 100),
          fluidRow(column(6, numericInput("rsi_low", "Buy level", 30, min = 1, max = 49)),
                   column(6, numericInput("rsi_high", "Sell level", 70, min = 51, max = 99)))),
        conditionalPanel(
          "input.indicators.indexOf('MACD') > -1 || input.rule == 'Majority vote (MA + RSI + MACD)'",
          fluidRow(column(4, numericInput("macd_fast", "Fast", 12, min = 2, max = 100)),
                   column(4, numericInput("macd_slow", "Slow", 26, min = 3, max = 200)),
                   column(4, numericInput("macd_signal", "Signal", 9, min = 2, max = 100))))
      ),
      mainPanel(width = 9, uiOutput("stats"), plotOutput("chart", height = "auto"))
    )),

  # ---------------------------------------------------------------- Signals tab
  tabPanel("Signals",
    br(), uiOutput("signal_text"), br(),
    downloadButton("download_signals", "Download signals (CSV)"), br(), br(),
    tableOutput("signal_table")),

  # -------------------------------------------------------------- Portfolio tab
  tabPanel("Portfolio",
    br(),
    p("Every stock in portfolio.txt over the date range and trading rule chosen on the Market tab."),
    actionButton("run_portfolio", "Load portfolio", class = "btn-primary"), br(), br(),
    plotOutput("portfolio_chart", height = "320px"), br(),
    tableOutput("portfolio_table")),

  # ------------------------------------------------------------------ Data tab
  tabPanel("Data", br(), p(class = "note", "The 60 most recent bars with the indicator values."),
           tableOutput("data_table")),

  tabPanel("About", br(),
    p("Prices come from Yahoo Finance through the quantmod package, or from synthetic demo data."),
    p("SMA, EMA, standard deviation, MACD and RSI are the base-R functions written for Assignment 5."),
    p("Each bar receives a Buy, Sell or Hold signal from the selected trading rule."),
    p(strong("For learning only. This is not investment advice.")))
)


# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  cache <- new.env()

  load_prices <- function(sym, source) {
    key <- paste(source, sym)
    if (is.null(cache[[key]])) {
      to <- Sys.Date(); from <- seq(to, by = paste0("-", HISTORY_YEARS, " years"), length.out = 2)[2]
      raw <- if (source == "Yahoo Finance") download_yahoo(sym, from, to) else make_demo(sym, from, to)
      cache[[key]] <- check_prices(raw)
    }
    cache[[key]]
  }

  sym_now <- reactive(toupper(trimws(input$symbol)))

  # ---- Step 1: data ---------------------------------------------------------------
  prices <- reactive({
    validate(need(nzchar(sym_now()), "Type a stock symbol."))
    df <- tryCatch(
      withProgress(message = paste("Loading", sym_now()), value = 0.5, load_prices(sym_now(), input$source)),
      error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 12); NULL })
    validate(need(!is.null(df), "No data. Read the message, or switch to 'Demo data (offline)'."))
    df
  })

  # ---- Step 2: date range and time frame ---------------------------------------------
  bars <- reactive({
    validate(need(input$date_range[1] < input$date_range[2], "The start date must be before the end date."))
    df <- prices()
    df <- df[df$Date >= input$date_range[1] & df$Date <= input$date_range[2], ]
    validate(need(nrow(df) >= 5, "Fewer than 5 trading days in this range. Widen the date range."))
    change_time_frame(df, input$time_frame)
  })

  settings <- reactive(list(
    ma_type = input$ma_type, ma_short = input$ma_short, ma_long = input$ma_long,
    rsi_period = input$rsi_period, rsi_low = input$rsi_low, rsi_high = input$rsi_high,
    macd_fast = input$macd_fast, macd_slow = input$macd_slow, macd_signal = input$macd_signal,
    bb_period = input$bb_period, bb_width = input$bb_width))

  # ---- Steps 3 and 4: indicators and signals -----------------------------------------
  analysed <- reactive({
    df <- bars(); s <- settings()
    validate(
      need(s$ma_short < s$ma_long, "The short MA must be smaller than the long MA."),
      need(s$macd_fast < s$macd_slow, "MACD fast must be smaller than MACD slow."),
      need(s$rsi_low < s$rsi_high, "The RSI buy level must be below the sell level."))
    df$MA_short <- moving_average(df$Close, s$ma_short, s$ma_type)
    df$MA_long  <- moving_average(df$Close, s$ma_long, s$ma_type)
    mid <- moving_average(df$Close, s$bb_period, "SMA"); spread <- rolling_sd(df$Close, s$bb_period)
    df$BB_mid <- mid; df$BB_top <- mid + s$bb_width * spread; df$BB_bottom <- mid - s$bb_width * spread
    df$RSI <- rsi(df$Close, s$rsi_period)
    m <- macd(df$Close, s$macd_fast, s$macd_slow, s$macd_signal)
    df$MACD <- m$macd_line; df$MACD_signal <- m$signal_line; df$MACD_hist <- m$histogram
    df$Signal <- make_signal(df, input$rule, s)
    df$Change <- signal_flips(df$Signal)
    df
  })

  # ---- summary strip ------------------------------------------------------------------
  output$stats <- renderUI({
    df <- analysed(); n <- nrow(df)
    chg <- if (n > 1) 100 * (df$Close[n] / df$Close[n - 1] - 1) else 0
    now <- df$Signal[n]
    stat <- function(lab, val, cls = "") div(class = "stat", div(class = "lab", lab), div(class = paste("val", cls), val))
    div(class = "stat-strip",
      stat("Symbol", sym_now()), stat("Last close", sprintf("$%.2f", df$Close[n])),
      stat("Last bar", sprintf("%+.2f%%", chg), if (chg >= 0) "up" else "down"),
      stat("High", sprintf("$%.2f", max(df$High))), stat("Low", sprintf("$%.2f", min(df$Low))),
      stat("Signal now", now, switch(now, Buy = "up", Sell = "down", "")))
  })

  # ---- chart panels ---------------------------------------------------------------------
  half_width <- function() switch(input$time_frame, Daily = 0.35, Weekly = 2, Monthly = 9)
  money <- function(v) paste0("$", format(v, big.mark = ",", trim = TRUE))

  price_plot <- function(df) {
    w <- half_width(); span <- diff(range(c(df$Low, df$High)))
    sw <- switch(input$time_frame, Daily = 0.5, Weekly = 3.5, Monthly = 15)   # shading fills the gaps
    g <- ggplot(df, aes(Date))
    up <- df$Close >= df$Open

    # Step 2: chart type
    if (input$chart_type == "Candlestick") {
      g <- g + geom_segment(aes(xend = Date, y = Low, yend = High), colour = ifelse(up, "#15803d", "#b91c1c")) +
        geom_rect(aes(xmin = Date - w, xmax = Date + w, ymin = pmin(Open, Close), ymax = pmax(Open, Close)),
                  fill = ifelse(up, "#15803d", "#b91c1c"))
    } else if (input$chart_type == "OHLC bars") {
      col <- ifelse(up, "#15803d", "#b91c1c")
      g <- g + geom_segment(aes(xend = Date, y = Low, yend = High), colour = col) +
        geom_segment(aes(x = Date - w, xend = Date, y = Open, yend = Open), colour = col) +
        geom_segment(aes(x = Date, xend = Date + w, y = Close, yend = Close), colour = col)
    } else {
      g <- g + geom_line(aes(y = Close), colour = "#1e2a4a", linewidth = 0.8)
    }

    # Step 3: overlays
    keys <- character(0)
    if ("Moving Averages" %in% input$indicators) {
      g <- g + geom_line(aes(y = MA_short, colour = "Short MA"), na.rm = TRUE, linewidth = 0.8) +
               geom_line(aes(y = MA_long, colour = "Long MA"), na.rm = TRUE, linewidth = 0.8)
      keys <- c(keys, "Short MA", "Long MA")
    }
    if ("Bollinger Bands" %in% input$indicators) {
      g <- g + geom_ribbon(aes(ymin = BB_bottom, ymax = BB_top), fill = "#6366f1", alpha = 0.12, na.rm = TRUE) +
               geom_line(aes(y = BB_top, colour = "Bollinger bands"), na.rm = TRUE, linewidth = 0.4) +
               geom_line(aes(y = BB_bottom, colour = "Bollinger bands"), na.rm = TRUE, linewidth = 0.4)
      keys <- c(keys, "Bollinger bands")
    }

    # Step 4: signals - shaded background and arrows where the signal changes
    if (isTRUE(input$shade)) {
      # one band for each run of consecutive Buy (or Sell) bars
      run <- cumsum(c(TRUE, df$Signal[-1] != df$Signal[-nrow(df)]))
      bands <- do.call(rbind, lapply(split(df, run), function(g)
        data.frame(from = min(g$Date) - sw, to = max(g$Date) + sw, Signal = g$Signal[1])))
      bands <- bands[bands$Signal != "Hold", ]
      if (nrow(bands) > 0)
        g <- g + geom_rect(data = bands, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf),
                           inherit.aes = FALSE, fill = ifelse(bands$Signal == "Buy", "#22c55e", "#ef4444"), alpha = 0.10)
    }
    if (isTRUE(input$arrows)) {
      ch <- df[df$Change, ]
      buys <- ch[ch$Signal == "Buy", ]; sells <- ch[ch$Signal == "Sell", ]
      ar <- grid::arrow(length = grid::unit(0.14, "cm"), type = "closed")
      if (nrow(buys) > 0) g <- g +
        geom_segment(data = buys, aes(xend = Date, y = Low - 0.09 * span, yend = Low - 0.01 * span),
                     colour = "#15803d", arrow = ar, linewidth = 0.8) +
        geom_text(data = buys, aes(y = Low - 0.12 * span, label = "BUY"), colour = "#15803d", size = 3.2, fontface = "bold")
      if (nrow(sells) > 0) g <- g +
        geom_segment(data = sells, aes(xend = Date, y = High + 0.09 * span, yend = High + 0.01 * span),
                     colour = "#b91c1c", arrow = ar, linewidth = 0.8) +
        geom_text(data = sells, aes(y = High + 0.12 * span, label = "SELL"), colour = "#b91c1c", size = 3.2, fontface = "bold")
    }

    palette <- c("Short MA" = "#2563eb", "Long MA" = "#f59e0b", "Bollinger bands" = "#6366f1")
    g + scale_colour_manual(values = palette, breaks = keys, name = NULL) +
      scale_y_continuous(labels = money, expand = expansion(mult = 0.12)) +
      labs(title = paste0(sym_now(), "  |  ", input$chart_type, ", ", tolower(input$time_frame)),
           subtitle = paste("Rule:", input$rule), x = NULL, y = "Price") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", plot.title = element_text(face = "bold", colour = "#1e2a4a"))
  }

  volume_plot <- function(df) {
    ggplot(df, aes(Date, Volume / 1e6)) +
      geom_col(width = half_width() * 2, fill = ifelse(df$Close >= df$Open, "#86efac", "#fca5a5")) +
      labs(x = NULL, y = "Volume (M)") + theme_minimal(base_size = 13)
  }

  rsi_plot <- function(df) {
    lo <- input$rsi_low; hi <- input$rsi_high
    ggplot(df, aes(Date, RSI)) +
      annotate("rect", xmin = min(df$Date), xmax = max(df$Date), ymin = hi, ymax = 100, fill = "#ef4444", alpha = 0.09) +
      annotate("rect", xmin = min(df$Date), xmax = max(df$Date), ymin = 0, ymax = lo, fill = "#22c55e", alpha = 0.09) +
      geom_hline(yintercept = c(lo, hi), linetype = "dotted", colour = "grey45") +
      geom_line(colour = "#7c3aed", linewidth = 0.8, na.rm = TRUE) +
      scale_y_continuous(limits = c(0, 100), breaks = c(0, lo, 50, hi, 100)) +
      labs(x = NULL, y = paste0("RSI (", input$rsi_period, ")")) + theme_minimal(base_size = 13)
  }

  macd_plot <- function(df) {
    ggplot(df, aes(Date)) +
      geom_col(aes(y = MACD_hist), width = half_width() * 2, position = "identity", alpha = 0.55,
               fill = ifelse(df$MACD_hist >= 0, "#22c55e", "#ef4444")) +
      geom_line(aes(y = MACD, colour = "MACD"), linewidth = 0.8) +
      geom_line(aes(y = MACD_signal, colour = "Signal line"), linewidth = 0.8) +
      geom_hline(yintercept = 0, colour = "grey50") +
      scale_colour_manual(values = c("MACD" = "#2563eb", "Signal line" = "#f59e0b"), name = NULL) +
      labs(x = NULL, y = "MACD") + theme_minimal(base_size = 13) + theme(legend.position = "none")
  }

  # Stack the panels so the date axes line up
  stack <- function(panels, heights, xlim) {
    panels <- lapply(panels, function(p) p + coord_cartesian(xlim = xlim))
    if (length(panels) == 1) return(print(panels[[1]]))
    k <- length(panels)
    for (i in seq_len(k - 1)) panels[[i]] <- panels[[i]] + theme(axis.text.x = element_blank())
    grob <- do.call(rbind, c(lapply(panels, ggplotGrob), list(size = "max")))
    rows <- unique(grob$layout$t[grepl("^panel", grob$layout$name)])
    grob$heights[rows] <- grid::unit(heights, "null")
    grid::grid.newpage(); grid::grid.draw(grob)
  }

  chosen_panels <- reactive({
    on <- input$indicators
    c("price", if ("Volume" %in% on) "volume", if ("RSI" %in% on) "rsi", if ("MACD" %in% on) "macd")
  })

  output$chart <- renderPlot({
    df <- analysed(); which <- chosen_panels()
    builders <- list(price = price_plot, volume = volume_plot, rsi = rsi_plot, macd = macd_plot)
    share <- c(price = 4, volume = 1.1, rsi = 1.5, macd = 1.7)
    stack(lapply(which, function(k) builders[[k]](df)), share[which], range(df$Date))
  }, height = function() {
    extra <- c(volume = 120, rsi = 160, macd = 180)
    460 + sum(extra[intersect(names(extra), chosen_panels())])
  }, res = 96)

  # ---- Signals tab -----------------------------------------------------------------------
  output$signal_text <- renderUI({
    df <- analysed(); k <- table(factor(df$Signal, levels = c("Buy", "Sell", "Hold")))
    tagList(
      p(strong("Rule: "), input$rule, " | ", strong("Symbol: "), sym_now(), " | ", strong("Time frame: "), input$time_frame),
      p(span(class = "up", paste(k["Buy"], "Buy bars")), " | ", span(class = "down", paste(k["Sell"], "Sell bars")),
        " | ", paste(k["Hold"], "Hold bars"), " | ", sum(df$Change), " signal changes"),
      if (k["Buy"] + k["Sell"] == 0)
        p(class = "down", "Every bar is Hold: this rule needs more bars than the range gives. Widen the date range or use a shorter time frame."))
  })

  signal_rows <- reactive({
    df <- analysed(); out <- df[df$Change, c("Date", "Signal", "Close")]
    out[order(out$Date, decreasing = TRUE), ]
  })

  output$signal_table <- renderTable({
    out <- signal_rows()
    validate(need(nrow(out) > 0, "No Buy or Sell changes with these settings. Try other settings or a longer range."))
    out$Date <- format(out$Date); out$Close <- sprintf("$%.2f", out$Close); out
  }, striped = TRUE, spacing = "s")

  output$download_signals <- downloadHandler(
    filename = function() paste0(sym_now(), "_signals.csv"),
    content = function(file) write.csv(signal_rows(), file, row.names = FALSE))

  # ---- Portfolio tab ---------------------------------------------------------------------
  portfolio_all <- eventReactive(input$run_portfolio, {
    s <- settings(); from <- input$date_range[1]; to <- input$date_range[2]
    parts <- lapply(symbols_in_file, function(sym) {
      df <- tryCatch(load_prices(sym, input$source), error = function(e) NULL)
      if (is.null(df)) return(NULL)
      df <- df[df$Date >= from & df$Date <= to, ]
      if (nrow(df) < 5) return(NULL)
      df$Signal <- make_signal(df, input$rule, s)
      df$Growth <- 100 * (df$Close / df$Close[1] - 1)
      cbind(Symbol = sym, df)
    })
    parts <- Filter(Negate(is.null), parts)
    validate(need(length(parts) > 0, "No portfolio data could be loaded."))
    do.call(rbind, parts)
  })

  portfolio_last <- reactive({
    all <- portfolio_all()
    do.call(rbind, lapply(split(all, all$Symbol), function(g) {
      n <- nrow(g)
      data.frame(Symbol = g$Symbol[1], Close = g$Close[n], Growth = g$Growth[n], Signal = g$Signal[n])
    }))
  })

  output$portfolio_chart <- renderPlot({
    last <- portfolio_last()
    ggplot(last, aes(reorder(Symbol, Growth), Growth, fill = Signal)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = sprintf("%+.1f%%", Growth)), hjust = ifelse(last$Growth >= 0, -0.1, 1.1)) +
      scale_fill_manual(values = c(Buy = "#22c55e", Sell = "#ef4444", Hold = "#94a3b8"), drop = FALSE) +
      coord_flip() + scale_y_continuous(expand = expansion(mult = 0.18)) +
      labs(title = "Growth over the date range, coloured by the current signal", x = NULL, y = "% change") +
      theme_minimal(base_size = 13)
  })

  output$portfolio_table <- renderTable({
    last <- portfolio_last()
    data.frame(Symbol = last$Symbol, Close = sprintf("$%.2f", last$Close),
               Growth = sprintf("%+.1f%%", last$Growth), Signal = last$Signal)
  }, striped = TRUE, spacing = "s")

  # ---- Data tab ----------------------------------------------------------------------------
  output$data_table <- renderTable({
    out <- tail(analysed()[, c("Date", "Open", "High", "Low", "Close", "Volume", "RSI", "Signal")], 60)
    out$Date <- format(out$Date); out$Volume <- format(round(out$Volume), big.mark = ",", trim = TRUE)
    out[order(out$Date, decreasing = TRUE), ]
  }, striped = TRUE, spacing = "s", digits = 2)
}

shinyApp(ui, server)
