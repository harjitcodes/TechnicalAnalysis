# Assignment 6 - Stock Portfolio Dashboard (R Shiny)

Author: Harjit Gill - BDA400 Data Science Tools and Techniques

## How to run
1. Open `app.R` in RStudio.
2. Click **Run App** (or run `shiny::runApp("Assignment6")`). Missing packages (shiny, ggplot2, quantmod) are installed on the first run.
3. Choose **Yahoo Finance** (internet needed) or **Demo data (offline)** as the data source.

## Features
- **Step 1 - data:** prices from Yahoo Finance through quantmod; stock symbols read from `portfolio.txt` (AAPL, MA, XOM, COST, TSLA); offline demo data as a fallback.
- **Step 2 - visualization:** "Select Date Range" and "Select Time Frame" (Daily / Weekly / Monthly) widgets; candlestick, OHLC-bar and line charts.
- **Step 3 - indicators:** Moving Averages (SMA/EMA), Bollinger Bands, Volume, RSI and MACD panels, calculated with my Assignment 5 base-R functions.
- **Step 4 - trading rules:** three rules (short vs long MA trend, RSI turn-around, majority vote of MA + RSI + MACD) give a Buy / Sell / Hold signal for every bar, shown as shaded periods and BUY / SELL arrows. The Signals tab lists the changes and exports them to CSV.
- **Portfolio tab:** growth of every stock in `portfolio.txt` over the chosen date range, coloured by its current signal.

## Files
- `app.R` - the dashboard
- `portfolio.txt` - stock symbols, one per line
