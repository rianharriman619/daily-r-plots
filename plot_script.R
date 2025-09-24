library(tidyquant)
library(TTR)
library(tidyverse)

tickers <- c(
  "AAPL","MSFT","AMZN","TSLA","NVDA","META","GOOGL","AMD","INTC","NFLX",
  "BABA","JPM","BAC","WFC","C","GS","PLTR","COIN","MSTR","HOOD",
  "AMC","GME","SNAP","UBER","LYFT","SOFI","NIO","MU","ORCL","AVGO"
)


# --- Parameters ---
forward_days        <- 365
pivot_k             <- 2
min_separation_days <- 3
bar_width           <- 0.8

# --- Helper functions ---
lagN  <- function(x, n) dplyr::lag(x, n, default = NA_real_)
leadN <- function(x, n) dplyr::lead(x, n, default = NA_real_)

less_than_all <- function(center, others) {
  rowSums((center < as.matrix(others)) | is.na(as.matrix(others))) == ncol(others)
}
greater_than_all <- function(center, others) {
  rowSums((center > as.matrix(others)) | is.na(as.matrix(others))) == ncol(others)
}

suppress_noise <- function(idx_vec, value_vec, min_gap) {
  keep <- rep(FALSE, length(idx_vec))
  last_kept <- -Inf
  cand_idx <- which(idx_vec)
  if (length(cand_idx) == 0) return(keep)
  for (i in cand_idx) {
    if (i - last_kept >= min_gap) {
      keep[i] <- TRUE
      last_kept <- i
    }
  }
  keep
}

# --- Function to process one ticker ---
process_ticker <- function(tkr) {
  test <- tq_get(tkr, from = Sys.Date()-years(4), to = Sys.Date()) %>%
    select(symbol, date, open, high, low, close, volume) 
  
  data <- test
  
  # Local extrema
  neighbors_lag  <- map_dfc(1:pivot_k, ~lagN(data$close, .x)) %>% setNames(paste0("lag", 1:pivot_k))
  neighbors_lead <- map_dfc(1:pivot_k, ~leadN(data$close, .x)) %>% setNames(paste0("lead", 1:pivot_k))
  data <- bind_cols(data, neighbors_lag, neighbors_lead)
  
  others_mat <- bind_cols(select(data, starts_with("lag")),
                          select(data, starts_with("lead")))
  
  data <- data %>%
    mutate(
      local_bottom_raw = less_than_all(close, others_mat),
      local_top_raw    = greater_than_all(close, others_mat),
      local_bottom     = suppress_noise(local_bottom_raw, close, min_separation_days),
      local_top        = suppress_noise(local_top_raw,    close, min_separation_days),
      buy_signal       = local_bottom,
      sell_signal      = FALSE
    )
  
  # Build trade windows
  trade_windows <- tibble(
    start        = as.Date(character()),
    end          = as.Date(character()),
    buy_price    = numeric(),
    sell_price   = numeric(),
    min_price    = numeric(),
    profit       = logical(),
    drawdown_pct = numeric()
  )
  
  n <- nrow(data)
  last_exit <- 0
  top_idx <- which(data$local_top)
  
  for (i in which(data$buy_signal)) {
    if (i <= last_exit) next
    end_idx <- min(n, i + forward_days)
    cand_tops <- top_idx[top_idx > i & top_idx <= end_idx]
    if (length(cand_tops) > 0) {
      j <- cand_tops[1]
      data$sell_signal[j] <- TRUE
      last_exit <- j
      
      buy_price  <- data$close[i]
      sell_price <- data$close[j]
      min_price  <- min(data$low[i:j], na.rm = TRUE)
      
      trade_profit <- (!is.na(sell_price) && !is.na(buy_price) && sell_price > buy_price)
      drawdown_pct <- (min_price - buy_price) / buy_price * 100
      
      trade_windows <- bind_rows(
        trade_windows,
        tibble(
          start        = data$date[i],
          end          = data$date[j],
          buy_price    = buy_price,
          sell_price   = sell_price,
          min_price    = min_price,
          profit       = trade_profit,
          drawdown_pct = drawdown_pct
        )
      )
    }
  }
  
  # Filter to last month
  data_last_month <- data %>% filter(date >= max(date) - 20)
  trades_last_month <- trade_windows %>% filter(end >= max(data$date) - 20)
  
  # Return both candles and trades with ticker label
  list(
    candles = data_last_month %>% mutate(symbol = tkr),
    trades  = trades_last_month %>% mutate(symbol = tkr)
  )
}

# --- Run for all tickers ---
results <- map(tickers, process_ticker)

candles_all <- bind_rows(map(results, "candles"))
trades_all  <- bind_rows(map(results, "trades"))

# --- Plot faceted by ticker ---
# Find the actual min and max dates in your plotted data
x_min <- min(candles_all$date, na.rm = TRUE) -1 
x_max <- max(candles_all$date, na.rm = TRUE) +1
today_str <- format(Sys.Date(), "%d %B %Y")

p <- ggplot(candles_all, aes(x = date)) +
  geom_rect(data = trades_all,
            aes(xmin = start, xmax = end,
                ymin = -Inf, ymax = Inf,
                fill = ifelse(profit, "positive", "negative")),
            alpha = 0.15, inherit.aes = FALSE) +
  geom_segment(aes(y = low, yend = high, xend = date), color = "black") +
  geom_rect(aes(xmin = date - bar_width/2, xmax = date + bar_width/2,
                ymin = pmin(open, close), ymax = pmax(open, close),
                fill = ifelse(close >= open, "increasing", "decreasing")),
            color = "black") +
  geom_point(data = filter(candles_all, buy_signal),
             aes(y = low * 0.98),
             shape = 24, size = 3, fill = "limegreen", color = "black") +
  geom_point(data = filter(candles_all, sell_signal),
             aes(y = high * 1.02),
             shape = 25, size = 3, fill = "#CC0000", color = "black") +
  scale_fill_manual(values = c("positive" = "green",
                               "negative" = "red",
                               "increasing" = "forestgreen",
                               "decreasing" = "red")) +
  scale_x_date(limits = c(x_min, x_max),   # << tighten to actual data
               date_breaks = "2 days", date_labels = "%b %d") +
  labs(title = labs(title = paste0(today_str, "- Last 20 Days")),
       x = "Date", y = "Price (USD)") +
  facet_wrap(~symbol, scales = "free_y") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    panel.grid.major.x = element_line(color = "grey40", linewidth = 0.4),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 10),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.minor = element_line(color = "grey50", linewidth = 0.2)
  )


# --- Save with timestamp ---
# --- Build timestamped filename inside that folder ---
if (!dir.exists("ultimate_plots")) {
  dir.create("ultimate_plots")
}

timestamp <- format(Sys.time(), "%y%d%m")
filename  <- file.path("ultimate_plots",
                       paste0(timestamp,"_candlestick_trades", ".png"))

# --- Save plot ---
ggsave(filename, plot = p, width = 14, height = 8, dpi = 300)


