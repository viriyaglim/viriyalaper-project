# ============================================
# Volatility Forecasting for BTC
# ============================================

# 1) Libraries
library(tidyquant)
library(dplyr)
library(ggplot2)
library(rugarch)
library(xts)
library(tidyr)
library(purrr)
library(tibble)

# 2) Helper to get DAILY LOG RETURNS safely
get_logrets <- function(symbol = "BTC-USD",
                        from   = "2023-01-01",
                        to     = "2023-12-31") {
  tq_get(symbol, from = from, to = to) %>%
    arrange(date) %>%
    tq_transmute(
      select      = adjusted,
      mutate_fun  = periodReturn,
      period      = "daily",
      type        = "log",
      col_rename  = "ret"
    ) %>%
    mutate(ret = as.numeric(ret)) %>%
    tidyr::drop_na()
}

# --- fetch data
btc_ret_tbl <- get_logrets("BTC-USD", from = "2023-01-01", to = "2023-12-31")
stopifnot(nrow(btc_ret_tbl) > 50)

ret_xts <- xts(btc_ret_tbl$ret, order.by = btc_ret_tbl$date)

# 3) Model specs
spec_list <- list(
  sGARCH  = ugarchspec(
    variance.model     = list(model = "sGARCH",  garchOrder = c(1,1)),
    mean.model         = list(armaOrder = c(0,0), include.mean = TRUE),
    distribution.model = "std"
  ),
  eGARCH  = ugarchspec(
    variance.model     = list(model = "eGARCH",  garchOrder = c(1,1)),
    mean.model         = list(armaOrder = c(0,0), include.mean = TRUE),
    distribution.model = "std"
  ),
  gjrGARCH = ugarchspec(
    variance.model     = list(model = "gjrGARCH", garchOrder = c(1,1)),
    mean.model         = list(armaOrder = c(0,0), include.mean = TRUE),
    distribution.model = "std"
  )
)

# 4) Fit helper with diagnostics + forecast
fit_one <- function(name, spec, xret) {
  # center residual mean to help optimizer
  x <- scale(as.numeric(xret), center = TRUE, scale = FALSE)[, 1]
  
  fit <- try(
    ugarchfit(spec = spec, data = x, solver = "hybrid",
              fit.control = list(scale = 1)),
    silent = TRUE
  )
  
  if (inherits(fit, "try-error")) {
    return(list(name = name, ok = FALSE))
  }
  
  # in-sample conditional sigma
  insig <- as.numeric(sigma(fit))
  
  # standardized residuals + Ljung-Box on resid and squared resid
  z  <- residuals(fit, standardize = TRUE)
  p_lb_10   <- try(Box.test(z,    lag = 10, type = "Ljung-Box")$p.value, TRUE)
  p_lb2_10  <- try(Box.test(z^2,  lag = 10, type = "Ljung-Box")$p.value, TRUE)
  
  # 10-day ahead forecast
  fc <- ugarchforecast(fit, n.ahead = 10)
  fsig <- as.numeric(sigma(fc))
  
  list(
    name   = name,
    ok     = TRUE,
    fit    = fit,
    insig  = insig,
    z      = z,
    aic    = infocriteria(fit)[1],   # Akaike
    bic    = infocriteria(fit)[2],   # Bayes
    p_lb   = suppressWarnings(as.numeric(p_lb_10)),
    p_lb2  = suppressWarnings(as.numeric(p_lb2_10)),
    fsig   = fsig
  )
}

# 5) Fit all models
fits <- imap(spec_list, ~ fit_one(.y, .x, ret_xts))

# Keep only successful fits
fits_ok <- keep(fits, ~ isTRUE(.x$ok))
stopifnot(length(fits_ok) > 0)

# 6) Comparison table (AIC/BIC + residual tests)
cmp_tbl <- map_dfr(fits_ok, function(x) {
  tibble(
    Model       = x$name,
    AIC         = x$aic,
    BIC         = x$bic,
    LB_pvalue   = x$p_lb,   # residuals LB(10)
    LB2_pvalue  = x$p_lb2   # squared residuals LB(10)
  )
}) %>%
  arrange(AIC)

print(cmp_tbl)

# 7) Build volatility plot: historical σ + 10-step forecast
#    (we’ll overlay the three models; forecast window shaded)

# Historical dates for in-sample sigma
hist_dates <- btc_ret_tbl$date
# Forecast dates = next 10 calendar days (BTC trades daily)
fc_dates <- seq(from = max(hist_dates) + 1, by = "day", length.out = 10)

vol_hist_long <- map_dfr(fits_ok, function(x) {
  tibble(date = hist_dates, sigma = x$insig, Model = x$name, Type = "In-sample")
})

vol_fc_long <- map_dfr(fits_ok, function(x) {
  tibble(date = fc_dates, sigma = x$fsig, Model = x$name, Type = "Forecast")
})

vol_long <- bind_rows(vol_hist_long, vol_fc_long)

# Plot
p_vol <- ggplot(vol_long, aes(x = date, y = sigma, color = Model, linetype = Type)) +
  geom_line() +
  geom_rect(aes(xmin = min(fc_dates), xmax = max(fc_dates),
                ymin = -Inf, ymax = Inf),
            fill = "grey90", inherit.aes = FALSE, alpha = 0.5) +
  labs(title = "BTC Volatility: GARCH-family Models",
       subtitle = "Shaded region = 10-day forecast",
       x = "Date", y = "Conditional Volatility (σ_t)") +
  theme_minimal()

print(p_vol)

# 8) (Optional) Pick best-by-AIC and show its parameter summary
best_name <- cmp_tbl$Model[1]
best_fit  <- fits_ok[[ best_name ]]$fit
cat("\nBest (by AIC):", best_name, "\n\n")
show(best_fit)

