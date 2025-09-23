# ============================================
# Full Fama-French Analysis on a Portfolio
# ============================================

# 1) Libraries
library(tidyquant)
library(dplyr)
library(ggplot2)
library(FFdownload)
library(broom)
library(knitr)
library(tidyr)
library(lubridate)

# --------------------------
# 2) Portfolio monthly returns
# --------------------------
symbols <- c("AAPL", "MSFT", "TSLA")
start   <- as.Date("2020-01-01")
end     <- as.Date("2023-12-31")

port_tbl <- tq_get(symbols, from = start, to = end) %>%
  arrange(date) %>%
  group_by(symbol) %>%
  tq_transmute(
    select      = adjusted,
    mutate_fun  = periodReturn,
    period      = "monthly",
    type        = "log",
    col_rename  = "ret"
  ) %>%
  ungroup() %>%
  group_by(date) %>%
  summarize(port_ret = mean(ret), .groups = "drop")

stopifnot(nrow(port_tbl) > 0)

# --------------------------
# 3) Fama–French 5 factors (Monthly) via FFdownload
#    (robust to different package versions)
# --------------------------
ff_obj <- FFdownload(
  download = TRUE, 
  tempd = tempdir(), 
  destdir = tempdir()
)

# Helper: pick the FF 5 Factors (2x3) monthly dataset regardless of naming
get_ff5_monthly <- function(ff_obj) {
  # Try to locate a subset whose name mentions "5_Factors_2x3"
  subset_names <- names(ff_obj$subsets)
  idx <- grep("5_Factors_2x3", subset_names, ignore.case = TRUE)
  if (length(idx) == 0) stop("Could not find 'F-F_Research_Data_5_Factors_2x3' in FFdownload subsets.")
  ff5_raw <- ff_obj$subsets[[ idx[1] ]]$data
  
  # Handle possible column names for date
  if ("Date" %in% names(ff5_raw)) {
    # Kenneth French style YYYYMM integer under "Date"
    dt <- as.character(ff5_raw$Date)
  } else if ("date" %in% names(ff5_raw)) {
    dt <- as.character(ff5_raw$date)
  } else {
    stop("FF5 data doesn't have 'Date' or 'date' column.")
  }
  
  # Convert YYYYMM -> Date (first of month)
  # Normalize dt to 6 chars then append "01"
  dt <- sprintf("%06d", as.integer(dt))
  ff5_raw$date <- as.Date(paste0(dt, "01"), format = "%Y%m%d")
  
  # Standardize column names
  nm <- names(ff5_raw)
  nm <- sub("^Mkt\\.RF$", "Mkt-RF", nm)   # sometimes 'Mkt.RF'
  names(ff5_raw) <- nm
  
  # Keep and scale (% -> decimals)
  keep <- c("date", "Mkt-RF", "SMB", "HML", "RMW", "CMA", "RF")
  miss <- setdiff(keep, names(ff5_raw))
  if (length(miss) > 0) stop(paste("FF5 columns missing:", paste(miss, collapse = ", ")))
  
  ff5 <- ff5_raw %>%
    select(all_of(keep)) %>%
    mutate(
      `Mkt-RF` = as.numeric(`Mkt-RF`)/100,
      SMB      = as.numeric(SMB)/100,
      HML      = as.numeric(HML)/100,
      RMW      = as.numeric(RMW)/100,
      CMA      = as.numeric(CMA)/100,
      RF       = as.numeric(RF)/100
    )
  
  # Keep desired date range
  ff5 <- ff5 %>% filter(date >= floor_date(start, "month"),
                        date <= floor_date(end,   "month"))
  ff5
}

ff5_factors <- get_ff5_monthly(ff_obj) %>%
  rename(MKT = `Mkt-RF`)

stopifnot(nrow(ff5_factors) > 0)

# --------------------------
# 4) Merge portfolio with factors, build excess returns
# --------------------------
reg_data <- inner_join(port_tbl, ff5_factors, by = "date") %>%
  mutate(excess_ret = port_ret - RF)

stopifnot(nrow(reg_data) > 0)

# --------------------------
# 5) Run CAPM / FF3 / FF5 regressions
# --------------------------
capm_fit <- lm(excess_ret ~ MKT, data = reg_data)
ff3_fit  <- lm(excess_ret ~ MKT + SMB + HML, data = reg_data)
ff5_fit  <- lm(excess_ret ~ MKT + SMB + HML + RMW + CMA, data = reg_data)

# --------------------------
# 6) Summarize results
# --------------------------
capm_res <- broom::tidy(capm_fit) %>% mutate(Model = "CAPM")
ff3_res  <- broom::tidy(ff3_fit)  %>% mutate(Model = "FF3")
ff5_res  <- broom::tidy(ff5_fit)  %>% mutate(Model = "FF5")

all_res <- bind_rows(capm_res, ff3_res, ff5_res) %>%
  select(Model, term, estimate, std.error, statistic, p.value)

knitr::kable(all_res, digits = 4, caption = "Fama-French Regression Results")

# Model fit comparison
fit_tbl <- tibble(
  Model = c("CAPM", "FF3", "FF5"),
  Adj_R2 = c(summary(capm_fit)$adj.r.squared,
             summary(ff3_fit)$adj.r.squared,
             summary(ff5_fit)$adj.r.squared),
  AIC = c(AIC(capm_fit), AIC(ff3_fit), AIC(ff5_fit)),
  BIC = c(BIC(capm_fit), BIC(ff3_fit), BIC(ff5_fit))
)
knitr::kable(fit_tbl, digits = 4, caption = "Model Fit Comparison")

# --------------------------
# 7) Visualize FF5 factor loadings (betas)
# --------------------------
ff5_loadings <- all_res %>%
  filter(Model == "FF5", term != "(Intercept)")

ggplot(ff5_loadings, aes(x = term, y = estimate, fill = term)) +
  geom_col() +
  labs(title = "FF5 Factor Loadings (Equal-Weight Portfolio)",
       x = "Factor", y = "Loading (Beta)") +
  theme_minimal()
