#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(zoo)
  library(forecast)
  library(urca)
  library(xts)
  library(tidyverse)
  library(lubridate)
  library(vars)
  library(aTSA)
  library(tseries)
  library(stats)
  library(tsDyn)
  library(texreg)
})

options(stringsAsFactors = FALSE)

data_dir <- file.path("data", "raw")
sp500_file <- file.path(data_dir, "S&P 500.csv")
indpro_file <- file.path(data_dir, "INDPRO.csv")

if (!file.exists(sp500_file) || !file.exists(indpro_file)) {
  stop("Missing CSV files. Please read data/raw/README.md for download instructions.")
}

date_start <- as.Date("1991-01-01")
date_end <- as.Date("2001-01-01")

read_sp500 <- function(path) {
  read.csv(path) %>%
    dplyr::select(Date, Close) %>%
    mutate(Date = as.Date(Date)) %>%
    arrange(Date) %>%
    filter(Date >= date_start, Date <= date_end) %>%
    mutate(LC = log(Close))
}

read_indpro <- function(path) {
  read.csv(path) %>%
    mutate(Date = as.Date(observation_date)) %>%
    dplyr::select(Date, INDPRO) %>%
    arrange(Date) %>%
    filter(Date >= date_start, Date <= date_end) %>%
    mutate(ID = log(INDPRO))
}

df_sp <- read_sp500(sp500_file)
df_ip <- read_indpro(indpro_file)

get_closest_value <- function(date, df_sp) {
  idx <- which.min(abs(as.numeric(df_sp$Date - date)))
  df_sp$LC[idx]
}

df <- df_ip %>%
  mutate(LC = sapply(Date, get_closest_value, df_sp = df_sp)) %>%
  dplyr::select(Date, ID, LC)

# --- Table 1: Year-by-Year Annual Returns ---

df_sp_ann <- df_sp %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(SP_Start = first(exp(LC)), SP_End = last(exp(LC)), .groups = "drop") %>%
  mutate(SP_Return = (SP_End / SP_Start - 1) * 100)

df_ip_ann <- df_ip %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(IPI_Start = first(exp(ID)), IPI_End = last(exp(ID)), .groups = "drop") %>%
  mutate(IPI_Return = (IPI_End / IPI_Start - 1) * 100)

df_returns <- left_join(df_sp_ann %>% select(Year, SP_Return),
                        df_ip_ann %>% select(Year, IPI_Return), by = "Year")

print("--- Table 1: Year-by-Year Annual Returns for S&P 500 and IPI (1991-2001) ---")
print(df_returns)

plot_data <- df %>%
  mutate(norm_LC = (LC / first(LC)) * 100,
         norm_ID = (ID / first(ID)) * 100)

fig1 <- ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = norm_LC, color = "S&P 500 (Normalised Log)"), size = 1) +
  geom_line(aes(y = norm_ID, color = "Industrial Production (Normalised Log)"), size = 1) +
  labs(title = "Normalised (Log) S&P 500 vs Industrial Production (1991-2001)",
       x = "Date", y = "Index (Base = 100)", color = "Series") +
  theme_minimal() +
  scale_color_manual(values = c("S&P 500 (Normalised Log)" = "blue",
                                "Industrial Production (Normalised Log)" = "red"))

ggsave(filename = file.path("output", "figure_01_normalised_series.png"), fig1,
       width = 9, height = 5)

LC_xts <- xts(df$LC, order.by = df$Date)
ID_xts <- xts(df$ID, order.by = df$Date)
df_diff <- diff(cbind(LC_xts, ID_xts))[-1, ]

adf_star_string <- function(urdf_obj) {
  test_stat <- urdf_obj@teststat[1]
  cvals <- urdf_obj@cval[1, ]
  star <- ""
  if (!is.na(test_stat) && !is.na(cvals["1pct"]) && test_stat < cvals["1pct"]) {
    star <- "***"
  } else if (!is.na(test_stat) && !is.na(cvals["5pct"]) && test_stat < cvals["5pct"]) {
    star <- "**"
  } else if (!is.na(test_stat) && !is.na(cvals["10pct"]) && test_stat < cvals["10pct"]) {
    star <- "*"
  }
  paste0(round(test_stat, 3), star)
}

extract_adf_results <- function(x_xts) {
  adf_level_none <- ur.df(x_xts, type = "none", selectlags = "AIC")
  adf_level_drift <- ur.df(x_xts, type = "drift", selectlags = "AIC")
  adf_level_trend <- ur.df(x_xts, type = "trend", selectlags = "AIC")

  x_diff <- diff(x_xts) %>% na.omit()
  adf_diff_none <- ur.df(x_diff, type = "none", selectlags = "AIC")
  adf_diff_drift <- ur.df(x_diff, type = "drift", selectlags = "AIC")
  adf_diff_trend <- ur.df(x_diff, type = "trend", selectlags = "AIC")

  list(
    Level_None = adf_star_string(adf_level_none),
    Level_Intercept = adf_star_string(adf_level_drift),
    Level_Intercept_Trend = adf_star_string(adf_level_trend),
    Diff_None = adf_star_string(adf_diff_none),
    Diff_Intercept = adf_star_string(adf_diff_drift),
    Diff_Intercept_Trend = adf_star_string(adf_diff_trend)
  )
}

var_names <- c("ID", "LC")
results_list_adf <- lapply(var_names, function(var) {
  extract_adf_results(get(paste0(var, "_xts")))
}) %>% setNames(var_names)

adf_results_df <- data.frame(
  Variable = var_names,
  Level_None = sapply(results_list_adf, `[[`, "Level_None"),
  Level_Intercept = sapply(results_list_adf, `[[`, "Level_Intercept"),
  Level_Intercept_Trend = sapply(results_list_adf, `[[`, "Level_Intercept_Trend"),
  Diff_None = sapply(results_list_adf, `[[`, "Diff_None"),
  Diff_Intercept = sapply(results_list_adf, `[[`, "Diff_Intercept"),
  Diff_Intercept_Trend = sapply(results_list_adf, `[[`, "Diff_Intercept_Trend"),
  row.names = NULL
)

print(adf_results_df)

extract_pp_results <- function(x_xts, type = "Z_tau") {
  x <- as.numeric(coredata(x_xts))
  pp_level <- aTSA::pp.test(x, type = type)
  x_diff <- diff(x) %>% na.omit()
  pp_diff <- aTSA::pp.test(x_diff, type = type)

  add_stars <- function(pval) {
    if (is.na(pval)) return("")
    if (pval <= 0.01) return("***")
    else if (pval <= 0.05) return("**")
    else if (pval <= 0.10) return("*")
    else return("")
  }

  level_none <- paste0(round(pp_level[1, 2], 3), add_stars(pp_level[1, 3]))
  level_drift <- paste0(round(pp_level[2, 2], 3), add_stars(pp_level[2, 3]))
  level_trend <- paste0(round(pp_level[3, 2], 3), add_stars(pp_level[3, 3]))

  diff_none <- paste0(round(pp_diff[1, 2], 3), add_stars(pp_diff[1, 3]))
  diff_drift <- paste0(round(pp_diff[2, 2], 3), add_stars(pp_diff[2, 3]))
  diff_trend <- paste0(round(pp_diff[3, 2], 3), add_stars(pp_diff[3, 3]))

  c(
    Level_None = level_none,
    Level_Intercept = level_drift,
    Level_Intercept_Trend = level_trend,
    Diff_None = diff_none,
    Diff_Intercept = diff_drift,
    Diff_Intercept_Trend = diff_trend
  )
}

results_list_pp <- lapply(var_names, function(var) {
  extract_pp_results(get(paste0(var, "_xts")), type = "Z_tau")
}) %>% setNames(var_names)

pp_results_df <- data.frame(
  Variable = var_names,
  Level_None = sapply(results_list_pp, `[[`, "Level_None"),
  Level_Intercept = sapply(results_list_pp, `[[`, "Level_Intercept"),
  Level_Intercept_Trend = sapply(results_list_pp, `[[`, "Level_Intercept_Trend"),
  Diff_None = sapply(results_list_pp, `[[`, "Diff_None"),
  Diff_Intercept = sapply(results_list_pp, `[[`, "Diff_Intercept"),
  Diff_Intercept_Trend = sapply(results_list_pp, `[[`, "Diff_Intercept_Trend"),
  row.names = NULL
)

print(pp_results_df)

extract_kpss_results <- function(x_xts) {
  x <- as.numeric(coredata(x_xts))

  add_stars <- function(pval) {
    if (is.na(pval)) return("")
    if (pval <= 0.01) return("***")
    else if (pval <= 0.05) return("**")
    else if (pval <= 0.10) return("*")
    else return("")
  }

  level_intercept_test <- tseries::kpss.test(x, null = "Level", lshort = TRUE)
  level_intercept <- paste0(round(level_intercept_test$statistic, 3),
                            add_stars(level_intercept_test$p.value))

  level_trend_test <- tseries::kpss.test(x, null = "Trend", lshort = TRUE)
  level_trend <- paste0(round(level_trend_test$statistic, 3),
                        add_stars(level_trend_test$p.value))

  x_diff <- diff(x) %>% na.omit()
  diff_intercept_test <- tseries::kpss.test(x_diff, null = "Level", lshort = TRUE)
  diff_intercept <- paste0(round(diff_intercept_test$statistic, 3),
                           add_stars(diff_intercept_test$p.value))

  diff_trend_test <- tseries::kpss.test(x_diff, null = "Trend", lshort = TRUE)
  diff_trend <- paste0(round(diff_trend_test$statistic, 3),
                       add_stars(diff_trend_test$p.value))

  c(
    level_intercept = level_intercept,
    level_trend = level_trend,
    diff_intercept = diff_intercept,
    diff_trend = diff_trend
  )
}

results_list_kpss <- lapply(var_names, function(var) {
  extract_kpss_results(get(paste0(var, "_xts")))
}) %>% setNames(var_names)

kpss_results_df <- data.frame(
  Variable = var_names,
  level_intercept = sapply(results_list_kpss, `[[`, "level_intercept"),
  level_trend = sapply(results_list_kpss, `[[`, "level_trend"),
  diff_intercept = sapply(results_list_kpss, `[[`, "diff_intercept"),
  diff_trend = sapply(results_list_kpss, `[[`, "diff_trend"),
  row.names = NULL
)

print("--- Appendix Table: KPSS Test Results Summary ---")
print(kpss_results_df)

alt_lags <- c(6, 12, floor(nrow(LC_xts)^0.25))
for (var in var_names) {
  var_xts <- get(paste0(var, "_xts"))
  var_diff_xts <- diff(var_xts) %>% na.omit()
  cat(paste("\n--- Variable:", var, "---\n"))
  for (lag in alt_lags) {
    cat(paste("\nLag =", lag, "\n"))
    cat("Level Tests:\n")
    print(summary(ur.df(var_xts, type = "none", lags = lag)))
    print(summary(ur.df(var_xts, type = "drift", lags = lag)))
    print(summary(ur.df(var_xts, type = "trend", lags = lag)))
    cat("\nDifference Tests:\n")
    print(summary(ur.df(var_diff_xts, type = "none", lags = lag)))
    print(summary(ur.df(var_diff_xts, type = "drift", lags = lag)))
    print(summary(ur.df(var_diff_xts, type = "trend", lags = lag)))
    cat("--------------------\n")
  }
}

png(file.path("output", "figure_02_acf_diffs.png"), width = 900, height = 450)
par(mfrow = c(1, 2))
acf(df_diff$LC, main = "ACF of Differenced S&P 500", na.action = na.pass, lag.max = 12)
acf(df_diff$ID, main = "ACF of Differenced IPI", na.action = na.pass, lag.max = 12)
dev.off()

png(file.path("output", "figure_03_differences.png"), width = 900, height = 600)
par(mfrow = c(2, 1))
plot(df_diff$LC, main = "First Differences of Log S&P 500 (dLC)", ylab = "dLC", xlab = "Date")
plot(df_diff$ID, main = "First Differences of Log IPI (dID)", ylab = "dID", xlab = "Date")
dev.off()

print(t.test(df_diff$LC, mu = 0))
print(t.test(df_diff$ID, mu = 0))
print(paste("Mean of differenced LC:", round(mean(df_diff$LC, na.rm = TRUE), 5)))
print(paste("Mean of differenced ID:", round(mean(df_diff$ID, na.rm = TRUE), 5)))

dfx <- cbind(LC_xts, ID_xts)

lag_selection_const <- VARselect(df_diff, lag.max = 12, type = "const")
lag_selection_none <- VARselect(df_diff, lag.max = 12, type = "none")
print(lag_selection_const)
print(lag_selection_none)

johansen_k3 <- ca.jo(dfx, type = "trace", K = 3, ecdet = "const", spec = "transitory")
vecm_k3 <- cajorls(johansen_k3, r = 1)

johansen_k2 <- ca.jo(dfx, type = "trace", K = 2, ecdet = "const", spec = "transitory")
vecm_k2 <- cajorls(johansen_k2, r = 1)

coef_count_k3 <- length(coef(vecm_k3$rlm))
coef_count_k2 <- length(coef(vecm_k2$rlm))

res_k3 <- residuals(vecm_k3$rlm)
res_k2 <- residuals(vecm_k2$rlm)

n_k3 <- nrow(res_k3)
n_k2 <- nrow(res_k2)

n_vars <- ncol(res_k3)

sigma_k3 <- crossprod(res_k3) / n_k3
sigma_k2 <- crossprod(res_k2) / n_k2

loglik_k3 <- -0.5 * n_k3 * (n_vars * log(2 * pi) + log(det(sigma_k3)) + n_vars)
loglik_k2 <- -0.5 * n_k2 * (n_vars * log(2 * pi) + log(det(sigma_k2)) + n_vars)

n_params_k3 <- coef_count_k3 + n_vars * (n_vars + 1) / 2
n_params_k2 <- coef_count_k2 + n_vars * (n_vars + 1) / 2

aic_k3 <- -2 * loglik_k3 + 2 * n_params_k3
aic_k2 <- -2 * loglik_k2 + 2 * n_params_k2

bic_k3 <- -2 * loglik_k3 + log(n_k3) * n_params_k3
bic_k2 <- -2 * loglik_k2 + log(n_k2) * n_params_k2

lr_stat <- 2 * (loglik_k3 - loglik_k2)
df_lr <- n_params_k3 - n_params_k2
p_value <- 1 - pchisq(lr_stat, df_lr)

print(aic_k3 - aic_k2)
print(bic_k3 - bic_k2)
print(p_value)
print(lr_stat)

johansen <- johansen_k2
print(summary(johansen))

test_stats <- johansen@teststat
critical_vals <- johansen@cval
lambda <- johansen@lambda
lambda_subset <- lambda[1:nrow(critical_vals)]

results <- cbind(test_stats, critical_vals, lambda = lambda_subset)
results_ordered <- results[c(2, 1), , drop = FALSE]
rownames(results_ordered) <- c("r <= 1", "r = 0")

results_df <- data.frame(
  Hypothesis = rownames(results_ordered),
  Trace.Statistic = paste0(
    format(round(results_ordered[, 1], 2), nsmall = 2),
    ifelse(!is.na(results_ordered[, 1]) & !is.na(results_ordered[, 4]) & results_ordered[, 1] > results_ordered[, 4], "***",
           ifelse(!is.na(results_ordered[, 1]) & !is.na(results_ordered[, 3]) & results_ordered[, 1] > results_ordered[, 3], "**",
                  ifelse(!is.na(results_ordered[, 1]) & !is.na(results_ordered[, 2]) & results_ordered[, 1] > results_ordered[, 2], "*", "")))
  ),
  Critical.Value.10. = format(round(results_ordered[, 2], 2), nsmall = 2),
  Critical.Value.5. = format(round(results_ordered[, 3], 2), nsmall = 2),
  Critical.Value.1. = format(round(results_ordered[, 4], 2), nsmall = 2),
  Eigenvalue = format(round(results_ordered[, 5], 4), nsmall = 4),
  stringsAsFactors = FALSE,
  row.names = NULL
)

print(results_df)
print(paste("Lag order (K):", johansen@lag))
print(paste("Deterministic terms (ecdet):", johansen@ecdet))
print(paste("Sample size:", nrow(johansen@x)))

coint_vec <- johansen@V[, 1]
norm_coint_vec <- coint_vec / coint_vec[1]
constant_term_eq1 <- -norm_coint_vec[3]
id_coefficient_eq1 <- -norm_coint_vec[2]

vecm_model <- vecm_k2
vecm_as_var <- vec2var(johansen, r = 1)

serial_test <- serial.test(vecm_as_var, lags.pt = 16, type = "PT.asymptotic")
normality_test <- normality.test(vecm_as_var)
arch_test <- vars::arch.test(vecm_as_var, lags.multi = 5)

pt_stat <- as.numeric(serial_test$serial$statistic)
pt_df <- as.numeric(serial_test$serial$parameter)
pt_pval <- as.numeric(serial_test$serial$p.value)

jb_stat <- tryCatch(as.numeric(normality_test$jb.mul$JB$statistic[1, 1]), error = function(e) NA)
jb_df <- tryCatch(as.numeric(normality_test$jb.mul$JB$parameter), error = function(e) NA)
jb_pval <- tryCatch(as.numeric(normality_test$jb.mul$JB$p.value[1, 1]), error = function(e) NA)

sk_stat <- tryCatch(as.numeric(normality_test$jb.mul$Skewness$statistic[1, 1]), error = function(e) NA)
sk_df <- tryCatch(as.numeric(normality_test$jb.mul$Skewness$parameter), error = function(e) NA)
sk_pval <- tryCatch(as.numeric(normality_test$jb.mul$Skewness$p.value[1, 1]), error = function(e) NA)

kt_stat <- tryCatch(as.numeric(normality_test$jb.mul$Kurtosis$statistic[1, 1]), error = function(e) NA)
kt_df <- tryCatch(as.numeric(normality_test$jb.mul$Kurtosis$parameter), error = function(e) NA)
kt_pval <- tryCatch(as.numeric(normality_test$jb.mul$Kurtosis$p.value[1, 1]), error = function(e) NA)

arch_stat <- as.numeric(arch_test$arch.mul$statistic)
arch_df <- as.numeric(arch_test$arch.mul$parameter)
arch_pval <- as.numeric(arch_test$arch.mul$p.value)

diag_results_df <- data.frame(
  Test = c("Portmanteau Test", "JB-Test", "Skewness", "Kurtosis", "ARCH Test"),
  Chi_Squared = c(pt_stat, jb_stat, sk_stat, kt_stat, arch_stat),
  df = c(pt_df, jb_df, sk_df, kt_df, arch_df),
  p_value = c(pt_pval, jb_pval, sk_pval, kt_pval, arch_pval),
  stringsAsFactors = FALSE
)

diag_results_df$Chi_Squared <- round(diag_results_df$Chi_Squared, 2)
diag_results_df$p_value <- round(diag_results_df$p_value, 3)

print(diag_results_df)

rlm_model <- vecm_model$rlm
coef_matrix <- coef(rlm_model)
X <- model.matrix(rlm_model$terms, rlm_model$model)
resid_matrix <- residuals(rlm_model)

n <- nrow(X)
p <- ncol(X)
df_resid <- n - p

XtX_inv <- solve(crossprod(X) + diag(ncol(X)) * 1e-10)
robust_se_matrix <- matrix(0, nrow = nrow(coef_matrix), ncol = ncol(coef_matrix),
                           dimnames = dimnames(coef_matrix))

for (i in seq_len(ncol(coef_matrix))) {
  resid_i <- resid_matrix[, i]
  h <- diag(X %*% XtX_inv %*% t(X))
  u_star <- resid_i / (1 - pmin(h, 0.95))
  omega_star_diag <- u_star^2
  first_term <- t(X) %*% diag(omega_star_diag) %*% X
  second_term <- (1 / n) * (t(X) %*% (u_star %*% t(u_star)) %*% X)
  middle_term <- first_term - second_term
  robust_vcov <- ((n - 1) / n) * XtX_inv %*% middle_term %*% XtX_inv
  robust_se_matrix[, i] <- sqrt(diag(robust_vcov))
}

print(round(coef_matrix, 4))
print(round(robust_se_matrix, 4))

robust_p_values <- 2 * pt(abs(coef_matrix / robust_se_matrix), df = df_resid, lower.tail = FALSE)
print("Robust P-values:")
print(round(robust_p_values, 4))

add_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

stars_matrix <- apply(robust_p_values, c(1, 2), add_stars)
combined_output <- matrix(paste0(format(round(coef_matrix, 4), nsmall = 4), stars_matrix),
                          nrow = nrow(coef_matrix), dimnames = dimnames(coef_matrix))
print(combined_output, quote = FALSE)

summary_list <- summary(rlm_model)
r_squared <- sapply(summary_list, function(s) s$r.squared)
adj_r_squared <- sapply(summary_list, function(s) s$adj.r.squared)
rmse <- sapply(seq_len(ncol(resid_matrix)), function(i) sqrt(mean(resid_matrix[, i]^2)))
n_obs <- nrow(X)

print(paste("Observations:", n_obs))
print(paste("R-squared (LC, ID):", round(r_squared[1], 4), ",", round(r_squared[2], 4)))
print(paste("Adj. R-squared (LC, ID):", round(adj_r_squared[1], 4), ",", round(adj_r_squared[2], 4)))
print(paste("RMSE (LC, ID):", round(rmse[1], 4), ",", round(rmse[2], 4)))

irf_LC_ID <- vars::irf(vecm_as_var, impulse = "LC_xts", response = "ID_xts",
                       boot = TRUE, runs = 1000, n.ahead = 24, ci = 0.95)

ggsave(file.path("output", "figure_04_irf_id_to_lc.png"), plot(irf_LC_ID))

irf_ID_LC <- vars::irf(vecm_as_var, impulse = "ID_xts", response = "LC_xts",
                       boot = TRUE, runs = 1000, n.ahead = 24, ci = 0.95)

ggsave(file.path("output", "figure_05_irf_lc_to_id.png"), plot(irf_ID_LC))

adf_level_none <- ur.df(LC_xts, type = "none", lags = 1)
adf_level_drift <- ur.df(LC_xts, type = "drift", lags = 1)
adf_level_trend <- ur.df(LC_xts, type = "trend", lags = 1)

residuals_none <- residuals(adf_level_none@testreg)
residuals_drift <- residuals(adf_level_drift@testreg)
residuals_trend <- residuals(adf_level_trend@testreg)

print("Ljung-Box Test for ADF (None) Residuals:")
print(Box.test(residuals_none, type = "Ljung-Box", lag = 12))
print("Ljung-Box Test for ADF (Drift) Residuals:")
print(Box.test(residuals_drift, type = "Ljung-Box", lag = 12))
print("Ljung-Box Test for ADF (Trend) Residuals:")
print(Box.test(residuals_trend, type = "Ljung-Box", lag = 12))

ID_diff <- diff(ID_xts) %>% na.omit()

adf_level_none <- ur.df(ID_xts, type = "none", lags = 1)
adf_level_drift <- ur.df(ID_xts, type = "drift", lags = 1)
adf_level_trend <- ur.df(ID_xts, type = "trend", lags = 1)

residuals_none <- residuals(adf_level_none@testreg)
residuals_drift <- residuals(adf_level_drift@testreg)
residuals_trend <- residuals(adf_level_trend@testreg)

print("Ljung-Box Test for ADF (None) Residuals:")
print(Box.test(residuals_none, type = "Ljung-Box", lag = 12))
print("Ljung-Box Test for ADF (Drift) Residuals:")
print(Box.test(residuals_drift, type = "Ljung-Box", lag = 12))
print("Ljung-Box Test for ADF (Trend) Residuals:")
print(Box.test(residuals_trend, type = "Ljung-Box", lag = 12))

adf_level_none <- ur.df(diff(LC_xts) %>% na.omit(), type = "none", lags = 1)
adf_level_drift <- ur.df(diff(LC_xts) %>% na.omit(), type = "drift", lags = 1)
adf_level_trend <- ur.df(diff(LC_xts) %>% na.omit(), type = "trend", lags = 1)

residuals_none <- residuals(adf_level_none@testreg)
residuals_drift <- residuals(adf_level_drift@testreg)
residuals_trend <- residuals(adf_level_trend@testreg)

print("Ljung-Box Test for ADF (None) Residuals:")
print(Box.test(residuals_none, type = "Ljung-Box", lag = 12))
print("Ljung-Box Test for ADF (Drift) Residuals:")
print(Box.test(residuals_drift, type = "Ljung-Box", lag = 12))
print("Ljung-Box Test for ADF (Trend) Residuals:")
print(Box.test(residuals_trend, type = "Ljung-Box", lag = 12))

adf_level_none <- ur.df(ID_diff, type = "none", lags = 1)
adf_level_drift <- ur.df(ID_diff, type = "drift", lags = 1)
adf_level_trend <- ur.df(ID_diff, type = "trend", lags = 1)

residuals_none <- residuals(adf_level_none@testreg)
residuals_drift <- residuals(adf_level_drift@testreg)
residuals_trend <- residuals(adf_level_trend@testreg)

print("Ljung-Box Test for ADF (None) Residuals:")
print(Box.test(residuals_none, type = "Ljung-Box", lag = 12))
print("Ljung-Box Test for ADF (Drift) Residuals:")
print(Box.test(residuals_drift, type = "Ljung-Box", lag = 12))
print("Ljung-Box Test for ADF (Trend) Residuals:")
print(Box.test(residuals_trend, type = "Ljung-Box", lag = 12))

mysum <- summary(vecm_k2$rlm)
screenreg(list(mysum[[1]], mysum[[2]]))

model_summaries <- summary(vecm_model$rlm)
screenreg(
  list(model_summaries[[1]], model_summaries[[2]]),
  custom.model.names = c("\\u0394 Log(S&P 500)", "\\u0394 Log(IPI)"),
  custom.coef.names = c(
    "Error Correction Term (ECT)",
    "Lag 1 \\u0394Log(S&P 500)",
    "Lag 1 \\u0394 Log(IPI)"
  ),
  include.rsquared = TRUE,
  include.adjrs = TRUE,
  include.nobs = TRUE,
  include.rmse = TRUE,
  digits = 4,
  single.row = TRUE
)

var_decomposition <- fevd(vecm_as_var, n.ahead = 48)
horizons <- c(1, 6, 12, 24, 36, 48)

sp500_decomp <- t(sapply(horizons, function(h) var_decomposition$LC_xts[h, ]))
ipi_decomp <- t(sapply(horizons, function(h) var_decomposition$ID_xts[h, ]))

rownames(sp500_decomp) <- paste0("h=", horizons)
rownames(ipi_decomp) <- paste0("h=", horizons)

print("Variance Decomposition of S&P 500:")
print(round(sp500_decomp * 100, 2))
print("Variance Decomposition of Industrial Production:")
print(round(ipi_decomp * 100, 2))

