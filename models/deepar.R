
library(tidyverse)
library(timetk)
library(tidymodels)
library(modeltime)
library(modeltime.gluonts)


tax_series <- taxbr::income_tax


# ---- 12 Meses à Frente ----
tax12_clean_tbl <- tax_curacy_tbl |>
  mutate(
    across(
      -dt,
      ~ if_else(dt <= "2020-12-01",
                ts_clean_vec(.x, period = 12, lambda = 0),
                .x)
    )
  )

target12_tbl <- tax12_clean_tbl |>
  select(dt, Y)

tax12_growth_tbl <- tax12_clean_tbl |>
  mutate(
    across(
      -dt,
      ~ log(.x) - lag(log(.x), 12)
    )
  )

tax12_growth_tbl |> summarize(across(.fns = sd, na.rm = TRUE))

tax12_clean_tbl |>
  mutate(
    across(
      -dt,
      ~ diff_vec(.x, lag = 12, difference = 1, log = TRUE, silent = TRUE)
    ),
    Y_backtrans = diff_inv_vec(Y, lag = 12, difference = 1, log = TRUE, initial_values = Y[1:12])
  ) |>
  select(dt, Y, Y_backtrans)

tax12_tbl <- tax12_growth_tbl |>
  drop_na() |>
  pivot_longer(-dt, names_to = "id", values_to = "revenue")


splits12 <- tax12_tbl |>
  time_series_split(dt, assess = 12, cumulative = TRUE)
# Overlapping Timestamps Detected. Processing overlapping time series together using sliding windows.


train12 <- training(splits12)
test12 <- testing(splits12)

rec12 <- recipe(revenue ~ dt + id, data = train12)


spec12 <- deep_ar(
  id = "id", freq = "M", prediction_length = 12,
  #
  epochs = 50
) |>
  set_engine("gluonts_deepar")

wf12 <- workflow() |>
  add_model(spec12) |>
  add_recipe(rec12)

fit12 <- wf12 |>
  fit(train12)

fcst12 <- fit12 |>
  modeltime_table() |>
  modeltime_calibrate(test12) |>
  modeltime_forecast(
    new_data = test12, actual_data = tax12_tbl, keep_data = TRUE
  ) |>
  filter(id == "Y")

pred12 <- fcst12 |>
  filter(.key == "prediction") |>
  select(dt, y_pred = .value)


tax12_clean_tbl |>
  select(dt, Y) |>
  mutate(
    Y_log = log(Y),
    Y_d12 = Y_log - lag(Y_log, 12)
  ) |>
  left_join(pred12, by = "dt") |>
  mutate(
    y_b12 = y_pred + lag(Y_log, 12),
    y_exp = exp(y_b12),
    y_hat = if_else(is.na(y_exp), Y, y_exp)
  ) |>
  filter(Y != y_hat) |>
  smape(truth = Y, estimate = y_hat)


####
tax12_log_tbl <- tax12_clean_tbl |>
  mutate(across(-dt, ~ log(.x)))

autoarima_spec <- arima_reg(
  "regression", 12,
  4, 0, 4,
  3, 1, 3
) |>
  set_engine(
    "auto_arima",
    max.order = 7,
    ic = "aicc",
    stepwise = FALSE,
    trace = TRUE,
    approximation = FALSE,
    allowdrift = TRUE
  )

splits12b <- tax12_log_tbl |>
  time_series_split(dt, assess = 12, cumulative = TRUE)

workflow() |>
  add_model(autoarima_spec) |>
  add_recipe(recipe(Y ~ dt, data = training(splits12b))) |>
  fit(data = training(splits12b))

arima_spec <- arima_reg(
  "regression", 12,
  3, 0, 3,
  0, 1, 1
) |>
  set_engine(
    "arima",
    include.constant = TRUE
  )

pred12b <- workflow() |>
  add_model(arima_spec) |>
  add_recipe(recipe(Y ~ dt, data = training(splits12b))) |>
  fit(data = training(splits12b)) |>
  modeltime_table() |>
  modeltime_calibrate(new_data = testing(splits12b)) |>
  modeltime_forecast(new_data = testing(splits12b), actual_data = tax12_log_tbl, keep_data = TRUE) |>
  filter(.key == "prediction") |>
  select(dt, y_pred = .value)

tax12_clean_tbl |>
  select(dt, Y) |>
  mutate(
    Y_log = log(Y)
  ) |>
  left_join(pred12b, by = "dt") |>
  mutate(
    y_exp = exp(y_pred),
    y_hat = if_else(is.na(y_exp), Y, y_exp)
  ) |>
  filter(Y != y_hat) |>
  smape(truth = Y, estimate = y_hat)
