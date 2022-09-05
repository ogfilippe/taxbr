# ---|---|---|---|---|---|---|---|---|---|---
# --------- Metodologia Box-Jenkins ---------
# ---|---|---|---|---|---|---|---|---|---|---

library(tidyverse)
library(tidymodels)
library(timetk)
library(modeltime)
library(modeltime.resample)
library(taxbr)


# TODO: Construção de gráficos
# - Análise de autocorrelação
# - Análise de estacionariedade
# - Análise de resíduos
# - Análise de influência do Covid-19

# TODO: Construção de resultados
# - Seleção por critério de informação
# - Ensemble com peso de critério de informação
# - Seleção por validação cruzada
# - Ensemble com peso de validação cruzada


get_months_ahead <- function(h) {

  lst <- income_tax |>
    tk_rolling_windows(h, init_size = 108, step_size = 6)

  lst$rec <- recipe(tax ~ dt, data = lst$train) |>
    step_log(all_outcomes()) |>
    step_normalize(all_outcomes())

  lst$specs <- show_ic_autoarima(lst$train, lst$rec)
  lst$models <- lst$specs |> fit_ic_autoarima(lst$train, lst$rec, top_n = 10)
  lst$cv <- lst$models |> tk_resamples(lst$folds)

  lst
}


res_arima <- list(h18 = 18, h12 = 12, h06 = 6) |>
  map(get_months_ahead)


res_arima |> move_to("results/arima.rds")

