# ---|---|---|---|---|---|---|---|---|---|---
# ----- Método Convencional (Benchmark) -----
# ---|---|---|---|---|---|---|---|---|---|---

library(tidyverse)
library(taxbr)

## dados
series <- income_tax_data$parsed

## modelo
fit_scenario_model <- function(.data, target, last_info, eta = 0.82) {

  clean <- \(v) v %>% as.list() %>% set_names(c("year", "half"))
  from <- clean(last_info); to <- clean(target)

  g <- gdp_expectations %>%
    filter(year == from$year, month == from$half * 6, ref == to$year) %>%
    summarize(1 + med / 100 * eta) %>%
    pull()

  .data %>%
    filter(year(dt) == to$year - 1, semester(dt) == to$half) %>%
    summarize(dt = dt %m+% months(12), across(-dt, ~ .x * g))
}


# Previsão 06 Meses à Frente ----
res06 <- fit_scenario_model(series$h06, c(2021, 2), last_info = c(2021, 1))


# Previsão 12 Meses à Frente ----
res12 <- map_dfr(
  1:2,
  ~ fit_scenario_model(series$h12, c(2021, .x), last_info = c(2020, 2))
)


# Previsão 18 Meses à Frente ----
res18 <- map2_dfr(
  2020:2021, 2:1,
  ~ fit_scenario_model(series$h18, c(.x, .y), last_info = c(2020, 1))
) %>%
  bind_rows(
    fit_scenario_model(., c(2021, 2), last_info = c(2020, 1))
  )


# Resultados ----
res <- list(h06 = res06, h12 = res12, h18 = res18)
res %>% move_to("results/benchmark.rds", here = TRUE, session = TRUE)


