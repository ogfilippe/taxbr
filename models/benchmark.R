# ---|---|---|---|---|---|---|---|---|---|---
# ----- Método Convencional (Benchmark) -----
# ---|---|---|---|---|---|---|---|---|---|---

library(tidyverse)
library(taxbr)


## Previsão 18 Meses à Frente ----
get_18m_ahead <- function(budget, eta = 0.82) {

  ###
  present <- make_date(budget - 1, 6, 1)

  gdp_hat_curr <- gdp_expectations %>%
    filter(dt == present, ref == (budget - 1)) %>%
    pull(med)

  gdp_hat_next <- gdp_expectations %>%
    filter(dt == present, ref == (budget)) %>%
    pull(med)

  ###
  taxs2_prev <- income_tax %>%
    filter(year == (budget - 2), month %in% 7:12)

  taxs1_curr <- income_tax %>%
    filter(year == (budget - 1), month %in% 1:6)

  ###
  taxs2hat_curr <- taxs2_prev %>%
    mutate(g_fct = 1 + gdp_hat_curr / 100 * eta) %>%
    summarize(dt  = dt %m+% months(12),
              tax = tax * g_fct)

  ###
  taxhat_next <- taxs1_curr %>%
    select(dt, tax) %>%
    bind_rows(taxs2hat_curr) %>%
    mutate(g_fct = 1 + gdp_hat_next / 100 * eta) %>%
    summarize(dt  = dt %m+% months(12),
              tax = tax * g_fct)

  ###
  taxs2hat_curr %>% bind_rows(taxhat_next)
}


## Previsão 12 Meses à Frente ----
get_12m_ahead <- function(budget, eta = 0.82) {

  ###
  present <- make_date(budget - 1, 12, 1)

  gdp_hat_next <- gdp_expectations %>%
    filter(dt == present, ref == (budget)) %>%
    pull(med)

  ###
  tax_curr <- income_tax %>%
    filter(year == (budget - 1), month %in% 1:12)

  ###
  tax_curr %>%
    mutate(g_fct = 1 + gdp_hat_next / 100 * eta) %>%
    summarize(dt  = dt %m+% months(12),
              tax = tax * g_fct)
}


## Previsão 6 Meses à Frente ----
get_06m_ahead <- function(budget, eta = 0.82) {

  ###
  present <- make_date(budget, 6, 1)

  gdp_hat_curr <- gdp_expectations %>%
    filter(dt == present, ref == (budget)) %>%
    pull(med)

  ###
  taxs2_prev <- income_tax %>%
    filter(year == (budget - 1), month %in% 7:12)

  ###
  taxs2_prev %>%
    mutate(g_fct = 1 + gdp_hat_curr / 100 * eta) %>%
    summarize(dt  = dt %m+% months(12),
              tax = tax * g_fct)
}


## Resultados ----
results_benchmark <- 2018:2021 %>%
  set_names(~ paste0("benchmark", .x)) %>%
  map(~ list(h18 = get_18m_ahead(.x),
             h12 = get_12m_ahead(.x),
             h06 = get_06m_ahead(.x)))

attr(results_benchmark, "session") <- devtools::session_info()

results_benchmark %>%
  write_rds(fs::path(here::here(), "results/benchmark.rds"))

