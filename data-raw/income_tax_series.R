
library(tidyverse)
library(timetk)
library(taxbr)


# ---- 1. Importação ----

# ---- 1.1 Arrecadação por Incidência ----
incidences <- c(
  Y    = "",     IPF  = "PF",   IPJ  = "PJ",   IRF  = "RF",
  IRFT = "RFRT", IRFK = "RFRC", IRFX = "RFRE", IRFU = "RFOUT"
)

raw_incidences_tbl <- incidences |>
  imap(~ wrap_ipea(sprintf("SRF12_IR%s12", .x), colname = .y)) |>
  reduce(~ full_join(.x, .y, by = "dt"))

# ---- 1.2 Arrecadação por Estado ----
regions <- c(
  "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO",
  "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR",
  "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"
) |>
  set_names()

raw_regions_tbl <- regions |>
  imap(~ wrap_ipea(sprintf("CONFAZ12_IR%s12", .x), colname = .y)) |>
  reduce(~ full_join(.x, .y, by = "dt")) |>
  mutate(across(-dt, ~ .x / 10^6))


# ---- 2. Junção e Intervalo Temporal ----
tax_nominal_tbl <- raw_incidences_tbl |>
  full_join(raw_regions_tbl, by = "dt") |>
  filter(between(year(dt), 2000, 2021))


# ---- 3. Correção Monetária ----
tax_real_tbl <- tax_nominal_tbl |>
  mutate(across(-dt, ~ deflate_vec(dt, .x, ref = max(dt), index = "IPCA")))


# ---- 4. Curadoria ----
tax_curacy_tbl <- tax_real_tbl |>
  mutate(
    # Pernambuco
    PE = if_else((year(dt) == 2016), NA_real_, PE),
    PE = ts_impute_vec(PE, period = 12, lambda = 0),
    # Tocantins
    TO = if_else((year(dt) == 2007), NA_real_, TO),
    TO = ts_impute_vec(TO, period = 12, lambda = 0),
    # Goiás
    GO = if_else((year(dt) == 2009) & (month(dt) %in% c(7, 8)), NA_real_, GO),
    GO = ts_impute_vec(GO, period = 12, lambda = 0),
    # Rondônia
    RO = if_else((year(dt) == 2007) & (month(dt) %in% c(3, 5)), NA_real_, RO),
    RO = ts_impute_vec(RO, period = 12, lambda = 0)
  )


# ---- 5. Exportação ----
income_tax_series <- list(
  raw     = list(incidences = raw_incidences_tbl, regions = raw_regions_tbl),
  nominal = tax_nominal_tbl,
  real    = tax_real_tbl,
  curacy  = tax_curacy_tbl
)

usethis::use_data(income_tax_series, overwrite = TRUE)
