
library(tidyverse)
library(taxbr)


# ---- 1. Importação ----

## arrecadação por incidência
incidences_vct <- c(
  Y    = "",     IPF  = "PF",   IPJ  = "PJ",   IRF  = "RF",
  IRFT = "RFRT", IRFK = "RFRC", IRFX = "RFRE", IRFU = "RFOUT"
)

raw_incidences <- incidences_vct %>%
  imap(~ wrap_ipea(sprintf("SRF12_IR%s12", .x), colname = .y)) %>%
  reduce(~ full_join(.x, .y, by = "dt"))


## arrecadação por estado
regions_vct <- c(
  "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO",
  "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR",
  "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"
) %>%
  set_names()

raw_regions <- regions_vct %>%
  imap(~ wrap_ipea(sprintf("CONFAZ12_IR%s12", .x), colname = .y)) %>%
  reduce(~ full_join(.x, .y, by = "dt")) %>%
  mutate(across(-dt, ~ .x / 10^6))


## séries brutas
raw_data <- list(
  incidences = raw_incidences,
  regions    = raw_regions
)


# ---- 2. Transformação ----

## junção e intervalo temporal
trans_nominal <- full_join(
  raw_data$incidences, raw_data$regions,
  by = "dt"
) %>%
  filter(between(year(dt), 2000, 2021))


## correção monetária
trans_real <- trans_nominal %>%
  mutate(across(-dt, ~ deflate_vec(dt, .x, ref = max(dt), index = "IPCA")))


## curadoria
impute_missings <- \(.col, ...) {
  .col <- if_else(unlist(...), NA_real_, .col)
  .col %>%
    ts(frequency = 12) %>%
    forecast::na.interp(lambda = 0) %>%
    as.numeric()
}

trans_curacy <- trans_real %>%
  mutate(
    yr = year(dt), mth = month(dt),
    GO = GO %>% impute_missings(yr == 2009, mth %in% c(7, 8)),
    PE = PE %>% impute_missings(yr == 2016),
    RO = RO %>% impute_missings(yr == 2007, mth %in% c(3, 5)),
    TO = TO %>% impute_missings(yr == 2007)
  ) %>%
  select(-yr, -mth)


## séries transformadas
trans_data <- list(
  nominal = trans_nominal,
  real    = trans_real,
  curacy  = trans_curacy
)


# ---- 3. Preparação ----

clean_anomalies <- \(.col, horizon) {
  f <- \(y) {
    y %>%
      ts(frequency = 12) %>%
      forecast::tsclean(lambda = 0) %>%
      as.numeric()
  }
  if_else(row_number() < (n() - horizon), f(.col), .col)
}


## séries limpas
parsed_data <- map(
  c(6, 12, 18) %>% set_names(sprintf("h%02d", .)),
  \(h) {
    trans_data$curacy %>%
      mutate(across(-dt, ~ clean_anomalies(.x, horizon = h)))
  }
)


# ---- 4. Exportação ----

income_tax_data <- list(
  raw = raw_data, trans = trans_data, parsed = parsed_data
)

usethis::use_data(income_tax_data, overwrite = TRUE)

