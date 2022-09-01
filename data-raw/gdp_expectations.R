# Expectativa de crescimento do PIB
# (Banco Central)

library(dplyr, warn.conflicts = FALSE)
library(rbcb)


gdp_expectations <- get_market_expectations(
  type = "annual", indic = "PIB Total"
) |>
  janitor::clean_names() |>
  transmute(
    dt    = data,
    year  = lubridate::year(dt),
    month = lubridate::month(dt),
    ref   = as.numeric(data_referencia),
    avg   = media,
    med   = mediana,
    std   = desvio_padrao,
    min   = minimo,
    max   = maximo,
    n     = numero_respondentes
  ) |>
  filter(
    year %in% 2017:2021, month %in% c(6, 12), ref %in% 2017:2021,
    (ref - year + month) %in% c(0+6, 1+6, 1+12)
  ) |>
  group_by(year, month, ref) |>
  slice(n()) |>
  ungroup() |>
  mutate(dt = lubridate::rollback(dt, roll_to_first = TRUE))


usethis::use_data(gdp_expectations, overwrite = TRUE)

