# PIB a preços de mercado
# - Índice de volume mensal (1428676)
# - Índice de volume mensal com ajuste sazonal (1428682)
# (FGV Dados)

library(dplyr, warn.conflicts = FALSE)
library(readr)


br_cfg <- locale(
  date_format   = "%m/%Y", decimal_mark = ",",
  grouping_mark = ".",     encoding     = "latin1"
)


# TODO: webscraping no portal do IBRE
gdp_monitor <- fs::path(here::here(), "data-raw/gdp_monitor.csv") |>
  read_delim(delim = ";", locale = br_cfg) |>
  rename_with(~ c("dt", "gdp", "gdp_seasadj")) |>
  filter(lubridate::year(dt) %in% 2008:2021)


usethis::use_data(gdp_monitor, overwrite = TRUE)

