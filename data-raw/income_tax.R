
library(dplyr, warn.conflicts = FALSE)
library(taxbr)


tax_id <- "SRF12_IR12"


income_tax <- wrap_ipea(tax_id, colname = "tax") |>
  mutate(
    year   = lubridate::year(dt),
    month  = lubridate::month(dt),
    .after = "dt"
  ) |>
  filter(year %in% 2008:2021) |>
  mutate(
    tax = deflate(dt, tax, ref = max(dt), index = "IPCA"),
    tax = if_else(dt == "2016-10-01",
                  timetk::ts_clean_vec(tax, 12, lambda = 0),
                  tax)
  )


usethis::use_data(income_tax, overwrite = TRUE)

