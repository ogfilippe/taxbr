
library(dplyr, warn.conflicts = FALSE)
library(taxbr)


tax_id <- "SRF12_IR12"


adjustments <- tibble::tribble(
  ~dt,           ~adj,
  # RERCT
  "2016-10-01",  -27429.,
  # IN RFB 1.934
  "2020-04-01",    6085.,
  "2020-05-01",    1603.,
  "2020-06-01",   -7688.,
  # IN RFB 2.077
  "2021-04-01",    8645.,
  "2021-05-01",   -7448.,
  "2021-06-01",   -1197.
) |>
  mutate(dt = as.Date(dt)) |>
  timetk::pad_by_time(
    dt, .by = "month", .pad_value = 0,
    .start_date = "2008-01-01", .end_date = "2021-12-01"
  )


income_tax <- wrap_ipea(tax_id, colname = "tax") |>
  filter(lubridate::year(dt) %in% 2008:2021) |>
  mutate(tax = deflate(dt, tax, ref = max(dt), index = "IPCA")) |>
  inner_join(adjustments, by = "dt") |>
  mutate(tax = tax + adj, .keep = "unused")


usethis::use_data(income_tax, overwrite = TRUE)
