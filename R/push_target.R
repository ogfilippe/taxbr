#' Push Tax & Horizon From Series
#'
#' @export
push_target <- function(tax, h, n_folds = 4, series = jan2011dez2021) {

  tax <- rlang::enquo(tax)

  full_tbl <- series |>
    dplyr::transmute(dt, tax = !!tax)

  splits <- full_tbl |>
    timetk::time_series_split(
      dt, assess = h, cumulative = TRUE
    )

  present_tbl <- rsample::training(splits)
  future_tbl <- rsample::testing(splits)

  end_date <- present_tbl |>
    dplyr::slice_max(lubridate::month(dt)) |>
    tail(1) |>
    dplyr::pull(dt)

  folds <- present_tbl |>
    dplyr::filter(dt <= end_date) |>
    timetk::time_series_cv(
      dt, assess = h, cumulative = TRUE,
      skip = 12, slice_limit = n_folds
    )

  list(
    full    = full_tbl,
    splits  = splits,
    present = present_tbl,
    folds   = folds,
    future  = future_tbl
  )
}

