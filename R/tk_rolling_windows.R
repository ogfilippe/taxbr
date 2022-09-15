#' Time Series Rolling Windows
#'
#' @export
tk_rolling_windows <- function(.data, horizon, init_size, step_size) {

  splits <- .data |>
    timetk::time_series_split(
      dt, assess = horizon, cumulative = TRUE
    )

  train <- rsample::training(splits)

  folds <- train |>
    timetk::time_series_cv(
      dt, initial = init_size, assess = horizon,
      skip = step_size, cumulative = TRUE
    )

  test <- rsample::testing(splits)

  list(splits = splits, train = train, folds = folds, test = test)
}

