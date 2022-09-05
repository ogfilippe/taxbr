#' Time Series Rolling Windows
#'
#' @export
tk_rolling_windows <- function(.data, h, init_size, step_size, date_col = dt) {

  date_col <- rlang::enquo(date_col)

  splits <- .data |>
    timetk::time_series_split(!!date_col, assess = h, cumulative = TRUE)

  train <- rsample::training(splits)

  folds <- train |>
    timetk::time_series_cv(
      !!date_col, initial = init_size, assess = h,
      skip = step_size, cumulative = TRUE
    )

  test <- rsample::testing(splits)

  list(splits = splits, train = train, folds = folds, test = test)
}

