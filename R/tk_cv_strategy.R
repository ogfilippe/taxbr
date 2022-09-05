#' Time Series Cross-Validation Strategy
#'
#' @export
tk_cv_strategy <- function(.folds) {

  .folds |>
    timetk::tk_time_series_cv_plan() |>
    dplyr::group_by(.id, .key) |>
    dplyr::summarize(from = min(dt), to = max(dt), .groups = "drop")
}

