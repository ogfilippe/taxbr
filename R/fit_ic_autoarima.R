#' Fit Best ARIMA Models by AICc
#'
#' @export
fit_ic_autoarima <- function(.data, specs, top_n = 5) {

  specs |>
    head(top_n) |>
    purrr::map(~ do.call(fit_arima_model, c(list(.data = .data), .x))) |>
    modeltime::as_modeltime_table()
}

