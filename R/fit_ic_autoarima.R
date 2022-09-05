#' Fit Best ARIMA Models by AICc
#'
#' @export
fit_ic_autoarima <- function(.specs, .data, .rec, top_n) {

  top_specs <- .specs |>
    head(top_n) |>
    dplyr::mutate(
      orders = gsub("[^0-9]+", "", model),
      p = 1, d = 2, q = 3, P = 4, D = 5, Q = 6,
      dplyr::across(p:Q, ~ orders |> substr(.x, .x) |> as.numeric())
    ) |>
    dplyr::select(p:Q) |>
    purrr::transpose()

  top_specs |>
    purrr::map(
      ~ do.call(fit_arima_model, c(list(.data = .data, .rec = .rec), .x))
    ) |>
    purrr::compact() |>
    modeltime::as_modeltime_table()
}

