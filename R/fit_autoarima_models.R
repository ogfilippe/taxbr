#' Fit Best ARIMA Models by AICc
#'
#' @export
fit_autoarima_models <- function(.data, specs, top_n = 5) {

  params <- specs |>
    head(top_n) |>
    dplyr::mutate(orders = gsub("[^0-9]+", "", model)) |>
    dplyr::transmute(
      p = 1, d = 2, q = 3,
      P = 4, D = 5, Q = 6,
      dplyr::across(p:Q, ~ orders |> substr(.x, .x) |> as.numeric()),
      constant = grepl("drift|mean", model)
    ) |>
    purrr::transpose()

  params |>
    purrr::map(
      ~ do.call(fit_arima_model, c(list(.data = .data), .x))
    ) |>
    modeltime::as_modeltime_table()
}

