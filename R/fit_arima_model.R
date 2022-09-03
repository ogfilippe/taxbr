#' Fit ARIMA Model
#'
#' @export
fit_arima_model <- function(.data, p, d, q, P, D, Q, constant) {

  arima_spec <- modeltime::arima_reg(
    "regression", 12,
    !!p, !!d, !!q,
    !!P, !!D, !!Q
  ) |>
    parsnip::set_engine(
      "arima",
      include.constant = !!constant
    )

  tryCatch(
    arima_spec |> parsnip::fit(tax ~ ., data = .data),
    error = function(cond) NULL
  )
}

