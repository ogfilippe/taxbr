#' Fit ARIMA Model
#'
#' @export
fit_arima_model <- function(.data, .rec,
                            p, d, q, P, D, Q) {

  arima_spec <- modeltime::arima_reg(
    "regression", 12,
    !!p, !!d, !!q,
    !!P, !!D, !!Q
  ) |>
    parsnip::set_engine(
      "arima",
      include.constant = FALSE
    )

  wf <- workflows::workflow() |>
    workflows::add_recipe(.rec) |>
    workflows::add_model(arima_spec)

  tryCatch(
    wf |> parsnip::fit(data = .data),
    # TODO: show which specifications failed
    error = function(cond) NULL
  )
}

