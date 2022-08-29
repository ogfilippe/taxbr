#' Show ARIMA Specifications by AICc
#'
#' @export
show_autoarima_specs <- function(.data, d, D, non_seas = 4, seas = 3,
                                 max_order = 4, stepwise = FALSE) {

  autoarima_spec <- modeltime::arima_reg(
    "regression", 12,
    !!non_seas, 1, !!non_seas,
    !!seas,     1, !!seas
  ) |>
    parsnip::set_engine(
      "auto_arima",
      d = !!d, D = !!D,
      max.order = !!max_order,
      stepwise  = !!stepwise,
      trace = TRUE, approximation = FALSE,
      ic = "aicc", lambda = NULL,
      allowdrift = TRUE, allowmean = TRUE
    )

  wf <- get_arima_workflow(.data, model = autoarima_spec)

  txt <- capture.output(
    { autoarima_res <- wf |> parsnip::fit(data = .data) }
  )

  con <- textConnection(txt)
  df <- read.table(con, sep = ":") |> head(-1)
  close(con)

  df |>
    tibble::as_tibble() |>
    dplyr::transmute(
      model = V1,
      aicc  = V2 |> trimws() |> as.numeric()
    ) |>
    dplyr::arrange(aicc)
}

