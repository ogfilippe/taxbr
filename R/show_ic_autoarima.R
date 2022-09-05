#' Show ARIMA Specifications by AICc
#'
#' @export
show_ic_autoarima <- function(.data, .rec,
                              non_seas  = 4,
                              seas      = 2,
                              max_order = 6,
                              stepwise  = FALSE) {

  autoarima_spec <- modeltime::arima_reg(
    "regression", 12,
    non_seasonal_ar = !!non_seas,
    non_seasonal_ma = !!non_seas,
    seasonal_ar     = !!seas,
    seasonal_ma     = !!seas
  ) |>
    parsnip::set_engine(
      "auto_arima",
      d = 0, D = 1,
      max.order     = !!max_order,
      stepwise      = !!stepwise,
      trace         = TRUE,
      approximation = FALSE,
      ic            = "aicc",
      allowdrift    = FALSE,
      lambda        = NULL
    )

  wf <- workflows::workflow() |>
    workflows::add_recipe(.rec) |>
    workflows::add_model(autoarima_spec)

  txt <- capture.output(
    { res <- wf |> parsnip::fit(data = .data) }
  )

  con <- textConnection(txt)
  df <- con |> read.table(sep = ":", strip.white = TRUE) |> head(-1)
  close(con)

  df |>
    tibble::as_tibble() |>
    dplyr::transmute(model = V1, aicc = as.numeric(V2)) |>
    dplyr::arrange(aicc)
}

