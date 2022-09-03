#' Show ARIMA Specifications by AICc
#'
#' @export
show_ic_autoarima <- function(.data,
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
      lambda        = NULL,
      allowdrift    = TRUE
    )

  txt <- capture.output(
    { res <- autoarima_spec |> parsnip::fit(tax ~ ., data = .data) }
  )

  con <- textConnection(txt)
  df <- con |> read.table(sep = ":") |> head(-1)
  close(con)

  df |>
    tibble::as_tibble() |>
    dplyr::mutate(
      model  = V1,
      aicc   = V2 |> trimws() |> as.numeric(),
      orders = gsub("[^0-9]+", "", model),
      p = 1, d = 2, q = 3,
      P = 4, D = 5, Q = 6
    ) |>
    dplyr::arrange(aicc) |>
    dplyr::transmute(
      dplyr::across(p:Q, ~ orders |> substr(.x, .x) |> as.numeric()),
      constant = grepl("drift", model)
    ) |>
    purrr::transpose()
}

