#' Get ARIMA Workflow
#'
#' @noRd
get_arima_workflow <- function(.data, model) {

  rec <- recipes::recipe(tax ~ dt, data = .data) |>
    recipes::step_log(all_outcomes()) |>
    recipes::step_normalize(all_outcomes())

  workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(model)
}

