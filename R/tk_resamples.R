#' Time Series Resamples
#'
#' @export
tk_resamples <- function(.models, .folds) {

  modeltime.resample::modeltime_fit_resamples(
    .models, .folds,
    control = tune::control_resamples(
      verbose = TRUE, save_pred = TRUE, save_workflow = TRUE
    )
  )
}

