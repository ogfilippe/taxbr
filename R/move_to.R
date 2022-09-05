#' Systematic Saving of R Objects
#'
#' @export
move_to <- function(.object, .path, here = TRUE, session = TRUE) {

  if (here) .path <- c(here::here(), .path)
  if (session) attr(.object, "session") <- devtools::session_info()

  .object |>
    readr::write_rds(do.call(fs::path, as.list(.path)))
}

