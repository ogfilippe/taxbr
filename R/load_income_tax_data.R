#' Load Brazilian Income Tax Datasets
#'
#' @export
load_income_tax_data <- function() {

  taxes <- list(
    "IR",      # (3/3)
    "IRPF",    # Pessoa Física
    "IRPJ",    # Pessoa Jurídica
    "IRRF",    # Retido na Fonte (4/4)
    "IRRFRT",  # Retido na Fonte - Rendimentos do Trabalho
    "IRRFRC",  # Retido na Fonte - Rendimentos do Capital
    "IRRFRE",  # Retido na Fonte - Remessas ao Exterior
    "IRRFOUT"  # Retido na Fonte - Outros Rendimentos
  ) |>
    purrr::set_names()

  get_tax_series <- function(tax_id, tax_name) {
    tax_id <- sprintf("SRF12_%s12", tax_id)
    wrap_ipea(tax_id, colname = tax_name)
  }

  taxes |>
    purrr::imap(get_tax_series) |>
    purrr::reduce(~ dplyr::full_join(.x, .y, by = "dt"))
}

