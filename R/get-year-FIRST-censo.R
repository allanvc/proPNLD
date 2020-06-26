#' @title Get the FIRST year of the time series
#'
#' @description Identifica e extrai o primeiro ano da série histórica do censo
#'
#' @param tb_censo_full \code{tibble} contendo a base completa do censo
#'     após tratamento e limpeza por meio da função \code{\link{extract_and_clean_censo_full}}.
#'
#' @return \code{vector} do tipo \code{numeric} contendo o ano inicial da série.
#'
#' @family get_ano
#'
#' @examples
#' \dontrun{
#'
#' ano_FIRST_censo <- get_year_FIRST_censo(tb_censo_full)
#'
#' }
#' @export
#'
get_year_FIRST_censo <- function(tb_censo_full) {

  # obtem ano do ultimo censo
  ano_FIRST_censo <- tb_censo_full %>% # import magrittr pro namespace
    dplyr::summarise(min(lubridate::year(NU_ANO_CENSO))) %>%
    dplyr::collect() %>%
    unlist()

  return(unname(ano_FIRST_censo))

}
