#' @title Exponential Smoothing Forecast
#'
#' @description Executa o forecast automático utilizando modelo de suavização
#'     exponencial.
#'
#' @param grupos \code{list} contendo grupos de munícipios retornada pela função
#'     \code{\link{split_mun_groups}}.
#' @param ano_FIRST_censo \code{vector} do tipo \code{numeric} contendo o ano
#'     inicial da série, retornado pela função \code{\link{get_year_FIRST_censo}}.
#' @param ano_LAST_censo \code{vector} do tipo \code{numeric} contendo o ano
#'     final da série, retornado pela função \code{\link{get_year_LAST_censo}}.
#'
#' @return \code{tibble} contendo o output dos diversos modelos aplicados nas
#'    etapas de ensino/escolas de um grupo específico de municípios.
#'
#' @family forecast
#'
#' @keywords internal
#'
ets_forecast_censo <- function(grupos, ano_FIRST_censo, ano_LAST_censo){

  out <- lapply(1:length(grupos), function(i){
    # i = 1
    grupos[[i]] %>%

      # parte1
      dplyr::group_by(CO_MUNICIPIO, CO_ENTIDADE, CO_ETAPA_ENSINO) %>% # talvez mais demorado
      dplyr::select(NU_ANO_CENSO, QTD_ALUNOS) %>%

      # parte2
      dplyr::group_by(CO_MUNICIPIO, CO_ENTIDADE, CO_ETAPA_ENSINO) %>% # talvez mais demorado
      tidyr::nest(.key = "data.tbl") %>% # nest nao dah para fazer direto no Oracle

      #parte3
      dplyr::mutate(data.ts = purrr::map(.x = data.tbl,
                                         .f = tk_ts_transform_and_impute, # internal function
                                         ano_FIRST_censo = ano_FIRST_censo,
                                         ano_LAST_censo = ano_LAST_censo)) %>%

      #parte fit
      dplyr::mutate(fit.ets = purrr::map(data.ts, forecast::ets)) %>%

      #parte forecast
      dplyr::mutate(fcast.ets = purrr::map(fit.ets, forecast::forecast, h = 2)) %>%
      #calcular erros
      dplyr::mutate(error.ets = purrr::map(fcast.ets, forecast::accuracy)) %>%
      # dplyr::mutate(error.ets = purrr::map(error.ets, tibble::as_tibble)) %>%
      dplyr::mutate(error.ets = purrr::map(error.ets, as.data.frame)) %>%
      tidyr::unnest(error.ets) %>%
      #parte tidy/apres
      # mutate(sweep = map(fcast.ets, sw_sweep, fitted = FALSE, timetk_idx = FALSE)) %>%
      dplyr::mutate(sweep = purrr::map(fcast.ets, sweep::sw_sweep, fitted = TRUE, timetk_idx = FALSE)) %>%
      tidyr::unnest(sweep) %>%
      dplyr::filter(key == "forecast")
    # deixar para ordenar a hora que tivermos tudo
  })
  return(out)
}
