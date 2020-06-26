#' @title Parallel Forecast
#'
#' @description Executa o forecast para etapa de ensino/escola de forma paralela
#'     nos grupos de municípios.
#'
#' @param n_clusters String contendo a "string de conexão" retornada por
#'     \code{\link{connect_censo_db}}.
#' @param grupos Lista contendo os grupos de municípios após \code{\link{split_mun_groups}}.
#' @param ano_FIRST_censo \code{vector} do tipo \code{numeric} contendo o ano
#'     inicial da série, retornado pela função \code{\link{get_year_FIRST_censo}}.
#' @param ano_LAST_censo \code{vector} do tipo \code{numeric} contendo o ano
#'     final da série, retornado pela função \code{\link{get_year_LAST_censo}}.
#' @param model String contendo o tipo de modelo a ser empregado para o fitting e
#'     o forecasting. Por enquanto, aceita apenas a opção \code{"ets"}
#'     (Exponential Smoothing).
#'
#' @return \code{list} de \code{tibble}s contendo o forecast output de todos os modelos.
#'
#' @family parallel forecast
#'
#' @examples
#' \dontrun{
#'
#' forecast_output <- paralell_forecast(n_clusters, grupos, ano_FIRST_censo,
#'                                      ano_LAST_censo, model = "ets")
#'
#' }
#' @export
#'
# parallel_forecast <- function(n_clusters, grupos, ano_FIRST_censo, ano_LAST_censo,
#                               ets_forecast_censo, tk_ts_transform_and_impute) {

parallel_forecast <- function(n_clusters, grupos, ano_FIRST_censo, ano_LAST_censo,
                              model="ets") {

  if (model=="ets") {
    forecast_FUNC = ets_forecast_censo
  } else if (model=="ma") {
    forecast_FUNC = ma_forecast_censo # a implementar
  } else if (model=="arima") {
    forecast_FUNC = arima_forecast_censo # a implementar
  } else if (model=="ets_ma") {
    forecast_FUNC = ets_ma_forecast_censo # a implemetar (modelo otimizado que pega a media em ETS e MA)
  }
  # sempre checar se jah nao temos o pacote!!!

  # library(foreach)
  # library(doParallel)

  # clusters = 8
  cl <- parallel::makeCluster(n_clusters)

  doParallel::registerDoParallel(cl)

  # ## ----
  # system.time({
  #   resultado <- foreach(i = 1:length(grupos),
  #                        .packages = c("dplyr", "dbplyr", "lubridate", "tidyr",
  #                                      "purrr", "purrr", "forecast", "timetk",
  #                                      "sweep"))  %dopar%  {
  #
  #                                        ets_forecast_censo(grupos[[i]])
  #
  #                                      }
  #   # ok! bem rapido!
  # })

  # nao precisamos passar os pacotes, pq estamos declarando como namespace::funcao
  `%dopar%` <- foreach::`%dopar%`

  log <- system.time({
    forecast_output <- foreach::foreach(i = 1:length(grupos),
                                        .packages = c("magrittr"))  %dopar%  {
                                         # remover o magrittr na versao de pacote
                                         # ets_forecast_censo(grupos[[i]], ano_FIRST_censo, ano_LAST_censo,
                                         #                    tk_ts_transform_and_impute)
                                          forecast_FUNC(grupos[[i]],
                                                        ano_FIRST_censo,
                                                        ano_LAST_censo)

                                       }
    # ok! bem rapido!
  })

  print(log)
  return(forecast_output)

}
