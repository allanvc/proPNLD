#' @title Time Series Transformation and Imputation
#'
#' @description Ordena a série temporal de cada etapa de ensino/escola e realiza
#'     a imputação de dados para os valores faltantes, utilizando Exponential
#'     Weighted Average quando há pelos menos dois valores e Next Observation
#'     Carried Backwards - NOCB quando hpa apenas o valor do último censo na série.
#'
#' @param data.tbl \code{tibble} agrupado contendo a série histórica de uma
#'     etapa de ensino/escola.
#' @param ano_FIRST_censo \code{vector} do tipo \code{numeric} contendo o ano
#'     inicial da série, retornado pela função \code{\link{get_year_FIRST_censo}}.
#' @param ano_LAST_censo \code{vector} do tipo \code{numeric} contendo o ano
#'     final da série, retornado pela função \code{\link{get_year_LAST_censo}}.
#'
#' @return Objeto \code{timetk::tk_ts} com a série histórica reordenada e com
#'     os valores imputados
#'
#' @family forecast
#'
#' @keywords internal
#'
tk_ts_transform_and_impute <- function(data.tbl, ano_LAST_censo, ano_FIRST_censo){

  data.tbl2 <- data.tbl %>% #df$data.tbl[[1]] %>% #
    dplyr::arrange(NU_ANO_CENSO) %>%
    # tidyr::complete(NU_ANO_CENSO = seq.POSIXt(min(NU_ANO_CENSO), max(NU_ANO_CENSO), by = "year"),
    #                 tidyr::nesting(CO_MUNICIPIO, CO_ENTIDADE, CO_ETAPA_ENSINO))
    tidyr::complete(NU_ANO_CENSO = seq.POSIXt(
      as.POSIXct(as.character(ano_FIRST_censo), format = '%Y'),
      as.POSIXct(as.character(ano_LAST_censo), format = '%Y'), by = "year"))

  # select(QTD_ALUNOS) %>%
  tb_time_data.tbl <- timetk::tk_ts(data.tbl2$QTD_ALUNOS,
                                    # select = -data.tbl2$NU_ANO_CENSO,
                                    start = ano_FIRST_censo,
                                    end = ano_LAST_censo) #%>%

  tb_time_data.tbl2 <- tryCatch({

    imputeTS::na_ma(tb_time_data.tbl, weighting = "exponential") #NA input

    }, error = function(e){

      tb_time_data.tbl2 <- imputeTS::na_locf(tb_time_data.tbl, option = "nocb") #NA input

      return(tb_time_data.tbl2)
    })
    #forecast::na.interp(.) #NA input

  return(tb_time_data.tbl2)

}
