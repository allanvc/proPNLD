#' @title Make Final Table
#'
#' @description Produz a tabela final contendo a projeção para gravação no SGBD
#'
#' @param forecast_output \code{list} de \code{tibble}s contendo o forecast output
#'     de todos os modelos, a partir da execução de \code{\link{parallel_forecast}}.
#' @param tb_censo Objeto do tipo \code{tibble} do \code{dbplyr} que ainda vive
#'     no SGBD, obtido por meio da função \code{\link{query_tb_censo}}
#' @param ano_FIRST_censo \code{vector} do tipo \code{numeric} contendo o ano
#'     inicial da série, retornado pela função \code{\link{get_year_FIRST_censo}}.
#' @param ano_LAST_censo \code{vector} do tipo \code{numeric} contendo o ano
#'     final da série, retornado pela função \code{\link{get_year_LAST_censo}}.
#'
#' @return Um \code{tibble} contendo os dados tratados.
#'
#' @family final_wrangling
#'
#' @examples
#' \dontrun{
#'
#' conn <- connect_censo_db(host, port, user, pass, svc)
#' tb_censo <- query_tb_censo(conn, schema, tab) # recuperando tab original para join
#' tb_censo_final_join <- make_final_table(forecast_output, tb_censo, ano_FIRST_censo, ano_LAST_censo)
#'
#' }
#' @export
#'
make_final_table <- function(forecast_output, tb_censo, ano_FIRST_censo, ano_LAST_censo) {


  # pouco >1min com 8 clusters em paralelo e 2 dfs por grupo
  resultado2 <- unlist(forecast_output, recursive=FALSE)

  rm(forecast_output)

  Xresultado_final_bind <- do.call(dplyr::bind_rows, resultado2)
  # show!


  # essa vai ser a base para o join final
  tb_LAST_censo <- tb_censo %>%
    dplyr::filter(NU_ANO_CENSO == ano_LAST_censo) %>%
    dplyr::mutate(index = ano_LAST_censo, key = "censo") %>%
    dplyr::select(-NU_ANO_CENSO) %>%
    dplyr::collect()

  rm(resultado2)

  # primeiro trazemos as colunas que interessam a COCAO
  Xtb_censo_final_join <- tb_LAST_censo %>% # direto do banco

    dplyr::select(CO_UF, SG_UF, CO_MUNICIPIO, NO_MUNICIPIO,
                  CO_ENTIDADE, NO_ENTIDADE,
                  CO_TP_LOCALIZACAO, DS_TP_LOCALIZACAO,
                  CO_ETAPA_ENSINO, NO_ETAPA_ENSINO
    ) %>%
    dplyr::left_join(Xresultado_final_bind, by=c(
      "CO_MUNICIPIO",
      "CO_ENTIDADE",
      "CO_ETAPA_ENSINO"))


  # agora trazemos fazemos o bind

  Xtb_censo_final_join2 <- dplyr::bind_rows(Xtb_censo_final_join, tb_LAST_censo) %>%
    dplyr::mutate(CENSO_REF = ano_LAST_censo,
                  DT_HR_EXEC = Sys.time(),
                  QTD_ALUNOS = ifelse(is.na(QTD_ALUNOS), value, QTD_ALUNOS),
                  # regra da COCAO - para facilitar a visualização -- sempre valores positivos
                  QTD_ALUNOS_AJUSTADA = ifelse(QTD_ALUNOS > 0, QTD_ALUNOS,
                                      ifelse(hi.80 > 0, hi.80,
                                             ifelse(hi.95 > 0, hi.95, 0)))
                  )


  # calculando tamanhos das series
  tb_TAM_SERIE_HISTORICA <- tb_censo %>%
    dplyr::group_by(CO_MUNICIPIO, CO_ENTIDADE, CO_ETAPA_ENSINO) %>%
    dplyr::summarize(TAM_SERIE_HISTORICA = dplyr::n()) %>%
    dplyr::mutate(FLAG_SERIE_COMPLETA = ifelse(
      TAM_SERIE_HISTORICA < (ano_LAST_censo - ano_FIRST_censo)+1,
      0, 1)) %>%
    dplyr::collect()

  rm(Xtb_censo_final_join)

  # join com o tamanho da serie
  Xtb_censo_final_join3 <- Xtb_censo_final_join2 %>%
    dplyr::left_join(tb_TAM_SERIE_HISTORICA)

  rm(Xtb_censo_final_join2)

  # ajustando a apresentacao final
  Xtb_censo_final_join4 <- Xtb_censo_final_join3 %>%

    dplyr::mutate(DS_VERSAO_PACOTE = as.character(utils::packageVersion("proPNLD"))) %>% #v0.1.5 - incluindo versao do pacote

    dplyr::select(CENSO_REF, DT_HR_EXEC, DS_VERSAO_PACOTE,
                  CO_UF, SG_UF, CO_MUNICIPIO, NO_MUNICIPIO,
                  CO_ENTIDADE, NO_ENTIDADE,
                  CO_TP_LOCALIZACAO, DS_TP_LOCALIZACAO,
                  CO_ETAPA_ENSINO, NO_ETAPA_ENSINO,
                  TAM_SERIE_HISTORICA, FLAG_SERIE_COMPLETA,
                  index, key,
                  QTD_ALUNOS, lo.95, lo.80, hi.80, hi.95,
                  MAPE, MASE, QTD_ALUNOS_AJUSTADA) %>%

    dplyr::rename(NU_ANO_CENSO_REF = CENSO_REF, #v0.1.3
                  DT_HORA_EXEC = DT_HR_EXEC,
                  NU_TAMANHO_SERIE_HIST = TAM_SERIE_HISTORICA,
                  FL_SERIE_COMPLETA = FLAG_SERIE_COMPLETA,
                  NU_ANO_INDEX = index,
                  DS_KEY = key,
                  QT_ALUNOS = QTD_ALUNOS,
                  NU_LO_95 = lo.95, NU_LO_80 = lo.80,
                  NU_HI_80 = hi.80, NU_HI_95 = hi.95,
                  NU_MAPE = MAPE, NU_MASE = MASE,
                  QT_ALUNOS_AJUSTADA = QTD_ALUNOS_AJUSTADA) %>% #Oracle SGBDs dont like "." in column names

    #v0.1.3
    dplyr::mutate(NU_MASE = dplyr::case_when(is.nan(NU_MASE) ~ NA_real_,
                                             TRUE ~ NU_MASE)) %>%

    dplyr::group_by(CO_MUNICIPIO, CO_ENTIDADE, CO_TP_LOCALIZACAO, CO_ETAPA_ENSINO,
                    NU_ANO_INDEX) %>%

    dplyr::arrange(.by_group=TRUE) %>%

    dplyr::ungroup() # v0.1.3


  return(Xtb_censo_final_join4)

}
