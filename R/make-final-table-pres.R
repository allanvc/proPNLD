#' @title Make Final Table for presenting
#'
#' @description Produz a tabela final contendo a projeção para gravação no SGBD e
#'     apresentação no SIMEC
#'
#' @param tb_censo_final_join \code{tibble} contendo a tabela final com
#'     especificações técnicas completas da projeção do Censo Escolar.
#'
#' @return Um \code{tibble} contendo os dados tratados para apresentação no SIMEC.
#'
#' @family final_wrangling
#'
#' @examples
#' \dontrun{
#'
#' tb_censo_final_join_pres <- make_final_table_pres(tb_censo_final_join)
#'
#' }
#' @export
#'
make_final_table_pres <- function(tb_censo_final_join) {

  tb_censo_list <- tb_censo_final_join %>%
    dplyr::mutate(QTD_ALUNOS_AJUSTADA = round(QTD_ALUNOS_AJUSTADA)) %>%
    dplyr::select(-c("TAM_SERIE_HISTORICA", "FLAG_SERIE_COMPLETA",
                     "key", "QTD_ALUNOS", "lo.95", "lo.80", "hi.80", "hi.95", "MAPE", "MASE")) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(index) %>%
    dplyr::group_split()

  tb_censo_base <- tb_censo_list[[1]]

  tb_censo_h1 <- tb_censo_list[[2]] %>%
    dplyr::select(CO_ENTIDADE, CO_ETAPA_ENSINO, QTD_ALUNOS_AJUSTADA)

  tb_censo_h2 <- tb_censo_list[[3]] %>%
    dplyr::select(CO_ENTIDADE, CO_ETAPA_ENSINO, QTD_ALUNOS_AJUSTADA)

  rm(tb_censo_list)

  tb_censo_final_join_pres <- tb_censo_base %>%
    dplyr::rename(QTD_CENSO = QTD_ALUNOS_AJUSTADA) %>%
    dplyr::left_join(tb_censo_h1, by=c("CO_ENTIDADE", "CO_ETAPA_ENSINO")) %>%
    dplyr::rename(QTD_h1 = QTD_ALUNOS_AJUSTADA) %>%
    dplyr::left_join(tb_censo_h2, by=c("CO_ENTIDADE", "CO_ETAPA_ENSINO")) %>%
    dplyr::rename(QTD_h2 = QTD_ALUNOS_AJUSTADA) %>%
    dplyr::select(-index)

  rm(tb_censo_base, tb_censo_h1, tb_censo_h2)

  return(tb_censo_final_join_pres)

}
