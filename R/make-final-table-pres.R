#' @title Make Final Table for Presentation
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
#' tb_censo_final_join_APR <- make_final_table_pres(tb_censo_final_join)
#'
#' }
#' @export
#'
make_final_table_pres <- function(tb_censo_final_join) {

  tb_censo_list <- tb_censo_final_join %>%
    dplyr::mutate(QT_ALUNOS_AJUSTADA = round(QT_ALUNOS_AJUSTADA)) %>%
    dplyr::select(-c("NU_TAMANHO_SERIE_HIST", "FL_SERIE_COMPLETA",
                     "DS_KEY", "QT_ALUNOS", "NU_LO_95", "NU_LO_80", "NU_HI_80",
                     "NU_HI_95", "NU_MAPE", "NU_MASE")) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(NU_ANO_INDEX) %>%
    dplyr::group_split()

  tb_censo_base <- tb_censo_list[[1]]

  tb_censo_h1 <- tb_censo_list[[2]] %>%
    dplyr::select(CO_ENTIDADE, CO_ETAPA_ENSINO, QT_ALUNOS_AJUSTADA)

  tb_censo_h2 <- tb_censo_list[[3]] %>%
    dplyr::select(CO_ENTIDADE, CO_ETAPA_ENSINO, QT_ALUNOS_AJUSTADA)

  rm(tb_censo_list)

  tb_censo_final_join_pres <- tb_censo_base %>%
    dplyr::rename(QT_CENSO = QT_ALUNOS_AJUSTADA) %>%
    dplyr::left_join(tb_censo_h1, by=c("CO_ENTIDADE", "CO_ETAPA_ENSINO")) %>%
    dplyr::rename(QT_H1 = QT_ALUNOS_AJUSTADA) %>%
    dplyr::left_join(tb_censo_h2, by=c("CO_ENTIDADE", "CO_ETAPA_ENSINO")) %>%
    dplyr::rename(QT_H2 = QT_ALUNOS_AJUSTADA) %>%
    dplyr::select(-NU_ANO_INDEX)

  rm(tb_censo_base, tb_censo_h1, tb_censo_h2)

  return(tb_censo_final_join_pres)

}
