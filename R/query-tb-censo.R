#' @title Query the table containing the time series of all census data
#'
#' @description Executa uma query na base "INAPSISPRO_FNDE.TB_CENSO_TS_QTD_ALUNOS" no FNDE
#'
#' @param conn String contendo a "string de conexão" retornada por
#'     \code{\link{connect_censo_db}}.
#' @param schema String contendo o nome do schema do banco, ex:
#'     \code{"INAPSISPRO_FNDE"}.
#' @param tab String contendo o nome da tabela em que vive a base do censo, ex:
#'     \code{"TB_CENSO_TS_QTD_ALUNOS"}.
#'
#' @return Objeto do tipo \code{tibble} do \code{dbplyr} que ainda vive no SGBD.
#'
#'
#' @family query
#'
#' @examples
#' \dontrun{
#'
#' schema = "INAPSISPRO_FNDE"
#' tab = "TB_CENSO_TS_QTD_ALUNOS"
#' tb_censo <- query_tb_censo(conn, schema, tab)
#'
#' }
#' @export
#'
query_tb_censo <- function(conn, schema, tab) {

  # tb_censo <- tbl(con5, in_schema('INAPSISPRO_FNDE', "TB_CENSO_TS_QTD_ALUNOS"))

  tb_censo <- dplyr::tbl(conn, dbplyr::in_schema(schema, tab))

  return(tb_censo)

}
