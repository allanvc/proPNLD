#' @title Insert Table into Data Base
#'
#' @description Grava a tabela informada no banco de dados
#'
#' @param conn String contendo a "string de conexão" retornada por
#'     \code{\link{connect_censo_db}}.
#' @param tb_censo_final \code{tibble} contendo uma tabela final da projeção do
#'     Censo Escolar. Pode ser a tabela técnica (TEC) ou de apresentação (APR).
#' @param schema String contendo o nome do schema do banco, ex:
#'     \code{"INAPSISPRO_FNDE"}.
#' @param tab String contendo o nome da tabela que receberá o insert no banco.
#'
#' @return \code{TRUE} caso a gravação seja bem sucedida.
#'
#' @family writing
#'
#' @examples
#' \dontrun{
#'
#' insert_table_into_db(conn, tb_censo_final, schema, tab)
#'
#' }
#' @export
#'
insert_table_into_db <- function(conn, tb_censo_final, schema, tab) {

  # Sys.setenv(TZ = "GMT")
  # Sys.setenv(ORA_SDTZ = "GMT")
  Sys.setenv(TZ = "America/Sao_Paulo")
  Sys.setenv(ORA_SDTZ = "America/Sao_Paulo")
  # sol: https://stackoverflow.com/questions/29917011/using-roracle-dbwritetable-to-write-posixct-back-to-oracle-db

  insStr<- paste0("insert into ", paste(schema, tab, sep="."), " values(",
                  paste0(":", 1:ncol(tb_censo_final), collapse = ", "), ")")

  tryCatch({
    ROracle::dbGetQuery(conn, insStr, data.frame(tb_censo_final))

    ROracle::dbCommit(conn)

  }, error = function(e){
    ROracle::dbCommit(conn) # precisamos dar
    #.. o commit sempre para evitar que a tabela fique presa, principalmente
    #.. em casos de erros na insercao
    # o container nao executa o finally se der pau
    stop(e$message)

  })

  return(TRUE)

}
