#' @title Connect to Censo Time Series SGBD
#'
#' @description Executa uma query na base "INAPSISPRO_FNDE.TB_CENSO_TS_QTD_ALUNOS" no FNDE
#'
#' @param host String contendo o "host" de conexão, ex: \code{"exafnde02-scan2.fnde.gov.br"}.
#' @param port Integer indicando a porta para conexão com o SGBD, ex: \code{1521}.
#' @param user String contendo o nome de usuário.
#' @param pass String contendo a o password.
#' @param svc String contendo o "service" de conexão, ex: \code{"berilodr.fnde.gov.br"}.
#'
#' @return Objeto do tipo \code{character} que contém a string de conexão para o banco de dados.
#'
#'
#' @family connection
#'
#' @examples
#' \dontrun{
#'
#' host <- "exafnde02-scan2.fnde.gov.br"
#' port <- 1521
#' user <- "user"
#' pass <-"password"
#' svc <- "berilodr.fnde.gov.br"
#'
#'  conn <- connect_censo_db(host, port, user, pass, svc)
#'
#' }
#' @export
#'
# install.packages("ROracle.zip", repos = NULL)
# baseado em: https://business-science.github.io/sweep/articles/SW01_Forecasting_Time_Series_Groups.html
connect_censo_db <- function(host, port, user, pass, svc) {

  loadNamespace("ROracle") #sol: https://stackoverflow.com/questions/48114430/r-oracle-connect-via-dbidbdriveroracle-throws-error
  # senao nao carregar o driver

  drv <- DBI::dbDriver("Oracle")
  # host <- "exafnde02-scan2.fnde.gov.br"
  # port <- 1521
  # nome_usuario <- "mq_consulta"
  # senha<-"consultamq123"
  # svc <- "berilodr.fnde.gov.br"
  connect.string <- paste("(DESCRIPTION=","(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))","(CONNECT_DATA=(SERVICE_NAME=",
                          svc, ")))", sep = "")
  conn <-ROracle::dbConnect(drv, username = user, password = pass, dbname = connect.string)

  # DBI::dbListTables(conn, "INAPSISPRO_FNDE")

  return(conn)
}
