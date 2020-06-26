#' @title Split Data in Groups
#'
#' @description Divide a base do Censo em grupos iguais ao número de cores como
#'     preparação para a execução paralelizada.
#'
#' @param tb_censo_full \code{tibble} contendo a base completa do censo
#'     após tratamento e limpeza por meio da função \code{\link{extract_and_clean_censo_full}}.
#' @param n_clusters String contendo a "string de conexão" retornada por
#'     \code{\link{connect_censo_db}}.
#'
#' @return \code{tibble} contendo a base completa do censo após tratamento e limpeza.
#'
#' @family clusters
#'
#' @examples
#' \dontrun{
#'
#' tb_censo_full <- extract_and_clean_censo_full(tb_censo)
#'
#' }
#' @export
#'
split_mun_groups <- function(tb_censo_full, n_clusters) {


  X <- split(tb_censo_full, tb_censo_full$CO_MUNICIPIO)


  rm(tb_censo_full)

  # ---------------------------
  # seprando em 5 partes iguais para 5 clusters diferentes:

  # 8 nucleos ( o servidor tem 9vCPUS)
  # usar a funcao split_in_sets que eu fiz no dourobot
  split_in_sets <- function(x,n){
    if(length(x) > n) split(x, cut(seq_along(x), n, labels = FALSE))
    else stop("Erro: numero de clusters nao pode ser maior que o numero de categorias.")
  }

  grupos <- split_in_sets(X, n_clusters)

  rm(X)

  return(grupos)

}
