#' @title Detect the number of processor cores
#'
#' @description Wrapper para \code{parallel::detectCores}
#'
#' @param ... demais parâmetros aceitos para \code{parallel::detectCores}.
#'
#' @return \code{vector} do tipo \code{numeric} contendo o número de cores presente
#'     na máquina.
#'
#' @family clusters
#'
#' @examples
#' \dontrun{
#'
#' n_clusters <- detect_cores()
#'
#' }
#' @export
#'
detect_cores <- function(...){

  return(parallel::detectCores(...))

}
