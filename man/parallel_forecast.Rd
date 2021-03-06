% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parallel-forecast.R
\name{parallel_forecast}
\alias{parallel_forecast}
\title{Parallel Forecast}
\usage{
parallel_forecast(
  n_clusters,
  grupos,
  ano_FIRST_censo,
  ano_LAST_censo,
  model = "ets"
)
}
\arguments{
\item{n_clusters}{String contendo a "string de conexão" retornada por
\code{\link{connect_censo_db}}.}

\item{grupos}{Lista contendo os grupos de municípios após \code{\link{split_mun_groups}}.}

\item{ano_FIRST_censo}{\code{vector} do tipo \code{numeric} contendo o ano
inicial da série, retornado pela função \code{\link{get_year_FIRST_censo}}.}

\item{ano_LAST_censo}{\code{vector} do tipo \code{numeric} contendo o ano
final da série, retornado pela função \code{\link{get_year_LAST_censo}}.}

\item{model}{String contendo o tipo de modelo a ser empregado para o fitting e
o forecasting. Por enquanto, aceita apenas a opção \code{"ets"}
(Exponential Smoothing).}
}
\value{
\code{list} de \code{tibble}s contendo o forecast output de todos os modelos.
}
\description{
Executa o forecast para etapa de ensino/escola de forma paralela
    nos grupos de municípios.
}
\examples{
\dontrun{

forecast_output <- paralell_forecast(n_clusters, grupos, ano_FIRST_censo,
                                     ano_LAST_censo, model = "ets")

}
}
\concept{parallel forecast}
