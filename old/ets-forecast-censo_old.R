# script projeção PNLD
# Allan Vieira
# allan.quadros@fnde.gov.br
# -----------------------
# ets_forecast_censo v0.0.6
# 2020-06-22
# ------------------------
# baseado em: https://business-science.github.io/sweep/articles/SW01_Forecasting_Time_Series_Groups.html


ets_forecast_censo <- function(grupos, ano_FIRST_censo, ano_LAST_censo){
  
  out <- lapply(1:length(grupos), function(i){
    # i = 1
    grupos[[i]] %>%
      
      # parte1
      dplyr::group_by(CO_MUNICIPIO, CO_ENTIDADE, CO_ETAPA_ENSINO) %>% # talvez mais demorado
      dplyr::select(NU_ANO_CENSO, QTD_ALUNOS) %>%
      
      # parte2
      dplyr::group_by(CO_MUNICIPIO, CO_ENTIDADE, CO_ETAPA_ENSINO) %>% # talvez mais demorado
      tidyr::nest(.key = "data.tbl") %>% # nest nao dah para fazer direto no Oracle
      
      #parte3
      dplyr::mutate(data.ts = purrr::map(.x = data.tbl,
                           .f = timetk::tk_ts,
                           select = -NU_ANO_CENSO,
                           start = ano_FIRST_censo,
                           end = ano_LAST_censo
                           # freq = 1
      )) %>%
      
      #parte fit
      dplyr::mutate(fit.ets = purrr::map(data.ts, forecast::ets)) %>%
      
      #parte forecast
      dplyr::mutate(fcast.ets = purrr::map(fit.ets, forecast::forecast, h = 2)) %>%
      #cauclar erros
      dplyr::mutate(error.ets = purrr::map(fcast.ets, forecast::accuracy)) %>% 
      dplyr::mutate(error.ets = purrr::map(error.ets, tibble::as_tibble)) %>% 
      tidyr::unnest(error.ets) %>%
      #parte tidy/apres
      # mutate(sweep = map(fcast.ets, sw_sweep, fitted = FALSE, timetk_idx = FALSE)) %>%
      dplyr::mutate(sweep = purrr::map(fcast.ets, sweep::sw_sweep, fitted = TRUE, timetk_idx = FALSE)) %>%
      tidyr::unnest(sweep) %>%
      dplyr::filter(key == "forecast")
    # deixar para ordenar a hora que tivermos tudo
  })
  return(out)
}
