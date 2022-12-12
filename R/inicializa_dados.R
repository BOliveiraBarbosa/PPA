inicializa_dados <- function(banco_destino = "bd/busdata.db"){
  
  conexao_db <- dbConnect(RSQLite::SQLite(), banco_destino)
  
  diretorio_busdata <- "busdata"
  
  arquivos_lidos <- list.files(diretorio_busdata) %>%
    enframe(value = "arquivo") %>%
    rowwise() %>%
    mutate(
      conteudo = list(str_glue("{diretorio_busdata}/{arquivo}") %>% read_parquet())
    ) %>%
    unnest(conteudo) %>%
    janitor::clean_names()
  
  busdata_filtrado <- arquivos_lidos %>% 
    mutate(
      date = lubridate::mdy_hms(date),
      date_ymd = as.Date(date),
      date_hms = as_hms(date),
    ) %>% 
    filter(
      date_hms > as_hms("02:00:00") &
      date_hms < as_hms("04:00:00") &
      velocity == 0
    ) %>%
    select(lat, long) %>% 
    arrange(long)
  
  dbWriteTable(conexao_db, "busdata_filtrado", busdata_filtrado, overwrite = TRUE)
  
  dbDisconnect(conexao_db)
}
