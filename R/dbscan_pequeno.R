dbscan_pequeno <- function(){
  
  conexao <- dbConnect(RSQLite::SQLite(), "bd/cluster.db")
  
  cluster_busdata <- bind_rows(
    tbl(conexao, "cluster_busdata_1") %>% collect(), 
    tbl(conexao, "cluster_busdata_2") %>% collect(), 
    tbl(conexao, "cluster_busdata_3") %>% collect(), 
    tbl(conexao, "cluster_busdata_4") %>% collect(),
    tbl(conexao, "cluster_busdata_5") %>% collect(),
    tbl(conexao, "cluster_busdata_6") %>% collect(),
  ) %>% 
    group_by(id_agrupamento) %>% 
    summarise(
      n = n(),
      lat_max = max(lat),
      lat_min = min(lat),
      long_max = max(long),
      long_min = min(long)
    ) %>% 
    filter(id_agrupamento != 0)
  
  qtd_r <- nrow(cluster_busdata)
  
  diretorio_busdata <- "busdata"
  
  arquivos_lidos <- list.files(diretorio_busdata) %>%
    enframe(value = "arquivo") %>%
    rowwise() %>%
    mutate(
      conteudo = list(str_glue("{diretorio_busdata}/{arquivo}") %>% read_parquet())
    ) %>%
    unnest(conteudo) %>%
    janitor::clean_names()
  
  busdata_filtrado_pequeno <- arquivos_lidos %>% 
    mutate(
      date = lubridate::mdy_hms(date),
      date_ymd = as.Date(date),
      date_hms = as_hms(date),
    ) %>% 
    filter(
      velocity == 0 & 
        date_hms > as_hms("00:00:00") &
        date_hms < as_hms("05:00:00")
    ) %>%
    select(lat, long)
  
  for (i in 1:qtd_r) {
    busdata_filtrado_pequeno <- busdata_filtrado_pequeno %>% 
      mutate(
        cluster = if_else(
          long > cluster_busdata$long_min[i] & 
            long < cluster_busdata$long_max[i] & 
            lat > cluster_busdata$lat_min[i] & 
            lat < cluster_busdata$lat_max[i],
          "cluster",
          "NA"
        )
      ) %>% 
      filter(cluster == "NA") %>% 
      arrange(long) %>% 
      select(-cluster)
  }
  
  dist <- c(
    c(-22.81653, -43.39437) - c(-22.81653, -43.39887),
    c(-22.81653, -43.39437) - c(-22.82272, -43.39437)
  )
  
  dist <- min(dist[dist > 0])
  
  min_pts <- 240
  
  count <- calcula_dbscan(busdata_filtrado_pequeno, dist, min_pts, 7, 0)
}
