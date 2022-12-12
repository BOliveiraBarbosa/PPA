calcula_dbscan <- function(
    busdata_cluster,
    dist,
    min_pts,
    num,
    count = 0,
    banco = "bd/cluster.db"
  ){
  
  tic()
  
  conexao <- dbConnect(drv = RSQLite::SQLite(), banco)
  
  db <- fpc::dbscan(busdata_cluster, eps = dist, MinPts = min_pts)
  
  id_agrupamento = db[["cluster"]]
  busdata_cluster$id_agrupamento = id_agrupamento
  
  busdata_cluster <- busdata_cluster %>% 
    rowwise() %>% 
    mutate(
      id_agrupamento = case_when(
        id_agrupamento == 0 ~ id_agrupamento,
        id_agrupamento >= 1 ~ id_agrupamento + count
      )
    )
  
  cluster_info <- as.data.frame(print(db))
  
  colnames(cluster_info) <- busdata_cluster %>% 
    distinct(id_agrupamento) %>% 
    pull(id_agrupamento) %>% 
    sort()
  
  dbWriteTable(conexao, paste0("cluster_busdata_", num), busdata_cluster, overwrite = TRUE)
  
  dbWriteTable(conexao, paste0("cluster_info_", num), cluster_info, overwrite = TRUE)
  
  dbDisconnect(conexao)
  
  count <- max(busdata_cluster$id_agrupamento)
  
  toc()
  
  # rm(list=ls())
  # .rs.restartR()
}

dbscan_parte <- function(banco = "bd/busdata.db"){
  
  set.seed(123)
  
  conexao <- dbConnect(drv = RSQLite::SQLite(), banco)
  
  busdata <- tbl(conexao, "busdata_filtrado") %>% 
    collect()
  
  dist <- c(
    c(-22.81653, -43.39437) - c(-22.81653, -43.39887),
    c(-22.81653, -43.39437) - c(-22.82272, -43.39437)
  )
  
  dist <- min(dist[dist > 0])
  
  min_pts <- 240
  
  # 1/4 ------------------------------------------------------------------------
  
  # writeLines("\n~~ 1/4 ~~\n")
  
  # busdata_cluster <- busdata %>% 
  #   slice_head(n = 100000)
  # 
  # calcula_dbscan(busdata_cluster, dist, min_pts, 1, 0)
  
  # 2/4 ------------------------------------------------------------------------
  
  # writeLines("\n~~ 2/4 ~~\n")
  
  # busdata_cluster <- busdata %>% 
  #   slice_tail(n = 100000)
  # 
  # calcula_dbscan(busdata_cluster, dist, min_pts, 2, 9)
  
  # 3/4 ------------------------------------------------------------------------
  
  # writeLines("\n~~ 3/4 ~~\n")
  
  # busdata_range <- busdata %>% 
  #   filter(row_number() %in% c(100000, n() - 100000)) %>% 
  #   select(long) %>% 
  #   t() %>% as.data.frame()
  # 
  # busdata_cluster <- busdata %>% 
  #   filter(
  #     long >= busdata_range$V1 - 2 * dist &
  #       long <= busdata_range$V2 + 2 * dist
  #   ) %>% 
  #   slice_head(n = 100000)
  # 
  # calcula_dbscan(busdata_cluster, dist, min_pts, 3, 21)
  
  # 4/4 ------------------------------------------------------------------------
  
  writeLines("\n~~ 4/4 ~~\n")
  
  busdata_range <- busdata %>%
    filter(row_number() %in% c(100000, n() - 100000)) %>%
    select(long) %>%
    t() %>% as.data.frame()

  busdata_cluster <- busdata %>%
    filter(
      long >= busdata_range$V1 - 2 * dist &
        long <= busdata_range$V2 + 2 * dist
    ) %>%
    slice_tail(n = 95000)

  calcula_dbscan(busdata_cluster, dist, min_pts, 4, 29)
}
