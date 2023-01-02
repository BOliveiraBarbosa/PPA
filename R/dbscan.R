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
  
  busdata_max <- busdata %>% 
    filter(row_number() %in% n())
  
  limit <- tribble(
    ~lat, ~long,
    -Inf, -Inf
  )
  
  count <- 0
  i <- 1
  
  # Calcula os "Cluster" por regiões
  while(limit$long < busdata_max$long){
    
    busdata_cluster <- busdata %>%
      filter(long >= limit$long) %>% # limit$long - 2 * dist
      slice_head(n = 50000)
    
    limit <- busdata_cluster %>% 
      filter(row_number() %in% n())
    
    count <- calcula_dbscan(busdata_cluster, dist, min_pts, i, count)
    
    print.data.frame(limit)
    
    i <- i + 1
  }
  
  dbDisconnect(conexao)
  
  # Junta os "Cluster" separados por regiões
  conexao <- dbConnect(drv = RSQLite::SQLite(), "bd/cluster.db")
  
  j <- 1
  k <- 1
  
  while(j < 6){ # 6 igual i - 1
    
    esq <- tbl(conexao, paste0("cluster_busdata_", j)) %>% collect()
    dir <- tbl(conexao, paste0("cluster_busdata_", j + 1)) %>% collect()
    dir_info <- tbl(conexao, paste0("cluster_info_", j + 1)) %>% collect()
    
    esq_end <- esq %>% 
      filter(id_agrupamento != 0) %>% 
      arrange(long) %>% 
      filter(row_number() %in% n())
    
    dir_start <- dir %>% 
      filter(id_agrupamento != 0) %>% 
      arrange(long) %>% 
      filter(row_number() %in% 1)
    
    if(dir_start$long - esq_end$long < dist){
      
      dir <- dir %>% 
        rowwise() %>% 
        mutate(
          id_agrupamento = case_when(
            id_agrupamento == 0 ~ id_agrupamento,
            id_agrupamento >= 1 ~ id_agrupamento - k
          )
        )
      
      colnames(dir_info) <- dir %>% 
        distinct(id_agrupamento) %>% 
        pull(id_agrupamento) %>% 
        sort()
      
      dbWriteTable(conexao, paste0("cluster_info_", k + 1), dir_info, overwrite = TRUE)
      
      dbWriteTable(conexao, paste0("cluster_busdata_", k + 1), dir, overwrite = TRUE)
      
      k <- k + 1
    }
    
    j <- j + 1
  }
  
  dbDisconnect(conexao)
}

calcula_dbscan <- function(
    busdata_cluster,
    dist,
    min_pts,
    num,
    count = 0,
    banco = "bd/cluster.db"
  ){
  
  tic(paste("\nCluster ", num, sep = ""))
  
  writeLines(paste("\n~~ ", num, " ~~\n", sep = ""))
  
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
  
  conexao <- dbConnect(drv = RSQLite::SQLite(), banco)

  dbWriteTable(conexao, paste0("cluster_busdata_", num), busdata_cluster, overwrite = TRUE)

  dbWriteTable(conexao, paste0("cluster_info_", num), cluster_info, overwrite = TRUE)

  dbDisconnect(conexao)
  
  count <- max(busdata_cluster$id_agrupamento)
  
  toc()

  return(count)
}
