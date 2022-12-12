# Setup/Config -----------------------------------------------------------------

library(tidyverse)
library(DBI)
library(plotly)

set.seed(123)

busdata <- read_csv("bd/busdata_filtrado.csv")

# Calcula eps ------------------------------------------------------------------

geosphere::distHaversine(c(-22.81653, -43.39437), c(-22.81653, -43.39887))
geosphere::distHaversine(c(-22.81653, -43.39437), c(-22.82272, -43.39437))

dist <- c(
  c(-22.81653, -43.39437) - c(-22.81653, -43.39887),
  c(-22.81653, -43.39437) - c(-22.82272, -43.39437)
)

dist <- min(dist[dist > 0])

min_pts <- 240

# DBSCAN 1/4 -------------------------------------------------------------------

busdata_cluster <- busdata %>% 
  slice_head(n = 100000)

db <- fpc::dbscan(busdata_cluster, eps = dist, MinPts = min_pts)

id_agrupamento = db[["cluster"]]
busdata_cluster$id_agrupamento = id_agrupamento

count <- max(busdata_cluster$id_agrupamento) # count <- 9

cluster_info <- as.data.frame(print(db))

num <- 1
write_csv(busdata_cluster, paste0("cluster/cluster_busdata_", num,".csv"))
write_csv(cluster_info, paste0("cluster/cluster_info_", num, ".csv"))

# DBSCAN 2/4 -------------------------------------------------------------------

busdata_cluster <- busdata %>% 
  slice_tail(n = 100000)

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

count <- max(busdata_cluster$id_agrupamento) # count <- 21

num <- 2
write_csv(busdata_cluster, paste0("cluster/cluster_busdata_", num,".csv"))
write_csv(cluster_info, paste0("cluster/cluster_info_", num, ".csv"))

# DBSCAN 3/4 -------------------------------------------------------------------

busdata_range <- busdata %>% 
  filter(row_number() %in% c(100000, n() - 100000)) %>% 
  select(long) %>% 
  t() %>% as.data.frame()

busdata_cluster <- busdata %>% 
  filter(
    long >= busdata_range$V1 - 2 * dist &
    long <= busdata_range$V2 + 2 * dist
  ) %>% 
  slice_head(n = 100000)
  
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

count <- max(busdata_cluster$id_agrupamento) # count <- 29

num <- 3
write_csv(busdata_cluster, paste0("cluster/cluster_busdata_", num,".csv"))
write_csv(cluster_info, paste0("cluster/cluster_info_", num, ".csv"))

# DBSCAN 4/4 -------------------------------------------------------------------

busdata_cluster <- busdata %>% 
  filter(
    long >= busdata_range$V1 - 2 * dist &
    long <= busdata_range$V2 + 2 * dist
  ) %>% 
  slice_tail(n = 95000)

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

count <- max(busdata_cluster$id_agrupamento) # count <- 32

num <- 4
write_csv(busdata_cluster, paste0("cluster/cluster_busdata_", num,".csv"))
write_csv(cluster_info, paste0("cluster/cluster_info_", num, ".csv"))
