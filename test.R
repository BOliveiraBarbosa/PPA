# TODO: .rs.restartR() - estender como essa função funciona e continua rodando

# IDEIA - antes de usar ele guarda em um txt o num da function "calcula_dbscan"
# Assim vai saber onde parou, e coloca no iniciar do R para rodar o script 
# (Vê como faz) ^^
# Na fução dbscan_parte terá que ter um if para rodar as partes de onde parou

# Config/Setup -----------------------------------------------------------------

library(tidyverse)
library(unglue)
library(DBI)
library(arrow)
library(hms)
library(DBI)
library(plotly)

set.seed(123)

# Dados Cluster ----------------------------------------------------------------

conexao <- dbConnect(RSQLite::SQLite(), "bd/cluster.db")

dbListTables(conexao)

cluster_busdata <- bind_rows(
  tbl(conexao, "cluster_busdata_1") %>% collect(), 
  tbl(conexao, "cluster_busdata_2") %>% collect(), 
  tbl(conexao, "cluster_busdata_3") %>% collect(), 
  tbl(conexao, "cluster_busdata_4") %>% collect()
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

# Dados ------------------------------------------------------------------------

diretorio_busdata <- "busdata"

arquivos_lidos <- list.files(diretorio_busdata) %>%
  enframe(value = "arquivo") %>%
  rowwise() %>%
  mutate(
    conteudo = list(str_glue("{diretorio_busdata}/{arquivo}") %>% read_parquet())
  ) %>%
  unnest(conteudo) %>%
  janitor::clean_names()

# Filtrar Dados ----------------------------------------------------------------

busdata_filtrado <- arquivos_lidos %>% 
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
  busdata_filtrado <- busdata_filtrado %>% 
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

# DBSCAN -----------------------------------------------------------------------

dist <- c(
  c(-22.81653, -43.39437) - c(-22.81653, -43.39887),
  c(-22.81653, -43.39437) - c(-22.82272, -43.39437)
)

dist <- min(dist[dist > 0])

min_pts <- 240

db <- fpc::dbscan(busdata_filtrado, eps = dist, MinPts = min_pts)

id_agrupamento = db[["cluster"]]
busdata_filtrado$id_agrupamento = id_agrupamento

count <- 32

busdata_filtrado <- busdata_filtrado %>% 
  rowwise() %>% 
  mutate(
    id_agrupamento = case_when(
      id_agrupamento == 0 ~ id_agrupamento,
      id_agrupamento >= 1 ~ id_agrupamento + count
    )
  )

cluster_info <- as.data.frame(print(db))

colnames(cluster_info) <- busdata_filtrado %>% 
  distinct(id_agrupamento) %>% 
  pull(id_agrupamento) %>% 
  sort()

num <- 5

# dbWriteTable(conexao, paste0("cluster_busdata_", num), busdata_cluster, overwrite = TRUE)

# dbWriteTable(conexao, paste0("cluster_info_", num), cluster_info, overwrite = TRUE)

# Plot -------------------------------------------------------------------------

cluster_busdata_sample <- busdata_filtrado %>% 
  filter(id_agrupamento != 0)

fig <- cluster_busdata_sample %>% 
  plot_ly(
    lat = ~lat,
    lon = ~long,
    color = ~id_agrupamento,
    opacity = 0.5,
    type = "scattermapbox",
    mode = "markers",
    marker = list(colorscale = "Rainbow"),
    hoverinfo = "text",
    text = ~paste(
      "Lat:", lat,
      "Long:" , long, "<br>",
      " Agrupamento:", id_agrupamento
    )
  ) %>%
  layout(
    mapbox = list(
      style = "open-street-map",
      zoom = 9.5,
      center = list(lat = -22.9, lon = -43.45)
    )
  ) %>% 
  hide_colorbar() %>% 
  suppressWarnings() # Remove Warnings 

fig
