# Setup/Config -----------------------------------------------------------------

library(tidyverse)
library(DBI)
library(hms)
library(plotly)

set.seed(123)

# Connect DB -------------------------------------------------------------------

banco <- "bd/busdata.db"

conexao <- dbConnect(drv = RSQLite::SQLite(), banco)

busdata <- tbl(conexao, "busdata") %>% 
  filter(velocity == 0) %>% 
  collect() %>% 
  mutate(
    date = lubridate::mdy_hms(date),
    date_ymd = as.Date(date),
    date_hms = as_hms(date),
  ) %>% 
  filter(date_hms > as_hms("02:00:00") & date_hms < as_hms("04:00:00")) %>%
  select(lat, long) %>% 
  slice_sample(prop = 0.30, replace = FALSE)

dbWriteTable(conexao_db, name = "busdata_utilizados", value = busdata, overwrite = TRUE)

dbDisconnect(conexao)

# DBSCAN -----------------------------------------------------------------------

geosphere::distHaversine(c(-22.81653, -43.39437), c(-22.81653, -43.39887))
geosphere::distHaversine(c(-22.81653, -43.39437), c(-22.82272, -43.39437))

dist <- c(
  c(-22.81653, -43.39437) - c(-22.81653, -43.39887),
  c(-22.81653, -43.39437) - c(-22.82272, -43.39437)
)

dist <- min(dist[dist > 0])

db <- fpc::dbscan(busdata, eps = dist, MinPts = 480)

cluster_out <- as.data.frame(print(db))

busdata <- busdata %>% 
  mutate(
    id_agrupamento = db[["cluster"]]
  )

# Plot -------------------------------------------------------------------------

print(db)

fig <- busdata %>% 
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
      zoom = 15,
      center = list(
        lat = -22.9125642,
        lon = -43.2240898
      )
    )
  ) %>% 
  hide_colorbar() %>% 
  suppressWarnings() # Remove Warnings 

fig

# BD Write ---------------------------------------------------------------------

banco_destino <- "bd/busdata.db"

conexao_db <- dbConnect(RSQLite::SQLite(), banco_destino)

dbWriteTable(conexao_db, name = "cluster_bus", value = busdata, overwrite = TRUE)
dbWriteTable(conexao_db, name = "cluster_db", value = cluster_out, overwrite = TRUE)

dbDisconnect(conexao_db)





