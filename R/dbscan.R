# Setup/Config -----------------------------------------------------------------

library(tidyverse)
library(DBI)
library(plotly)

set.seed(123)

# Connect DB -------------------------------------------------------------------

banco <- "bd/busdata.db"

conexao <- dbConnect(drv = RSQLite::SQLite(), banco)

busdata <- tbl(conexao, "busdata_utilizados") %>% 
  collect() %>% 
  slice_sample(prop = 0.05, replace = FALSE)

dbDisconnect(conexao)

# DBSCAN -----------------------------------------------------------------------

geosphere::distHaversine(c(-22.81653, -43.39437), c(-22.81653, -43.39887))
geosphere::distHaversine(c(-22.81653, -43.39437), c(-22.82272, -43.39437))

dist <- c(
  c(-22.81653, -43.39437) - c(-22.81653, -43.39887),
  c(-22.81653, -43.39437) - c(-22.82272, -43.39437)
)

dist <- min(dist[dist > 0])

db <- fpc::dbscan(busdata, eps = dist, MinPts = 240)

cluster_out <- as.data.frame(print(db))

id_agrupamento = db[["cluster"]]
busdata$id_agrupamento = id_agrupamento

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





