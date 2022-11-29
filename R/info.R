# Setup/Config -----------------------------------------------------------------

library(tidyverse)
library(DBI)
library(plotly)

# Connect DB -------------------------------------------------------------------

banco <- "bd/busdata.db"

conexao <- dbConnect(drv = RSQLite::SQLite(), banco)

dbListTables(conexao)

busdata <- tbl(conexao, "busdata") %>% 
  collect()

bus_linhas <- as.data.frame(unique(busdata$line))

bus_unicos <- as.data.frame(unique(busdata$busid))

cluster_bus <- tbl(conexao, "cluster_bus") %>% 
  collect()

cluster_db <- tbl(conexao, "cluster_db") %>% 
  collect()

dbDisconnect(conexao)

# Plot -------------------------------------------------------------------------

cluster_bus <- cluster_bus %>%
  filter(id_agrupamento != 0)

fig <- cluster_bus %>% 
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