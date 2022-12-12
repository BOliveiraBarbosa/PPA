# Setup/Config -----------------------------------------------------------------

library(tidyverse)
library(plotly)

# Read data --------------------------------------------------------------------

diretorio_busdata <- "cluster"

cluster_bus <- list.files(diretorio_busdata) %>%
  enframe(value = "arquivo") %>% 
  slice_head(n = 4) %>%
  rowwise() %>%
  mutate(
    conteudo = list(str_glue("{diretorio_busdata}/{arquivo}") %>% read_csv())
  ) %>%
  unnest(conteudo)

# Resultados -------------------------------------------------------------------

# Tratar isso aqui melhor 
num_agrupamento <- cluster_bus %>% 
  group_by(id_agrupamento) %>% 
  summarise(
    n = n(),
    lat_max = max(lat),
    lat_min = min(lat),
    long_max = max(long),
    long_min = min(long)
  ) %>%  
  write_csv("resultados/resultados_agrupamento.csv")


# Plot Bae ---------------------------------------------------------------------

fig <- cluster_bus %>%
  plot_ly(
    lat = ~lat,
    lon = ~long,
    marker = list(color = "black"),
    opacity = 0.5,
    type = 'scattermapbox',
    mode = "markers",
    hoverinfo = "text",
    text = ~paste("Lat:", lat, "Long:" , long)
  ) %>% 
  layout(
    mapbox = list(
      style = "open-street-map",
      zoom = 9.5,
      center = list(lat = -22.9125642, lon = -43.45)
    )
  ) %>% 
  suppressWarnings()

fig

# Plot -------------------------------------------------------------------------

cluster_bus_parte <- cluster_bus_parte %>% 
  filter(id_agrupamento != 0)

fig <- cluster_bus_parte %>% 
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

# Plot Region ------------------------------------------------------------------

num <- 1
cluster_bus_parte <- read_csv(paste0("cluster/cluster_busdata_", num,".csv")) %>% 
  filter(row_number() %in% c(1, n()))

num <- 2
cluster_bus_2 <- read_csv(paste0("cluster/cluster_busdata_", num,".csv")) %>% 
  filter(row_number() %in% c(1, n()))

num <- 3
cluster_bus_3 <- read_csv(paste0("cluster/cluster_busdata_", num,".csv")) %>% 
  filter(row_number() %in% c(1, n()))

num <- 4
cluster_bus_4 <- read_csv(paste0("cluster/cluster_busdata_", num,".csv")) %>% 
  filter(row_number() %in% c(1, n()))

fig <- plot_ly(
  type = 'scattermapbox',
  mode = "markers+lines",
  lon = c(-43.39362, -43.39362, -43.71682, -43.71682, -43.39362),
  lat = c(-22.75318, -23.03097, -23.03097, -22.75318, -22.75318),
  marker = list(size = 10)
) %>% 
  add_trace(
    type = 'scattermapbox',
    mode = "markers+lines",
    lon = c(-43.16153, -43.16153, -43.34904, -43.34904, -43.16153),
    lat = c(-22.75318, -23.03097, -23.03097, -22.75318, -22.75318),
    marker = list(size = 10)
  ) %>% 
  add_trace(
    type = 'scattermapbox',
    mode = "markers+lines",
    lon = c(-43.34880, -43.34880, -43.40157, -43.40157, -43.34880),
    lat = c(-22.75318, -23.03097, -23.03097, -22.75318, -22.75318),
    marker = list(size = 10)
  ) %>% 
  add_trace(
    type = 'scattermapbox',
    mode = "markers+lines",
    lon = c(-43.34065, -43.34065, -43.35700, -43.35700, -43.34065),
    lat = c(-22.75318, -23.03097, -23.03097, -22.75318, -22.75318),
    marker = list(size = 10)
  ) %>% 
  layout(
    mapbox = list(
      style = "open-street-map",
      zoom = 9.5,
      center = list(lat = -22.9, lon = -43.45)
    )
  ) %>% 
  hide_colorbar() %>% 
  suppressWarnings()

fig
