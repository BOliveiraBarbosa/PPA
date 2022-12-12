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
  unnest(conteudo) %>%
  janitor::clean_names()

# Plot -------------------------------------------------------------------------

cluster_bus <- cluster_bus %>% 
  filter(id_agrupamento != 0) %>% 
  slice_sample(prop = 0.8,replace = FALSE)

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
      zoom = 9.5,
      center = list(lat = -22.9, lon = -43.45)
    )
  ) %>% 
  hide_colorbar() %>% 
  suppressWarnings() # Remove Warnings 

fig
