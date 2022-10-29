# Setup/Config -----------------------------------------------------------------
library(tidyverse)
library(unglue)
library(fpc)
library(plotly)

# Read data --------------------------------------------------------------------

diretorio_busdata <- "busdata"

arquivos_lidos <- list.files(diretorio_busdata) %>%
  enframe(value = "arquivo") %>%
  filter(name == min(name)) %>% 
  rowwise() %>%
  mutate(
    conteudo = list(str_glue("{diretorio_busdata}/{arquivo}") %>% arrow::read_parquet())
  ) %>%
  unnest(conteudo) %>%
  janitor::clean_names() %>% 
  select(lat, long) %>% 
  slice_tail(n = 1000)

# DBScan -----------------------------------------------------------------------

dbscan::kNNdistplot(arquivos_lidos, k = 4)

set.seed(123)
db <- fpc::dbscan(arquivos_lidos, eps = 0.02, MinPts = 5)

fig <- ggplotify::as.ggplot(~plot(db, arquivos_lidos, main = "DBSCAN", frame = FALSE))

fig


# Plot -------------------------------------------------------------------------

fig <- arquivos_lidos %>% 
  plot_ly(
    lat = ~lat,
    lon = ~long,
    marker = list(color = "black"),
    type = "scattermapbox"
  )
fig <- fig %>%
  layout(
    mapbox = list(
      style = "open-street-map",
      zoom = 15,
      center = list(
        lat = -22.9125642,
        lon = -43.2240898
      )
    )
  ) 

fig
