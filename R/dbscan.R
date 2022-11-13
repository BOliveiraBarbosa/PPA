# Setup/Config -----------------------------------------------------------------

library(tidyverse)
library(DBI)
library(hms)
library(plotly)

# Connect DB -------------------------------------------------------------------

banco <- "bd/busdata.db"

conexao <- dbConnect(drv = RSQLite::SQLite(), banco)

busdata <- tbl(conexao, "busdata") %>% 
  collect() %>% 
  # filter(name == 1) %>%
  mutate(
    date = as.POSIXct(date, origin="1970-01-01", tz="UTC"),
    date_ymd = as.Date(date),
    date_hms = as_hms(date),
  ) %>% 
  filter(date_hms > as_hms("00:00:00") & date_hms < as_hms("06:00:00")) %>%
  # filter(velocity == 0) %>% 
  select(lat, long)

dbDisconnect(conexao)

# DBSCAN -----------------------------------------------------------------------

geosphere::distHaversine(c(-22.81653, -43.39437), c(-22.81653, -43.39887))
geosphere::distHaversine(c(-22.81653, -43.39437), c(-22.82272, -43.39437))

dist <- c(
  c(-22.81653, -43.39437) - c(-22.81653, -43.39887),
  c(-22.81653, -43.39437) - c(-22.82272, -43.39437)
)

dist <- min(dist[dist > 0])

set.seed(123)

db <- fpc::dbscan(busdata, eps = dist, MinPts = 240)

print(db)

# Plot -------------------------------------------------------------------------

plot(db, busdata, main = "DBSCAN", frame = FALSE)

busdata <- busdata %>% 
  mutate(
    id_agrupamento = db[["cluster"]]
  )

# busdata <- busdata %>% 
#   filter(id_agrupamento != 0)

fig <- busdata %>% 
  plot_ly(
    lat = ~lat,
    lon = ~long,
    color = ~id_agrupamento,
    type = "scattermapbox",
    mode = "markers",
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









