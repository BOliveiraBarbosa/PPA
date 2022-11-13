# Setup/Config -----------------------------------------------------------------

library(tidyverse)
library(DBI)

# Connect DB -------------------------------------------------------------------

banco <- "bd/busdata.db"

conexao <- dbConnect(drv = RSQLite::SQLite(), banco)

busdata <- tbl(conexao, "busdata") %>% 
  collect()

dbDisconnect(conexao)

bus_linhas <- as.data.frame(unique(busdata$line))

bus_unicos <- as.data.frame(unique(busdata$busid))
