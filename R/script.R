# Setup/Config -----------------------------------------------------------------

library(tidyverse)
library(unglue)
library(DBI)
library(arrow)

# Read data --------------------------------------------------------------------

diretorio_busdata <- "busdata"

arquivos_lidos <- list.files(diretorio_busdata) %>%
  enframe(value = "arquivo") %>%
  filter(name == 2) %>%
  rowwise() %>%
  mutate(
    conteudo = list(str_glue("{diretorio_busdata}/{arquivo}") %>% read_parquet())
  ) %>%
  unnest(conteudo) %>%
  janitor::clean_names()

dados_bus <- arquivos_lidos %>% 
  mutate(
    date = lubridate::dmy_hms(date)
  )

# BD Connect -------------------------------------------------------------------

banco_destino <- "bd/busdata.db"

conexao_db <- dbConnect(RSQLite::SQLite(), banco_destino)

dbWriteTable(conexao_db, name = "busdata", value = dados_bus, overwrite = TRUE)

dbDisconnect(conexao_db)


