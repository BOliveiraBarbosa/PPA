# Setup/Config -----------------------------------------------------------------

library(tidyverse)
library(unglue)
library(DBI)
library(arrow)

# Read data --------------------------------------------------------------------

diretorio_busdata <- "busdata"

arquivos_lidos <- list.files(diretorio_busdata) %>%
  enframe(value = "arquivo") %>%
  rowwise() %>%
  mutate(
    conteudo = list(str_glue("{diretorio_busdata}/{arquivo}") %>% read_parquet())
  ) %>%
  unnest(conteudo) %>%
  janitor::clean_names()

# BD Write ---------------------------------------------------------------------

banco_destino <- "bd/busdata.db"

conexao_db <- dbConnect(RSQLite::SQLite(), banco_destino)

dbWriteTable(conexao_db, name = "busdata", value = arquivos_lidos, overwrite = TRUE)

dbDisconnect(conexao_db)


