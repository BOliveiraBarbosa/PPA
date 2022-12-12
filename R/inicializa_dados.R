# Setup/Config -----------------------------------------------------------------

library(tidyverse)
library(unglue)
library(arrow)
library(hms)

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

busdata <- arquivos_lidos %>% 
  mutate(
    date = lubridate::mdy_hms(date),
    date_ymd = as.Date(date),
    date_hms = as_hms(date),
  ) %>% 
  filter(
    date_hms > as_hms("02:00:00") &
    date_hms < as_hms("04:00:00") &
    velocity == 0
  ) %>%
  select(lat, long) %>% 
  arrange(long) %>% 
  write_csv(file = "bd/busdata_filtrado.csv")
