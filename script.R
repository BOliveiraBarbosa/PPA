library(tidyverse)
library(unglue)
library(DBI)
library(arrow)
library(hms)
library(tictoc)

source("R/inicializa_dados.R")
source("R/dbscan.R")

inicializa_dados()
dbscan_parte()
