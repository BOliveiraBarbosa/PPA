library(tidyverse)
library(unglue)
library(DBI)
library(arrow)
library(hms)
library(tictoc)

tic("\nTempo Total")

source("R/inicializa_dados.R")
source("R/dbscan.R")
source("R/dbscan_pequeno.R")

inicializa_dados()
dbscan_parte()
dbscan_pequeno()

toc()
