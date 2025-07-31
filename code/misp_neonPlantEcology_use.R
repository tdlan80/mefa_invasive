## Date originated: 2025-07-30
## Date recent modification:
##
## Purpose: Attempting to use the neonPlants R package
## to access the plant presence and percent cover data product

## Load R packages

## Install neonPlants
# library(devtools)
# devtools::install_github("NEONScience/neonPlants")

library(pacman)
#library(neonPlants)

pacman::p_load(tidyverse, neonUtilities, neonPlantEcology)


## Get NEON data

NEON_TOKEN <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJtYXR0LmxhbW1lbnNAZ21haWwuY29tIiwic2NvcGUiOiJyYXRlOnB1YmxpYyIsImlzcyI6Imh0dHBzOi8vZGF0YS5uZW9uc2NpZW5jZS5vcmcvIiwiZXhwIjoxOTExNTg3MTI0LCJpYXQiOjE3NTM5MDcxMjQsImVtYWlsIjoibWF0dC5sYW1tZW5zQGdtYWlsLmNvbSJ9.Q9-HbXQpbfYmiMpeMA07wY-7_mmr8bCxkJXtAfc-1hC_pp8FUIm8YV4D9ylTYrZSklK9lpwYWDPGNhNfaAAr-g" 

all_sites <- npe_site_ids(by = "all") 
d <- npe_download(sites = all_sites, token = NEON_TOKEN) 
di <- npe_summary(d, scale = "site", timescale = "all")

species_occurrence_matrix <- npe_community_matrix(d, 
                                                  scale = "1m", 
                                                  binary=FALSE)

npe_cm_metadata(species_occurrence_matrix)


## Get info on select sites
select_sites <- c("SCBI", "GRSM", "HARV")
select_data <- npe_download(sites = select_sites, token = NEON_TOKEN) 

species_occurrence_matrix <- npe_community_matrix(select_data, 
                                                  scale = "1m", 
                                                  binary=FALSE)

npe_cm_metadata(species_occurrence_matrix) %>% View()
npe_summary(select_data, scale = "1m") %>% View()

species_data_lf <- npe_longform(select_data)
species_spatial <- npe_plot_centroids(species_occurrence_matrix)


SCBI <- npe_download(sites = "SCBI", token = NEON_TOKEN) 
SCBI_occ_mat <- npe_community_matrix(SCBI,
                                     scale = "1m",
                                     binary = FALSE)
