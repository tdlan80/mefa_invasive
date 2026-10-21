# ==============================================================================
# Script: 01_pppc_download_process_all_sites.R
# Purpose: Download and process NEON Plant Presence and Percent Cover
#          (DP1.10058.001) for all 47 terrestrial sites and all available plots.
#
# Raw output (one file per site):
#   rawData/NEON/PPPC/raw_all_<SITE>.rds
#   Each file contains ALL tables returned by loadByProduct().
#
# Combined output:
#   rawData/NEON/PPPC/pppc_all_terrestrial_all_scales.RData
#   Contains:
#     pppc_plantDiv       - identified plant records at 1, 10, and 100 m2
#     pppc_subplotVisits  - surveyed subplot/event combinations, including
#                           subplots with no identified plant species
#     pppc_400m_presence  - distinct detected taxa per 400 m2 plot/event
#     sites_cached        - sites successfully processed
#
# Run before downstream analysis of cumulative and mean species richness.
# To refresh NEON downloads, delete the affected raw_all_<SITE>.rds files.
# ============================================================================== 

# Packages: install once, if needed, with install.packages().
packages <- c("tidyverse", "neonUtilities", "here")
missing_packages <- packages[!vapply(packages, requireNamespace,
                                     logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Install missing packages first: ", paste(missing_packages, collapse = ", "))
}

library(tidyverse)
library(neonUtilities)
library(here)

# ==============================================================================
# 1. Configuration
# ==============================================================================

# All 47 NEON terrestrial sites (including sites with historical data).
sites <- c(
  "ABBY", "BARR", "BART", "BLAN", "BONA",
  "CLBJ", "CPER", "DCFS", "DEJU", "DELA",
  "DSNY", "GRSM", "GUAN", "HARV", "HEAL",
  "JERC", "JORN", "KONA", "KONZ", "LAJA",
  "LENO", "MLBS", "MOAB", "NIWO", "NOGP",
  "OAES", "ONAQ", "ORNL", "OSBS", "PUUM",
  "RMNP", "SCBI", "SERC", "SJER", "SOAP",
  "SRER", "STEI", "STER", "TALL", "TEAK",
  "TOOL", "TREE", "UKFS", "UNDE", "WOOD",
  "WREF", "YELL"
)
stopifnot(length(sites) == 47L, !anyDuplicated(sites))

pppc_product <- "DP1.10058.001"

neon_token <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJ0c3VyYXNpbmdoZUBicmlkZ2V3LmVkdSIsInNjb3BlIjoicmF0ZTpwdWJsaWMiLCJpc3MiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnLyIsImV4cCI6MTkxMTY1NDYyMywiaWF0IjoxNzUzOTc0NjIzLCJlbWFpbCI6InRzdXJhc2luZ2hlQGJyaWRnZXcuZWR1In0.ZEonFGxXjd3jY7Azk28c9L_pq_CgKR--LuhLf6ZO2izUO7_eZHIE4XA_zyqeF0gmQ4aSCbTqLfPcL7GnBo3zrg"
neonUtilities::tokenDate(neon_token)

# neon_token <- Sys.getenv("NEON_TOKEN")

# Keep the settings used in the earlier script.
# 'current' is updated over time; use a named release for a frozen dataset.
pppc_release <- "current"
include_provisional <- TRUE

# TRUE rebuilds the combined dataset from the per-site raw files.
# It does NOT re-download a site if its raw cache already exists.
rebuild_processed <- FALSE

pppc_dir <- here::here("rawData", "NEON", "PPPC")
pppc_path <- file.path(pppc_dir, "pppc_all_terrestrial_all_scales.RData")

dir.create(pppc_dir, recursive = TRUE, showWarnings = TRUE)
if (!dir.exists(pppc_dir) || file.access(pppc_dir, 2) != 0) {
  stop("The PPPC output directory does not exist or is not writable: ", pppc_dir)
}

# ==============================================================================
# 2. Helper: download and process one site
# ==============================================================================

download_and_clean_site <- function(site, product, token, cache_dir) {
  
  raw_cache <- file.path(cache_dir, paste0("raw_all_", site, ".rds"))
  
  if (file.exists(raw_cache)) {
    message("[", site, "] Reading cached raw download")
    neon_dl <- readRDS(raw_cache)
  } else {
    message("[", site, "] Downloading all available PPPC tables")
    neon_dl <- neonUtilities::loadByProduct(
      dpID                = product,
      site                = site,
      package             = "expanded",
      release             = pppc_release,
      tabl                = "all",
      check.size          = FALSE,
      include.provisional = include_provisional,
      token               = if (nzchar(token)) token else NA_character_
    )
    
    # Preserve every table (including voucher and metadata tables).
    # Only a successful save creates the reusable raw cache.
    saveRDS(neon_dl, raw_cache)
    message("[", site, "] Raw tables saved: ", raw_cache)
  }
  
  required_tables <- c("div_1m2Data", "div_10m2Data100m2Data")
  if (!all(required_tables %in% names(neon_dl))) {
    stop("Missing required observation table(s) at ", site, ": ",
         paste(setdiff(required_tables, names(neon_dl)), collapse = ", "))
  }
  
  # Each 1 m2 observation is in the first table.
  d1 <- neon_dl$div_1m2Data %>%
    mutate(subplotID = as.character(subplotID),
           subplot_area_m2 = 1L, source_table = "div_1m2Data")
  
  # The second table mixes 10 m2 and 100 m2 observations.
  # Since 2024: 31_10_1 and 31_100.
  # Older naming: 31.1.10 and 31.
  d2 <- neon_dl$div_10m2Data100m2Data %>%
    mutate(
      subplotID = as.character(subplotID),
      subplot_area_m2 = case_when(
        str_detect(as.character(subplotID), "(_100$|^[0-9]+$)") ~ 100L,
        str_detect(as.character(subplotID), "(_10_|\\.10$)")  ~ 10L,
        TRUE ~ NA_integer_
      ),
      source_table = "div_10m2Data100m2Data"
    )
  
  # Do not silently assign the wrong scale if NEON changes subplot IDs.
  unknown <- unique(d2$subplotID[is.na(d2$subplot_area_m2) &
                                   !is.na(d2$subplotID)])
  if (length(unknown) > 0) {
    stop("Unrecognized subplotID(s) at ", site, ": ",
         paste(head(unknown, 10), collapse = ", "))
  }
  
  # Combine the two observation tables. Keep their different columns.
  dat <- bind_rows(d1, d2) %>%
    mutate(
      across(where(is.character), str_squish),
      year = lubridate::year(endDate),
      plot_subplot = paste0(plotID, "_", subplotID)
    )
  
  # Remove sampling events specifically marked as impractical.
  # NA is retained: this flag was not available throughout the time series.
  surveyed <- dat %>%
    filter(is.na(samplingImpractical) | samplingImpractical == "OK")
  
  # Preserve ONE row per surveyed subplot/event, including no-plant records.
  # This is the sampling-effort denominator for mean 1 m2 richness.
  subplot_visits <- surveyed %>%
    filter(!is.na(plotID), !is.na(subplotID), !is.na(eventID)) %>%
    distinct(siteID, plotID, eventID, subplotID,
             subplot_area_m2, .keep_all = TRUE) %>%
    select(siteID, plotID, eventID, subplotID,
           subplot_area_m2, year, endDate, samplingImpractical)
  
  # Plant species observations only. Preserve all other fields for review.
  # Do not aggregate across subplots or sampling events here.
  plant_div <- surveyed %>%
    filter(
      targetTaxaPresent == "Y",
      tolower(taxonRank) %in% c("species", "subspecies", "variety"),
      !is.na(scientificName),
      !is.na(plotID), !is.na(subplotID), !is.na(eventID)
    ) %>%
    mutate(binomialName = stringr::word(scientificName, 1, 2)) %>%
    relocate(binomialName, .after = scientificName)
  
  list(plants = plant_div, visits = subplot_visits)
}

# ==============================================================================
# 3. Load combined cache or build it from site downloads
# ==============================================================================

use_processed_cache <- FALSE

if (file.exists(pppc_path) && !rebuild_processed) {
  load(pppc_path)  # restores processed datasets and sites_cached
  
  if (exists("sites_cached") && identical(sort(sites_cached), sort(sites))) {
    use_processed_cache <- TRUE
    message("Using complete processed cache: ", pppc_path)
  } else {
    message("Processed cache does not cover all requested sites; rebuilding.")
  }
}

if (!use_processed_cache) {
  
  # Catch an error for one site without losing progress at other sites.
  results <- lapply(sites, function(s) {
    tryCatch(
      download_and_clean_site(s, pppc_product, neon_token, pppc_dir),
      error = function(e) {
        message("[", s, "] FAILED: ", conditionMessage(e))
        NULL
      }
    )
  })
  names(results) <- sites
  
  successful <- !vapply(results, is.null, logical(1))
  sites_cached <- sites[successful]
  failed_sites <- sites[!successful]
  
  if (length(sites_cached) == 0) stop("No sites processed successfully.")
  
  # Bind the per-site plant records and surveyed-subplot records separately.
  pppc_plantDiv <- bind_rows(lapply(results[successful], `[[`, "plants"))
  pppc_subplotVisits <- bind_rows(lapply(results[successful], `[[`, "visits"))
  
  # A preliminary plot-level species inventory (observed records only).
  # A plot is not automatically a COMPLETE 400 m2 survey; completeness
  # must be assessed separately using pppc_subplotVisits.
  pppc_400m_presence <- pppc_plantDiv %>%
    distinct(siteID, plotID, eventID, year, binomialName)
  
  save(pppc_plantDiv, pppc_subplotVisits, pppc_400m_presence,
       sites_cached, failed_sites, file = pppc_path)
  message("Processed datasets saved: ", pppc_path)
  
  if (length(failed_sites) > 0) {
    warning("Sites not processed: ", paste(failed_sites, collapse = ", "),
            ". Re-run to retry; successful sites remain cached.")
  }
}

# ==============================================================================
# 4. Summary and checks
# ==============================================================================

message("\n--- NEON PPPC summary ---")
message("Requested terrestrial sites: ", length(sites))
message("Successfully processed sites: ", length(sites_cached))
message("Sites with plant records: ", n_distinct(pppc_plantDiv$siteID))
message("Unique plots: ", n_distinct(pppc_subplotVisits$plotID))
message("Unique plot-events: ", nrow(distinct(pppc_subplotVisits,
                                              siteID, plotID, eventID)))
message("Plant observation rows: ", nrow(pppc_plantDiv))
message("Surveyed subplot-events: ", nrow(pppc_subplotVisits))

# Confirm the observation scales and sampling effort.
table(pppc_plantDiv$subplot_area_m2, useNA = "ifany")
table(pppc_subplotVisits$subplot_area_m2, useNA = "ifany")

# Missing site/event identifiers should be reviewed before richness analyses.
message("Missing eventID in plant records: ", sum(is.na(pppc_plantDiv$eventID)))

# Number of observations from each original table
table(pppc_plantDiv$source_table)



# Example: inspect all original raw tables at HARV.
# harv_raw <- readRDS(file.path(pppc_dir, "raw_all_HARV.rds"))
# names(harv_raw)
# harv_raw$div_1m2Data
# harv_raw$div_10m2Data100m2Data


# raw_all_<SITE>.rds	Complete original NEON download for each site
# pppc_plantDiv	--> Cleaned species observations across all three sampling scales
# pppc_subplotVisits -->	Surveyed subplot records, including those without qualifying species detections
# pppc_400m_presence -->	Unique detected species per plot and sampling event
# sites_cached	List of successfully processed sites

