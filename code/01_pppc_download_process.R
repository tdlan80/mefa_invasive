# Script: 01_pppc_download_process.R
# Purpose: Download NEON Plant Presence and Percent Cover data (DP1.10058.001)
#          for one or more NEON sites, apply a cleaning pipeline,
#          and save a combined 1 m2 subplot dataset as a local cache.
#
# Output:
#   rawData/NEON/PPPC/<SITE1_SITE2_...>_1m_plantDiv.RData
#   Contains: pppc_1m_plantDiv  (long-format data frame, all sites combined)
#
# Run this script BEFORE 02_neon_aop_pppc.R.
#
# CyVerse / HPC notes:
#   - Set NEON_TOKEN:
#       export NEON_TOKEN="your_token_here"    # bash / zsh
#       $env:NEON_TOKEN = "your_token_here"    # PowerShell
#   - All paths are relative to the repo root (using .Rproj via here()).
#   - To force a fresh download, delete the relevant rawData/NEON/PPPC/ files.

# List of packages
packages <- c("tidyverse", "neonUtilities", "here")

# Install/load packages
lapply(packages, function(x){
  if (require(x, character.only = TRUE) == TRUE){
    
    library(x, character.only = TRUE)
    
  } else {
    
    install.packages(x)
    
  }
})


# ==============================================================================
# Configuration ----------------------------------------------------------------
# ==============================================================================

# NEON TOS site codes to process.
# Add or remove sites here; Script 02 will pick its target from this cache.
#sites <- c("HARV", "SCBI", "GRSM")

# modified: get list of all pppc sites
rs <- httr::GET("https://data.neonscience.org/api/v0/products/DP1.10058.001")
rj <- jsonlite::fromJSON(httr::content(rs, as="text"))
sites <- rj$data$siteCodes$siteCode

# NEON API token. Reads from the NEON_TOKEN environment variable.
## Note: I removed this from being hard coded
neon_token <- Sys.getenv("NEON_TOKEN")

# Plant Presence and Percent Cover (PPPC) data product ID 
pppc_product <- "DP1.10058.001"

# ---- Paths (all relative to repo root) ---------------------------------------

raw_dir  <- here("rawData", "NEON")
pppc_dir <- file.path(raw_dir, "PPPC")

# This is tagging the site name to the directory so we don't have to redownload
# the same site data every time
cache_tag <- paste(sort(sites), collapse = "_")
pppc_path <- file.path(pppc_dir, paste0(cache_tag, "_1m_plantDiv.RData"))


# ==============================================================================
# Helper: download and clean one NEON site -------------------------------------
# ==============================================================================

#' Download DP1.10058.001 for one NEON site and return a cleaned 1 m2 table.
#'
#' A per-site raw cache (raw_<SITE>.RData) is written on first run so that
#' re-running the script skips the API call. Delete this file to force a fresh
#' download.
#'
#' Cleaning pipeline is the same as mefaV2.R (lines 396–456):
#'   - Drop administrative / QC columns not needed downstream
#'   - Remove sampling impractical events (samplingImpractical != "OK")
#'   - Keep only records with plants (targetTaxaPresent == "Y")
#'   - Filter to divDataType = plants (note we may want to include other)
#'   - Strip whitespace from all character fields
#'   - Add binomialName, year, and plot_subplot helper columns
#'
#' @param site       Four-letter NEON site code (e.g. "GRSM").
#' @param product    NEON dpID string.
#' @param token      NEON API token, or "" for unauthenticated access.
#' @param cache_dir  Directory for per-site raw caches.
#' @return Cleaned long-format data frame of 1 m2 subplot observations.
download_and_clean_site <- function(site, product, token, cache_dir) {

  raw_cache <- file.path(cache_dir, paste0("raw_", site, ".RData"))

  if (file.exists(raw_cache)) {
    message("[", site, "] Loading cached raw download: ", raw_cache)
    load(raw_cache)   # restores `div_1m2`
  } else {
    message("[", site, "] Querying NEON API for ", product, " ...")
    neon_dl <- neonUtilities::loadByProduct(
      dpID       = product,
      site       = site,
      package    = "expanded",
      release    = "current",
      tabl       = "div_1m2Data",   # requesting only the 1 m2 table here
      check.size = FALSE,
      include.provisional=TRUE,
      token      = if (nzchar(token)) token else NA_character_
    )
    div_1m2 <- neon_dl$div_1m2Data

    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    save(div_1m2, file = raw_cache)
    message("[", site, "] Raw download cached: ", raw_cache)
  }

  # ---- Cleaning pipeline ---------------------------------------------------

  # Standardise column names
  names(div_1m2) <- str_squish(names(div_1m2))
  
  # create full subplotID identifier
  div_1m2$subplotFullID <- paste(div_1m2$plotID, div_1m2$subplotID, sep=".")
  
  # get set of records where plants were detected but taxonRank is too high
  rankset <- div_1m2[which(div_1m2$targetTaxaPresent=="Y" & 
                              is.na(div_1m2$otherVariablesPresent) & 
                              !div_1m2$taxonRank %in% 
                              c("species", "subspecies", "variety")),]
  # get unique plots in this set
  rankplots <- unique(rankset$subplotFullID)
  
  # create variables for number of records removed and % cover represented
  div_1m2$removedNum <- 0
  div_1m2$removedPercent <- 0
  
  # calculate number removed and % cover represented
  for(j in rankplots) {
    divj <- div_1m2[which(div_1m2$subplotFullID==j),]
    eventsj <- unique(divj$eventID)
    for(k in eventsj) {
      div_1m2$removedNum[which(div_1m2$subplotFullID==j & div_1m2$eventID==k)] <- 
        length(which(rankset$subplotFullID==j & rankset$eventID==k))
      div_1m2$removedPercent[which(div_1m2$subplotFullID==j & div_1m2$eventID==k)] <- 
        sum(rankset$percentCover[which(rankset$subplotFullID==j & rankset$eventID==k)], 
            na.rm=T)
    }
  }

  div_1m2 %>%
    # Drop administrative / provenance columns not needed for analysis
    dplyr::select(-any_of(c(
      "uid", "geodeticDatum", "coordinateUncertainty", "elevation",
      "elevationUncertainty", "taxonIDRemarks", "morphospeciesID",
      "morphospeciesIDRemarks", "identificationReferences",
      "identificationHistoryID", "measuredBy", "recordedBy",
      "samplingImpracticalRemarks", "biophysicalCriteria",
      "publicationDate", "release"
    ))) %>%
    
    # Remove events where field sampling was not completed
    dplyr::filter(is.na(samplingImpractical) | samplingImpractical == "OK") %>%
    
    # Keep only subplot visits where plants were actually detected
    dplyr::filter(targetTaxaPresent == "Y") %>%
    
    # Restrict to identifications at species / subspecies / variety rank
    ## NOTE: we are filtering out otherVariables here (e.g. lichen, rock, etc.)
    ## These can influence the total biotic % cover so we may need to keep
    ## these variables in and standardize relative to the abiotic pc.
    dplyr::filter(taxonRank %in% c("species", "subspecies", "variety")) %>%
    
    # Strip leading / trailing whitespace from all character columns
    dplyr::mutate(across(where(is.character), str_squish)) %>%
    
    # Add helper columns used in downstream scripts
    dplyr::mutate(
      binomialName = word(scientificName, start = 1, end = 2),
      year         = lubridate::year(endDate),
      plot_subplot = paste0(plotID, "_", subplotID)
    ) %>%
    dplyr::relocate(binomialName, .after = scientificName)
}


# ==============================================================================
# Main: build processed data if absent -----------------------------------------
# ==============================================================================

if (file.exists(pppc_path)) {

  message("Processed PPPC cache found — loading:\n  ", pppc_path)
  load(pppc_path)   # restores `pppc_1m_plantDiv`

} else {

  message("No processed cache found. Downloading and processing ",
          length(sites), " site(s): ", paste(sites, collapse = ", "))

  pppc_list <- lapply(
    sites,
    download_and_clean_site,
    product = pppc_product,
    token = neon_token,
    cache_dir = pppc_dir
  )

  pppc_1m_plantDiv <- bind_rows(pppc_list)

  dir.create(pppc_dir, recursive = TRUE, showWarnings = FALSE)
  save(pppc_1m_plantDiv, file = pppc_path)
  message("Processed PPPC data saved to:\n  ", pppc_path)
}


# summarize data removed by subplot and event
# removedNum and removedPercent are always the same value for each subplot and eventID combination
removals <- aggregate(pppc_1m_plantDiv[,c("removedNum","removedPercent")], 
                      by=list(pppc_1m_plantDiv$subplotFullID,
                              pppc_1m_plantDiv$eventID),
                      FUN = mean, na.rm=T)
names(removals)[1:2] <- c("subplotFullID","eventID")

# how many subplot x event combinations have any records removed?
length(which(removals$removedNum>0))
# 9459/89934 = 10.5%

# how many subplot x event combinations have more than 5% of percent cover removed?
length(which(removals$removedPercent>5))
# 1873/89934 = 2%


# ==============================================================================
# Summary ----------------------------------------------------------------------
# ==============================================================================

message("\n--- PPPC dataset summary ---")
message("Sites       : ", paste(sort(unique(pppc_1m_plantDiv$siteID)), collapse = ", "))
message("Years       : ", paste(sort(unique(pppc_1m_plantDiv$year)),   collapse = ", "))
message("Plots       : ", length(unique(pppc_1m_plantDiv$plotID)))
message("Plot/subplot: ", length(unique(pppc_1m_plantDiv$plot_subplot)))
message("Rows        : ", nrow(pppc_1m_plantDiv))
message("nativeStatusCode values: ",
        paste(sort(unique(pppc_1m_plantDiv$nativeStatusCode)), collapse = ", "))
