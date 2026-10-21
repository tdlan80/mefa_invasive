
# ==============================================================================
# Download and clean NEON plant diversity data (1, 10, and 100 m2)
# ==============================================================================

download_and_clean_site_allplots <- function(site, product, token, cache_dir) {
  
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  
  # NEW cache name to avoid loading old 1 m2-only files
  raw_cache <- file.path(cache_dir, paste0("raw_all_", site, ".rds"))
  
  if (file.exists(raw_cache)) {
    
    message("[", site, "] Loading cached raw data")
    neon_dl <- readRDS(raw_cache)
    
  } else {
    
    message("[", site, "] Downloading all tables for ", product)
    
    neon_dl <- neonUtilities::loadByProduct(
      dpID                = product,
      site                = site,
      package             = "expanded",
      release             = "current",
      tabl                = "all",
      check.size          = FALSE,
      include.provisional = TRUE,
      token               = if (nzchar(token)) token else NA_character_
    )
    
    # Save ALL raw tables, not just div_1m2Data
    saveRDS(neon_dl, file = raw_cache)
    
    message("[", site, "] All raw tables cached")
  }
  
  # Check that both vegetation observation tables are available
  required_tables <- c("div_1m2Data", "div_10m2Data100m2Data")
  
  if (!all(required_tables %in% names(neon_dl))) {
    stop("Missing vegetation table(s) for site: ", site)
  }
  
  # ============================================================================
  # Cleaning function applied to BOTH vegetation tables
  # ============================================================================
  
  clean_plant_table <- function(dat, table_name) {
    
    # Standardise column names
    names(dat) <- stringr::str_squish(names(dat))
    
    dat %>%
      
      # Drop administrative / provenance columns
      dplyr::select(-dplyr::any_of(c(
        "uid", "geodeticDatum", "coordinateUncertainty", "elevation",
        "elevationUncertainty", "taxonIDRemarks", "morphospeciesID",
        "morphospeciesIDRemarks", "identificationReferences",
        "identificationHistoryID", "measuredBy", "recordedBy",
        "samplingImpracticalRemarks", "biophysicalCriteria",
        "publicationDate", "release"
      ))) %>%
      
      # Remove events where sampling was not completed
      dplyr::filter(
        is.na(samplingImpractical) | samplingImpractical == "OK"
      ) %>%
      
      # Retain records with plants detected
      dplyr::filter(targetTaxaPresent == "Y") %>%
      
      # Restrict to species and infraspecific identifications
      dplyr::filter(
        taxonRank %in% c("species", "subspecies", "variety")
      ) %>%
      
      # Remove unnecessary whitespace
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.character),
          stringr::str_squish
        )
      ) %>%
      
      # Add helper columns
      dplyr::mutate(
        source_table = table_name,
        binomialName = stringr::word(scientificName, 1, 2),
        year = lubridate::year(endDate),
        plot_subplot = paste0(plotID, "_", subplotID)
      ) %>%
      
      dplyr::relocate(binomialName, .after = scientificName)
  }
  
  # ============================================================================
  # Clean both tables and combine their vegetation observations
  # ============================================================================
  
  div_1m2 <- clean_plant_table(
    neon_dl$div_1m2Data,
    "div_1m2Data"
  )
  
  div_10m2_100m2 <- clean_plant_table(
    neon_dl$div_10m2Data100m2Data,
    "div_10m2Data100m2Data"
  )
  
  # Combine rows while preserving subplot and sampling-event identifiers.
  # Columns absent from one table are filled with NA.
  # The source_table column identifies the original table.
  dplyr::bind_rows(div_1m2, div_10m2_100m2)
}


# ==============================================================================
# Main: build processed data if absent
# ==============================================================================

# NEW filename: prevents loading the previous 1 m2-only processed cache
pppc_path <- file.path(pppc_dir, "pppc_all_scales_plantDiv_allPlots.RData")

if (file.exists(pppc_path)) {
  
  message("Processed PPPC cache found - loading:\n  ", pppc_path)
  load(pppc_path)   # restores pppc_plantDiv
  
} else {
  
  message("Downloading and processing ", length(sites), " site(s)")
  
  pppc_list <- lapply(
    sites,
    download_and_clean_site_allplots,
    product = pppc_product,
    token = neon_token,
    cache_dir = pppc_dir
  )
  
  # Combine cleaned vegetation records across sites
  pppc_plantDiv <- dplyr::bind_rows(pppc_list)
  
  dir.create(pppc_dir, recursive = TRUE, showWarnings = FALSE)
  
  save(pppc_plantDiv, file = pppc_path)
  
  message("Processed PPPC data saved to:\n  ", pppc_path)
}


# ==============================================================================
# Summary
# ==============================================================================

message("\n--- PPPC dataset summary ---")
message("Sites       : ",
        paste(sort(unique(pppc_plantDiv$siteID)), collapse = ", "))
message("Years       : ",
        paste(sort(unique(pppc_plantDiv$year)), collapse = ", "))
message("Plots       : ",
        length(unique(pppc_plantDiv$plotID)))
message("Plot/subplot: ",
        length(unique(pppc_plantDiv$plot_subplot)))
message("Rows        : ", nrow(pppc_plantDiv))
message("nativeStatusCode values: ",
        paste(sort(unique(pppc_plantDiv$nativeStatusCode)),
              collapse = ", "))

# Number of observations from each original table
table(pppc_plantDiv$source_table)


harv_raw <- readRDS(
  file.path(pppc_dir, "raw_all_HARV.rds")
)

names(harv_raw)

harv_raw$div_1m2Data
harv_raw$div_10m2Data100m2Data
harv_raw$div_voucher


pppc_400m_presence <- pppc_plantDiv %>%
  dplyr::distinct(
    siteID, plotID, eventID, binomialName
  )

