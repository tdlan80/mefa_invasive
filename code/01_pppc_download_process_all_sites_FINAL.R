# ==============================================================================
# Script 01: Download and organize NEON plant presence and percent cover (PPPC)
# Data product: DP1.10058.001
# ==============================================================================
# PURPOSE
# Download PPPC for the selected NEON terrestrial sites and preserve ALL
# observation rows, including plants identified only to genus/family,
# unidentified plant observations, nonplant cover records, and survey-status
# records. Construct a separate species-level dataset and quantify taxonomic
# exclusions without removing the excluded observations from the full data.
#
# SPATIAL SCALES
#   1 m2:   plant records and species-specific percent cover, plus ground cover
#   10 m2:  additional plant species observations (presence only)
#   100 m2: additional plant species observations (presence only)
#   400 m2: NOT a fourth direct observation table. Script 02 reconstructs
#           plot-level species presence by pooling the nested observations.
#
# SCRIPTS
#   01 (this script): download, separate by scale, retain survey records,
#                     filter to accepted taxonomic ranks, audit exclusions.
#   02 (next script): distinguish no plants vs. no additional species;
#                     assess survey completeness; calculate plot richness.
#
# Raw source files are cached by site as raw_all_<SITE>.rds. They are never
# edited. All derived outputs are rebuilt from the cached raw data when run.
# Delete a raw_all_<SITE>.rds file ONLY when you want a fresh NEON download.
#
# Source: https://data.neonscience.org/data-products/DP1.10058.001
# NEON PPPC user guide: NEON_plantDiv_userGuide (see data product page)
# ==============================================================================

# 1. Packages -----------------------------------------------------------------
# Install any missing packages once using install.packages(), then rerun.
packages <- c("tidyverse", "neonUtilities", "here")
missing_packages <- packages[!vapply(packages, requireNamespace,
                                     logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Install missing packages first: ", paste(missing_packages, collapse = ", "))
}

library(tidyverse)
library(neonUtilities)
library(here)

# 2. Configuration ------------------------------------------------------------
# All 47 NEON terrestrial site codes, including sites with historical data.
sites_all <- c(
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
stopifnot(length(sites_all) == 47, !anyDuplicated(sites_all))

# Run all sites by default. To test a few sites, instead use e.g.:
# sites <- c("HARV", "SCBI", "GRSM")
sites <- sites_all

pppc_product <- "DP1.10058.001"
pppc_release <- "current"       # Updated over time; fixed release for reproducibility
include_provisional <- TRUE      # Provisional records can be revised later

# Never commit an API token in an R script or GitHub repository.
# This reads the token from the environment; setup instructions are below.
# An API token is required if a site needs to be downloaded from NEON.
# neon_token <- Sys.getenv("NEON_TOKEN")

neon_token <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJ0c3VyYXNpbmdoZUBicmlkZ2V3LmVkdSIsInNjb3BlIjoicmF0ZTpwdWJsaWMiLCJpc3MiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnLyIsImV4cCI6MTkxMTY1NDYyMywiaWF0IjoxNzUzOTc0NjIzLCJlbWFpbCI6InRzdXJhc2luZ2hlQGJyaWRnZXcuZWR1In0.ZEonFGxXjd3jY7Azk28c9L_pq_CgKR--LuhLf6ZO2izUO7_eZHIE4XA_zyqeF0gmQ4aSCbTqLfPcL7GnBo3zrg"
neonUtilities::tokenDate(neon_token)



# In RStudio, one temporary option is to run Sys.setenv(NEON_TOKEN = "...")
# interactively in the Console BEFORE sourcing this script. Do not write the
# actual token in a tracked script. For a persistent setup, create ~/.Renviron
# with a line NEON_TOKEN=your_token and restart RStudio.
# Check without displaying your token: nzchar(Sys.getenv("NEON_TOKEN"))

pppc_dir <- here::here("rawData", "NEON", "PPPC")
pppc_path <- file.path(pppc_dir, "pppc_all_terrestrial_all_scales.RData")
dir.create(pppc_dir, recursive = TRUE, showWarnings = TRUE)
if (!dir.exists(pppc_dir) || file.access(pppc_dir, 2) != 0) {
  stop("PPPC directory does not exist or is not writable: ", pppc_dir)
}

# The accepted identification ranks for the ecological analyses are species,
# subspecies, and variety. Every other plant observation is kept in the full
# dataset, but is excluded from pppc_plantDiv and counted in the taxon audit.
accepted_ranks <- c("species", "subspecies", "variety")
coarser_ranks <- c("genus", "subgenus", "family", "subfamily", "tribe",
                   "subtribe", "order", "suborder", "class", "phylum",
                   "division", "kingdom")

# Identify nonempty entries, safely handling NA values.
nonblank <- function(x) !is.na(x) & nzchar(trimws(as.character(x)))

# 3. Download / read and process ONE site -------------------------------------
process_site <- function(site) {

  raw_file <- file.path(pppc_dir, paste0("raw_all_", site, ".rds"))

  if (file.exists(raw_file)) {
    message("[", site, "] Reading cached raw tables")
    neon_dl <- readRDS(raw_file)
  } else {
    if (!nzchar(neon_token)) {
      stop("NEON_TOKEN is empty. Set it in your R session or ~/.Renviron.")
    }
    message("[", site, "] Downloading all original PPPC tables")
    neon_dl <- neonUtilities::loadByProduct(
      dpID = pppc_product,
      site = site,
      package = "expanded",
      release = pppc_release,
      tabl = "all",
      check.size = FALSE,
      include.provisional = include_provisional,
      token = neon_token
    )
    # Preserve ALL tables from loadByProduct(), unmodified.
    saveRDS(neon_dl, raw_file)
  }

  if (!"div_1m2Data" %in% names(neon_dl)) {
    stop("div_1m2Data not available for site ", site)
  }

  # Do not silently classify every plant record as excluded if a required
  # taxonomic / sampling field is missing from a NEON table.
  required_1m <- c("siteID", "plotID", "eventID", "subplotID", "endDate",
                   "targetTaxaPresent", "divDataType", "taxonRank",
                   "scientificName", "percentCover")
  missing_1m <- setdiff(required_1m, names(neon_dl$div_1m2Data))
  if (length(missing_1m) > 0) {
    stop("Missing required 1 m2 fields at ", site, ": ",
         paste(missing_1m, collapse = ", "))
  }

  # Original 1 m2 species and ground-cover records.
  d1 <- neon_dl$div_1m2Data %>%
    mutate(subplotID = as.character(subplotID),
           subplot_area_m2 = 1L,
           source_table = "div_1m2Data")

  # Original 10 and 100 m2 records, stored together in the NEON product.
  # The subplot IDs follow the 2024 convention in recent releases, but we
  # also recognize historical names in already-cached tables.
  d2 <- NULL
  if ("div_10m2Data100m2Data" %in% names(neon_dl)) {
    required_large <- c("siteID", "plotID", "eventID", "subplotID",
                        "endDate", "targetTaxaPresent", "additionalSpecies",
                        "taxonRank", "scientificName")
    missing_large <- setdiff(required_large,
                             names(neon_dl$div_10m2Data100m2Data))
    if (length(missing_large) > 0) {
      stop("Missing required 10/100 m2 fields at ", site, ": ",
           paste(missing_large, collapse = ", "))
    }
    d2 <- neon_dl$div_10m2Data100m2Data %>%
      mutate(
        subplotID = as.character(subplotID),
        subplot_area_m2 = case_when(
          str_detect(subplotID, "^[0-9]+_100$|^[0-9]+$") ~ 100L,
          str_detect(subplotID, "^[0-9]+_10_[0-9]+$|^[0-9]+\\.[0-9]+\\.10$") ~ 10L,
          TRUE ~ NA_integer_
        ),
        source_table = "div_10m2Data100m2Data"
      )
    unexpected_ids <- unique(d2$subplotID[
      !is.na(d2$subplotID) & is.na(d2$subplot_area_m2)
    ])
    if (length(unexpected_ids) > 0) {
      stop("Unknown larger-subplot ID(s) at ", site, ": ",
           paste(head(unexpected_ids, 10), collapse = ", "))
    }
  } else {
    message("[", site, "] Larger-scale observation table not returned")
  }

  # Combine the observation tables WITHOUT restricting taxonomic rank.
  # All rows, all columns and all original observations remain available.
  dat <- bind_rows(d1, d2) %>%
    mutate(across(where(is.character), str_squish))

  # Add missing optional fields so the next steps work across past releases.
  optional <- c("siteID", "plotID", "eventID", "subplotID", "endDate",
                "samplingImpractical", "targetTaxaPresent",
                "additionalSpecies", "divDataType", "taxonID",
                "scientificName", "morphospeciesID", "taxonRank",
                "nativeStatusCode", "percentCover")
  for (nm in optional) {
    if (!nm %in% names(dat)) dat[[nm]] <- NA
  }

  dat <- dat %>%
    mutate(
      year = lubridate::year(as.Date(substr(as.character(endDate), 1, 10))),
      # Keep the reported cover field intact. Convert a copy to numeric for
      # the 1 m2 cover-exclusion audit. Non-numeric/unknown values become NA,
      # so we do not silently treat a trace or missing value as zero cover.
      cover_value = suppressWarnings(as.numeric(percentCover)),
      # Physical plot and plot during a specific sampling event.
      plot_key = paste(siteID, plotID, sep = "_"),
      plot_event_key = paste(siteID, plotID, eventID, sep = "_"),

      # Explicitly unsuccessful subplot sampling is excluded from the derived
      # datasets. NA is retained because this field was added in 2020;
      # historical NA does NOT independently prove a successful visit.
      valid_sampling = is.na(samplingImpractical) |
        samplingImpractical == "OK",

      # Only rows representing PLANTS enter the taxonomic-resolution audit.
      # The 1 m2 table also contains nonplant 'otherVariables' records.
      # At 10/100 m2, 'additionalSpecies = N' can mean no NEW plants were
      # recorded, despite plant presence in the nested subplots. That row
      # alone is not a distinct new plant observation.
      plant_observation = valid_sampling &
        coalesce(targetTaxaPresent == "Y", FALSE) &
        case_when(
          subplot_area_m2 == 1L ~ coalesce(divDataType == "plantSpecies", FALSE),
          subplot_area_m2 %in% c(10L, 100L) ~
            coalesce(additionalSpecies == "Y", FALSE) |
            nonblank(taxonID) | nonblank(scientificName) |
            nonblank(morphospeciesID),
          TRUE ~ FALSE
        ),

      # Filter for our agreed species-level definition. We require a rank
      # of species/subspecies/variety and a usable scientific binomial.
      accepted_rank = tolower(taxonRank) %in% accepted_ranks,
      has_binomial = nonblank(scientificName) &
        str_detect(as.character(scientificName), "^[^[:space:]]+[[:space:]]+[^[:space:]]+"),
      accepted_plant = plant_observation &
        coalesce(accepted_rank, FALSE) & coalesce(has_binomial, FALSE),

      # Explicitly distinguish coarse identifications from other unresolved
      # observations. Both are excluded from species-level analyses, but
      # remain accessible in the full dataset and audit.
      taxon_resolution = case_when(
        !plant_observation ~ "not_a_plant_observation",
        accepted_plant ~ "accepted_species_or_finer",
        tolower(taxonRank) %in% coarser_ranks ~ "coarser_identification",
        TRUE ~ "other_or_missing_identification"
      )
    )

  # FULL SOURCE OBSERVATIONS: includes nonplant rows and unsuccessful attempts.
  all_observations <- dat

  # The analytical subset excludes explicit unsuccessful field sampling,
  # but keeps no-plant survey records for the sampling-effort dataset.
  surveyed <- dat %>% filter(valid_sampling)

  # One record for each observed subplot x event at 1, 10 or 100 m2.
  # Preserve original presence/additional-species codes. Script 02, not 01,
  # will distinguish no plants from no additional plants using these fields.
  subplot_visits <- surveyed %>%
    filter(nonblank(siteID), nonblank(plotID), nonblank(eventID),
           nonblank(subplotID)) %>%
    group_by(siteID, plotID, eventID, year, subplotID, subplot_area_m2) %>%
    summarise(
      n_original_rows = n(),
      observed_target_Y = any(targetTaxaPresent == "Y", na.rm = TRUE),
      observed_target_N = any(targetTaxaPresent == "N", na.rm = TRUE),
      observed_additional_Y = any(additionalSpecies == "Y", na.rm = TRUE),
      observed_additional_N = any(additionalSpecies == "N", na.rm = TRUE),
      n_plant_observations = sum(plant_observation, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(plot_key = paste(siteID, plotID, sep = "_"),
           plot_event_key = paste(siteID, plotID, eventID, sep = "_"))

  # Species-level records for Script 02. Each observation remains a record;
  # no pooling of subplots or sampling events is performed in Script 01.
  plant_div <- surveyed %>%
    filter(accepted_plant, nonblank(siteID), nonblank(plotID),
           nonblank(eventID), nonblank(subplotID)) %>%
    mutate(binomialName = stringr::word(scientificName, 1, 2)) %>%
    relocate(binomialName, .after = scientificName)

  # TAXONOMIC AUDIT: at EACH plot x event x DIRECTLY SAMPLED scale.
  # Denominator = rows representing actual plant observations, NOT ground
  # cover, unsuccessful surveys, or rows stating 'no additional species'.
  # Percent excluded = 100 * excluded plant observations / all plant records.
  # This estimates loss of observation records; it is NOT percentage of
  # species richness lost, and does not depend on the number of subplots.
  taxon_audit <- surveyed %>%
    group_by(siteID, plotID, eventID, year, subplot_area_m2) %>%
    summarise(
      n_plant_observations = sum(plant_observation, na.rm = TRUE),
      n_accepted = sum(accepted_plant, na.rm = TRUE),
      n_excluded_coarse = sum(taxon_resolution == "coarser_identification", na.rm = TRUE),
      n_excluded_other = sum(taxon_resolution == "other_or_missing_identification", na.rm = TRUE),
      # For 1 m2 only: do not interpret the following as percent of ground
      # area. It is a ratio of SUMMED observed plant-cover values.
      n_cover_missing = sum(plant_observation & is.na(cover_value)),
      cover_all = sum(cover_value[plant_observation], na.rm = TRUE),
      cover_excluded = sum(cover_value[
        plant_observation & !accepted_plant
      ], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      n_excluded = n_excluded_coarse + n_excluded_other,
      pct_excluded = if_else(n_plant_observations > 0,
                             100 * n_excluded / n_plant_observations,
                             NA_real_),
      # If any plant cover observations are missing, report NA rather than
      # a potentially misleading cover fraction.
      pct_cover_excluded = if_else(
        subplot_area_m2 == 1L & n_cover_missing == 0L & cover_all > 0,
        100 * cover_excluded / cover_all,
        NA_real_
      )
    )

  # POOLED 400 m2 AUDIT: count all available direct-scale plant observation
  # rows in a plot x event. This is NOT a fourth independently observed scale.
  # Whether all nested subplots were actually surveyed is checked in Script 02.
  taxon_audit_400 <- taxon_audit %>%
    group_by(siteID, plotID, eventID, year) %>%
    summarise(
      n_plant_observations = sum(n_plant_observations),
      n_accepted = sum(n_accepted),
      n_excluded_coarse = sum(n_excluded_coarse),
      n_excluded_other = sum(n_excluded_other),
      n_excluded = sum(n_excluded),
      .groups = "drop"
    ) %>%
    mutate(subplot_area_m2 = 400L,
           pct_excluded = if_else(n_plant_observations > 0,
                                  100 * n_excluded / n_plant_observations,
                                  NA_real_))

  list(all = all_observations, plants = plant_div, visits = subplot_visits,
       audit = taxon_audit, audit400 = taxon_audit_400)
}

# 4. Process every requested site ----------------------------------------------
# If one site fails, the other sites still process. Failed sites are reported.
# Their presence in the 47-code list DOES NOT mean PPPC data exist at every site.
results <- lapply(sites, function(site) {
  tryCatch(process_site(site), error = function(e) {
    message("[", site, "] FAILED: ", conditionMessage(e))
    NULL
  })
})
names(results) <- sites

successful <- !vapply(results, is.null, logical(1))
sites_cached <- sites[successful]
failed_sites <- sites[!successful]
if (length(sites_cached) == 0) stop("No sites were processed successfully")

# 5. Combine data across successfully processed sites ------------------------
pppc_allObservations <- bind_rows(lapply(results[successful], `[[`, "all"))
pppc_plantDiv <- bind_rows(lapply(results[successful], `[[`, "plants"))
pppc_subplotVisits <- bind_rows(lapply(results[successful], `[[`, "visits"))
pppc_taxonAudit <- bind_rows(lapply(results[successful], `[[`, "audit"))
pppc_taxonAudit_400m <- bind_rows(lapply(results[successful], `[[`, "audit400"))

# Extract the original observation rows at each DIRECT sampling scale.
# These are NOT species-filtered and include survey-status and ground cover.
pppc_1m_all <- filter(pppc_allObservations, subplot_area_m2 == 1L)
pppc_10m_all <- filter(pppc_allObservations, subplot_area_m2 == 10L)
pppc_100m_all <- filter(pppc_allObservations, subplot_area_m2 == 100L)

# 6. Validate the observation classification / exclusion arithmetic -----------
# Every counted plant observation must fall into exactly one of the groups:
# (accepted species-level) + (coarser) + (other unresolved).
stopifnot(all(pppc_taxonAudit$n_plant_observations ==
              pppc_taxonAudit$n_accepted + pppc_taxonAudit$n_excluded))

# Inspect the actual taxonomic rank labels and their exclusions.
# This verifies how NEON codes the records before interpretation.
print(pppc_allObservations %>%
        filter(plant_observation) %>%
        count(subplot_area_m2, taxonRank, taxon_resolution, sort = TRUE))

# 7. Save every processed dataset with its intended analytical use ------------
# 01. Complete combined observations, ALL sites and DIRECT sampling scales.
#     Includes accepted and excluded plants, nonplant cover, survey-status
#     rows, and unsuccessful attempts. No taxonomic observations are discarded.
#     Filter siteID and subplot_area_m2 to select any site/scale combination.
saveRDS(pppc_allObservations,
        file.path(pppc_dir, "pppc_allObservations.rds"))

# 02. Complete, unfiltered 1 m2 rows (1 x 1 m).
#     Includes plant species percent cover, coarsely identified plants,
#     ground-cover categories, and no-plant and unsuccessful sampling rows.
saveRDS(pppc_1m_all,
        file.path(pppc_dir, "pppc_1m_all.rds"))

# 03. Complete, unfiltered 10 m2 rows (approximately 3.16 x 3.16 m).
#     Includes additional plant observations of every taxonomic resolution,
#     plus rows that report no additional species or unsuccessful sampling.
saveRDS(pppc_10m_all,
        file.path(pppc_dir, "pppc_10m_all.rds"))

# 04. Complete, unfiltered 100 m2 rows (10 x 10 m).
#     Includes additional plant observations of every taxonomic resolution,
#     plus no-additional-species and unsuccessful sampling rows.
saveRDS(pppc_100m_all,
        file.path(pppc_dir, "pppc_100m_all.rds"))

# 05. Accepted species-level observations pooled across the three DIRECT
#     sampling scales, but still at their original subplot/event resolution.
#     Includes species, subspecies and varieties only; other ranks are retained
#     in the full observations, not here. Used in Script 02 species inventories.
saveRDS(pppc_plantDiv,
        file.path(pppc_dir, "pppc_plantDiv.rds"))

# 06. One row per observed subplot x sampling event.
#     Includes 1, 10 and 100 m2 units, even where no qualifying species was
#     recorded. Preserves targetTaxaPresent and additionalSpecies indicators
#     for interpretation in Script 02 and for sampling-effort denominators.
saveRDS(pppc_subplotVisits,
        file.path(pppc_dir, "pppc_subplotVisits.rds"))

# 07. Taxonomic audit for each plot x event x directly observed scale.
#     Counts all plant observation records, accepted species-level records,
#     coarser identifications and other unresolved records; reports the
#     percentage excluded. For 1 m2, also reports exclusion by summed cover.
saveRDS(pppc_taxonAudit,
        file.path(pppc_dir, "pppc_taxonAudit.rds"))

# 08. Pooled taxonomic audit for each 400 m2 plot x sampling event.
#     Counts plant observation records across whatever nested scales were
#     sampled. NOT yet restricted to complete 400 m2 plot surveys; Script 02
#     will link this to effort/completeness flags before comparisons.
saveRDS(pppc_taxonAudit_400m,
        file.path(pppc_dir, "pppc_taxonAudit_400m.rds"))

# Compatibility file for existing Script 02, which can use load() instead.
# Contains the species-level observations and subplot visits (not the full
# unfiltered observation data), plus the lists of successful/failed sites.
save(pppc_plantDiv, pppc_subplotVisits, sites_cached, failed_sites,
     file = pppc_path)

# Processing manifest: records which sites and release settings were used.
# This is metadata only; it is not a plant-observation dataset.
pppc_manifest <- list(
  product = pppc_product, release = pppc_release,
  include_provisional = include_provisional,
  processed_on = Sys.time(),
  sites_requested = sites, sites_processed = sites_cached,
  failed_sites = failed_sites, accepted_ranks = accepted_ranks
)
saveRDS(pppc_manifest, file.path(pppc_dir, "pppc_manifest.rds"))

# 8. Quality-control summary ---------------------------------------------------
message("\n--- NEON PPPC Script 01 summary ---")
message("Sites requested: ", length(sites), "; succeeded: ", length(sites_cached))
message("All original observation rows: ", nrow(pppc_allObservations))
message("Accepted species-level plant rows: ", nrow(pppc_plantDiv))
message("Observed subplot/event rows: ", nrow(pppc_subplotVisits))
message("Number of excluded plant observations: ", sum(pppc_taxonAudit$n_excluded))
message("Rows with unidentified sampling scale: ",
        sum(is.na(pppc_allObservations$subplot_area_m2)))
if (length(failed_sites)) {
  warning("Processed datasets are incomplete. Failed sites: ",
          paste(failed_sites, collapse = ", "))
}

# Example selections (do not rerun downloads):
# subset_data <- pppc_allObservations %>%
#   filter(siteID %in% c("HARV", "SCBI"), subplot_area_m2 %in% c(1, 100))
# subset_species <- subset_data %>% filter(accepted_plant)
# subset_audit <- pppc_taxonAudit %>%
#   filter(siteID %in% c("HARV", "SCBI"), subplot_area_m2 == 1)
