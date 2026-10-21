# ==============================================================================
# Script 02: NEON PPPC species inventories, cover, richness, and sampling status
# Data product: DP1.10058.001
# Run AFTER 01_pppc_download_process_all_sites_FINAL.R
# ==============================================================================
# PURPOSE
#   1. Load Script 01 outputs (NO additional NEON downloads).
#   2. Keep individual 1 m2 plant percent-cover records intact.
#   3. Use neonPlants::stackPlantPresence() to reconstruct the plant species
#      inventories within each physically nested 10 and 100 m2 subplot and
#      across the complete 400 m2 (20 x 20 m) plot.
#   4. Keep species identities (long data) separate from richness (summaries).
#   5. Preserve surveyed units with no species-level identifications, distinguish
#      confirmed no-plant observations from unresolved taxa, and flag incomplete
#      sampling rather than treating missing surveys as biological absence.
#   6. Generate species-as-columns wide datasets when requested.
#
# IMPORTANT NEON DETAILS
#   - 1 m2: plant-specific percent cover and presence; also nonplant cover.
#   - 10/100 m2: principally ADDITIONAL species not observed in smaller nested
#     units. A row with targetTaxaPresent='Y' and additionalSpecies='N'
#     means PLANTS EXIST but no new species were reported at that scale.
#   - 400 m2 has NO independent species-observation table; it is reconstructed.
#   - NEON's samplingImpractical flag was introduced in 2020. Before then, an
#     NA in that field is not proof of a successful survey. Completeness based
#     on subplot counts is an EFFORT CHECK, not absolute proof of field quality.
#   - The number of scheduled 1 and 10 m2 subplots changed from 8 to 6 in 2019;
#     four 100 m2 subplots are normally included in a complete 400 m2 survey.
#     Larger scales are typically sampled biennially after 2018.
#   - Taxa at species/subspecies/variety rank are accepted; infraspecific names
#     are pooled to binomial SPECIES for richness. Full scientificName is kept.
#   - A zero accepted-species count does NOT necessarily mean no plants present.
#   - This script does NOT resolve morphospecies or update identifications.
#
# SOURCES
#   https://data.neonscience.org/data-products/DP1.10058.001
#   https://github.com/NEONScience/neonPlants
# ==============================================================================

# 1. Packages and configuration -----------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(neonPlants)

if (!requireNamespace("neonPlants", quietly = TRUE)) {
  stop("Install neonPlants first: remotes::install_github('NEONScience/neonPlants')")
}

pppc_dir <- here::here("rawData", "NEON", "PPPC")

# NULL uses all sites processed successfully in Script 01. Examples:
# sites_to_use <- c("HARV", "SCBI")
sites_to_use <- NULL

# Wide matrices can be large for all 47 sites; change to FALSE when unnecessary.
make_wide <- TRUE

# 2. Load Script 01's processed datasets --------------------------------------
# FULL original observation rows, including nonplant cover, unresolved taxa,
# survey-status records and even unsuccessful sampling attempts.
pppc_allObservations <- readRDS(file.path(pppc_dir, "pppc_allObservations.rds"))

# Species-, subspecies-, and variety-level PLANT observations at their original
# 1/10/100 m2 sampling scales (with full scientificName and binomialName).
pppc_plantDiv <- readRDS(file.path(pppc_dir, "pppc_plantDiv.rds"))

# One row per recorded valid subplot/event; includes surveys without accepted
# species, and targetTaxaPresent/additionalSpecies status summaries.
pppc_subplotVisits <- readRDS(file.path(pppc_dir, "pppc_subplotVisits.rds"))

# Count and percentage of plant observations excluded by taxonomic resolution,
# at the plot-event x direct sampling scale (1, 10, 100 m2).
pppc_taxonAudit <- readRDS(file.path(pppc_dir, "pppc_taxonAudit.rds"))

# Taxonomic exclusion audit pooled across the directly observed scales for each
# plot-event. Completeness is checked HERE, not established in Script 01.
pppc_taxonAudit_400m <- readRDS(file.path(pppc_dir, "pppc_taxonAudit_400m.rds"))

# Manifest: which sites were actually processed in Script 01, which failed,
# and the data-product/release options used. Does not contain plant records.
pppc_manifest <- readRDS(file.path(pppc_dir, "pppc_manifest.rds"))

if (is.null(sites_to_use)) sites_to_use <- pppc_manifest$sites_processed
sites_to_use <- unique(sites_to_use)
if (length(sites_to_use) == 0L) stop("No successfully processed sites selected.")
if (!all(sites_to_use %in% pppc_manifest$sites_processed)) {
  stop("Some requested sites were not processed successfully by Script 01.")
}

# Restrict ALL processed datasets to the same requested site combination.
pppc_allObservations <- filter(pppc_allObservations, siteID %in% sites_to_use)
pppc_plantDiv <- filter(pppc_plantDiv, siteID %in% sites_to_use)
pppc_subplotVisits <- filter(pppc_subplotVisits, siteID %in% sites_to_use)
pppc_taxonAudit <- filter(pppc_taxonAudit, siteID %in% sites_to_use)
pppc_taxonAudit_400m <- filter(pppc_taxonAudit_400m, siteID %in% sites_to_use)

# 'plotID' identifies a physical 400 m2 plot; 'eventID' distinguishes seasonal
# sampling bouts. subplotID belongs ONLY in keys for individual subplots.
plot_keys <- c("siteID", "plotID", "eventID", "year")
subplot_keys <- c(plot_keys, "subplotID")

# 3. Reconstruct species identities with NEON's nesting function --------------
# The neonPlants package is used ONLY for species stacking (the spatial nesting
# relationships). Our sampling flags and taxonomic exclusions are independent.
# The installed package may expose an older/newer function argument name and
# may return either a data frame or a list containing div_stackedPresence.
# Inspect formals/output to accommodate the documented package versions.
stack_one_site <- function(site) {
  raw_file <- file.path(pppc_dir, paste0("raw_all_", site, ".rds"))
  if (!file.exists(raw_file)) stop("Missing original NEON cache: ", raw_file)
  raw <- readRDS(raw_file)
  if (!all(c("div_1m2Data", "div_10m2Data100m2Data") %in% names(raw))) {
    message("[", site, "] No complete pair of source tables; 1 m2 kept, larger scales unavailable")
    return(NULL)
  }
  fn <- neonPlants::stackPlantPresence
  args <- names(formals(fn))
  if ("inputDataList" %in% args) {
    ans <- fn(inputDataList = raw)
  } else if ("divDataList" %in% args) {
    ans <- fn(divDataList = raw)
  } else {
    stop("Unrecognized stackPlantPresence() interface; check package version")
  }
  if (is.list(ans) && !is.data.frame(ans) && "div_stackedPresence" %in% names(ans)) {
    ans <- ans$div_stackedPresence
  }
  if (!is.data.frame(ans) || !"totalSampledArea" %in% names(ans)) {
    stop("Unexpected stackPlantPresence output for ", site)
  }
  ans
}

stack_results <- lapply(sites_to_use, function(s) {
  tryCatch(stack_one_site(s), error = function(e) {
    message("[", s, "] Stacking failed: ", conditionMessage(e))
    NULL
  })
})
names(stack_results) <- sites_to_use
failed_stacks <- names(stack_results)[vapply(stack_results, is.null, logical(1))]
# CRITICAL: do not accidentally label plants as absent if the neonPlants
# aggregation failed. Stop if a site HAS larger-scale observations but the
# stacking step failed; sites with only 1 m2 surveys may proceed normally.
sites_with_larger_visits <- unique(pppc_subplotVisits$siteID[
  pppc_subplotVisits$subplot_area_m2 %in% c(10L, 100L)])
failed_with_larger <- intersect(failed_stacks, sites_with_larger_visits)
if (length(failed_with_larger)) {
  stop("neonPlants stacking failed for sites with 10/100 m2 data: ",
       paste(failed_with_larger, collapse = ", "),
       ". Resolve the package/input issue before building richness datasets.")
}

# Keep successful stacked results. Direct 1 m2 data are taken from Script 01,
# so a missing or failed larger-scale table never deletes 1 m2 observations.
stacked <- bind_rows(stack_results[!vapply(stack_results, is.null, logical(1))])
# Create a zero-row template if no site had larger-scale data.
if (nrow(stacked) == 0L) {
  stacked <- tibble(siteID = character(), plotID = character(),
                    eventID = character(), subplotID = character(),
                    taxonRank = character(), scientificName = character(),
                    taxonID = character(), nativeStatusCode = character(),
                    totalSampledArea = integer(), year = integer(),
                    binomialName = character())
}

# The source code can normalize older eventIDs longer than 11 characters or
# absent eventIDs. Align Script 01 identifiers with those used by neonPlants.
# Preserve the originally reported eventID as eventID_original.
# A site/plot/event/year must map to ONLY one normalized eventID; otherwise
# grouping could combine distinct seasonal sampling bouts and we STOP.
raw_events <- bind_rows(lapply(sites_to_use, function(s) {
  raw <- readRDS(file.path(pppc_dir, paste0("raw_all_", s, ".rds")))
  bind_rows(raw$div_1m2Data, raw$div_10m2Data100m2Data) %>%
    transmute(siteID, plotID, eventID = as.character(eventID),
              year = as.integer(substr(as.character(endDate), 1, 4)),
              boutNumber,
              eventID_stack = if_else(is.na(eventID) | nchar(eventID) > 11,
                                      paste(siteID, boutNumber, year, sep = "."),
                                      eventID))
})) %>%
  distinct(siteID, plotID, eventID, year, eventID_stack)

ambiguous_events <- raw_events %>%
  count(siteID, plotID, eventID, year) %>% filter(n > 1)
if (nrow(ambiguous_events)) {
  stop("An original plot/event maps to multiple normalized events. Review raw boutNumber before aggregating.")
}

normalize_events <- function(x) {
  x %>%
    left_join(raw_events, by = plot_keys) %>%
    mutate(eventID_original = eventID,
           eventID = eventID_stack) %>%
    select(-eventID_stack)
}

pppc_allObservations <- normalize_events(pppc_allObservations)
pppc_plantDiv <- normalize_events(pppc_plantDiv)
pppc_subplotVisits <- normalize_events(pppc_subplotVisits)
pppc_taxonAudit <- normalize_events(pppc_taxonAudit)
pppc_taxonAudit_400m <- normalize_events(pppc_taxonAudit_400m)

if (anyNA(pppc_subplotVisits$eventID)) {
  stop("Some valid subplot visits could not be matched to normalized eventIDs.")
}
# Normalizing historical event IDs must not create duplicate sampling units.
# Otherwise two bouts could be inadvertently merged or counted twice.
if (anyDuplicated(pppc_subplotVisits[c(plot_keys, "subplotID", "subplot_area_m2")])) {
  stop("Normalized event IDs created duplicate subplot/event keys. Inspect sampling bouts.")
}

# Remove coarser identifications and nonplant placeholders from stacked data.
# Keep full scientificName, taxonID, and original taxonRank in long outputs.
if (nrow(stacked) > 0L) {
  required <- c("siteID", "plotID", "eventID", "subplotID", "taxonRank",
                "scientificName", "totalSampledArea")
  if (!all(required %in% names(stacked))) {
    stop("Stacked data missing fields: ",
         paste(setdiff(required, names(stacked)), collapse = ", "))
  }
  # Do not infer year by splitting the event ID string. The source dates
  # supply year via the checked raw-events lookup below.
  stacked <- stacked %>%
    mutate(subplotID = as.character(subplotID),
           totalSampledArea = as.integer(totalSampledArea),
           binomialName = word(scientificName, 1, 2)) %>%
    filter(totalSampledArea %in% c(10L, 100L, 400L),
           tolower(taxonRank) %in% c("species", "subspecies", "variety"),
           !is.na(scientificName), str_detect(scientificName, "\\S+\\s+\\S+"))
  stack_year_map <- raw_events %>%
    select(siteID, plotID, eventID_stack, year) %>% distinct()
  if (anyDuplicated(stack_year_map[c("siteID", "plotID", "eventID_stack")])) {
    stop("Normalized eventID maps to more than one year. Inspect raw dates.")
  }
  stacked <- stacked %>%
    select(-any_of("year")) %>%
    left_join(stack_year_map,
              by = c("siteID", "plotID", "eventID" = "eventID_stack"))
  if (anyNA(stacked$year)) stop("Cannot match stacked events to source years.")
}

# Helpers: concise summary of one species, without losing original name forms.
collapse_values <- function(x) {
  x <- sort(unique(as.character(x[!is.na(x) & nzchar(as.character(x))])))
  if (length(x)) paste(x, collapse = "; ") else NA_character_
}
one_or_na <- function(x) {
  x <- unique(as.character(x[!is.na(x) & nzchar(as.character(x))]))
  if (length(x) == 1L) x else NA_character_
}

# Standardize legacy subplot identifiers in status keys to match neonPlants.
# e.g. 31.1.1 -> 31_1_1; 31.1.10 -> 31_10_1; 31 -> 31_100.
canonical_subplot <- function(x) {
  x <- as.character(x)
  old <- grepl("^[0-9]+\\.[0-9]+\\.(1|10)$", x)
  parts <- strsplit(x[old], "\\.")
  x[old] <- vapply(parts, function(v) paste(v[c(1, 3, 2)], collapse = "_"), "")
  bare <- grepl("^[0-9]+$", x)
  x[bare] <- paste0(x[bare], "_100")
  x
}

# 4. Survey flags / effort -----------------------------------------------------
# Work from VISITS, not just rows with identified species. This preserves
# successful visits that found no target plants or only unresolved plants.
pppc_subplot_status <- pppc_subplotVisits %>%
  mutate(
    no_plants_observed = observed_target_N & !observed_target_Y,
    no_additional_species = subplot_area_m2 %in% c(10L, 100L) &
      observed_target_Y & observed_additional_N & !observed_additional_Y,
    conflicting_target_status = observed_target_Y & observed_target_N,
    unknown_target_status = !observed_target_Y & !observed_target_N,
    # An N for additionalSpecies is NOT an N for targetTaxaPresent.
    # Missing additional species does not mean missing vegetation.
    subplot_key = paste(siteID, plotID, eventID, subplotID, sep = "_")
  )

# Explicit unsuccessful attempts live in allObservations, not the valid-visit
# list. Record how many sampling units had an impractical attempt; do NOT count
# such attempts as completed sampling without a successful visit.
pppc_impractical <- pppc_allObservations %>%
  filter(!is.na(samplingImpractical), samplingImpractical != "OK") %>%
  distinct(siteID, plotID, eventID, year, subplotID, subplot_area_m2)

pppc_plot_effort <- pppc_subplot_status %>%
  group_by(across(all_of(plot_keys))) %>%
  summarise(
    n_1m2 = n_distinct(subplotID[subplot_area_m2 == 1]),
    n_10m2 = n_distinct(subplotID[subplot_area_m2 == 10]),
    n_100m2 = n_distinct(subplotID[subplot_area_m2 == 100]),
    n_unknown_target = sum(unknown_target_status),
    n_conflicting_target = sum(conflicting_target_status),
    .groups = "drop"
  ) %>%
  mutate(
    expected_small = if_else(year < 2019, 8L, 6L),
    complete_1m2 = n_1m2 == expected_small,
    complete_10m2 = n_10m2 == expected_small,
    complete_100m2 = n_100m2 == 4L,
    complete_1plus10 = complete_1m2 & complete_10m2,
    complete_400m2 = complete_1plus10 & complete_100m2,
    # A counts-based completeness flag does NOT prove that every historic
    # survey succeeded. samplingImpractical was introduced in 2020; older
    # missing values must not be interpreted as confirmed successful visits.
    incomplete_sampling = !complete_400m2,
    plot_key = paste(siteID, plotID, sep = "_"),
    plot_event_key = paste(siteID, plotID, eventID, sep = "_")
  )

# Retain any site/plot-event where attempts occurred but no valid visit exists.
impractical_by_plot <- pppc_impractical %>%
  group_by(across(all_of(plot_keys))) %>%
  summarise(n_impractical_subplots = n_distinct(paste(subplot_area_m2, subplotID)),
            .groups = "drop")
pppc_plot_effort <- pppc_plot_effort %>%
  full_join(impractical_by_plot, by = plot_keys) %>%
  mutate(n_impractical_subplots = coalesce(n_impractical_subplots, 0L),
         n_1m2 = coalesce(n_1m2, 0L),
         n_10m2 = coalesce(n_10m2, 0L),
         n_100m2 = coalesce(n_100m2, 0L),
         expected_small = if_else(year < 2019, 8L, 6L),
         complete_1m2 = n_1m2 == expected_small,
         complete_10m2 = n_10m2 == expected_small,
         complete_100m2 = n_100m2 == 4L,
         complete_1plus10 = complete_1m2 & complete_10m2,
         complete_400m2 = complete_1plus10 & complete_100m2,
         incomplete_sampling = !complete_400m2,
         plot_key = paste(siteID, plotID, sep = "_"),
         plot_event_key = paste(siteID, plotID, eventID, sep = "_"))

# A missing event/year identifier could pool different visits or make a
# missing survey appear complete. Do not continue if any effort key is absent.
if (anyNA(pppc_plot_effort[plot_keys])) {
  stop("Plot-effort records contain missing site/plot/event/year keys; review raw data.")
}

# Join Script 01's audited taxonomic-exclusion counts. They are RECORD counts:
# they do not represent lost species richness. Coarse rank and missing/other
# identifications are kept separate. Percent-cover exclusion is available only
# for 1 m2 and is based on summed reported plant cover, not ground area.
pppc_plot_effort <- pppc_plot_effort %>%
  left_join(pppc_taxonAudit_400m %>%
              select(all_of(plot_keys), n_plant_observations, n_accepted,
                     n_excluded_coarse, n_excluded_other, n_excluded,
                     pct_excluded),
            by = plot_keys)

# 5. Species-identity LONG datasets -------------------------------------------
# 1 m2 cover: one ORIGINAL accepted plant observation per row, no cover sums.
pppc_1m_cover_long <- pppc_plantDiv %>%
  filter(subplot_area_m2 == 1L) %>%
  mutate(subplotID_original = subplotID,
         subplotID = canonical_subplot(subplotID),
         plot_key = paste(siteID, plotID, sep = "_"),
         plot_event_key = paste(siteID, plotID, eventID, sep = "_"))

# Convert individual 1 m2 observations to species-presence records. A binomial
# may occur more than once if, for example, species and variety records map to
# the SAME species name. This is not automatically an error or independent
# cover: we retain the full names for inspection and count that species once.
pppc_1m_species_detected <- pppc_1m_cover_long %>%
  group_by(across(all_of(subplot_keys)), binomialName) %>%
  summarise(scientificNames_all = collapse_values(scientificName),
            scientificName = one_or_na(scientificName),
            taxonIDs_all = collapse_values(taxonID),
            nativeStatusCode = one_or_na(nativeStatusCode),
            n_cover_records = n(),
            .groups = "drop") %>%
  mutate(presence = 1L)

# Build source-scale provenance to show which 400 m2 species were first recorded
# in 1 m2, added at 10 m2, or added at 100 m2. This is observational addition,
# NOT proof that a species is biologically absent from smaller nested areas.
source_scale <- pppc_plantDiv %>%
  group_by(across(all_of(plot_keys)), binomialName) %>%
  summarise(first_recorded_m2 = min(subplot_area_m2),
            recorded_in_1m2 = any(subplot_area_m2 == 1),
            recorded_in_10m2 = any(subplot_area_m2 == 10),
            recorded_in_100m2 = any(subplot_area_m2 == 100),
            .groups = "drop")

# The stacking function creates a cumulative species list for every 10/100 m2
# subplot and 400 m2 plot; it DOES NOT produce percent-cover estimates there.
# Deduplicate by binomial within each reconstructed sampling unit.
pppc_stacked_species <- stacked %>%
  group_by(siteID, plotID, eventID, year, subplotID, totalSampledArea,
           binomialName) %>%
  summarise(scientificNames_all = collapse_values(scientificName),
            scientificName = one_or_na(scientificName),
            taxonIDs_all = if ("taxonID" %in% names(stacked))
              collapse_values(taxonID) else NA_character_,
            nativeStatusCode = if ("nativeStatusCode" %in% names(stacked))
              one_or_na(nativeStatusCode) else NA_character_,
            .groups = "drop") %>%
  rename(subplot_area_m2 = totalSampledArea) %>%
  mutate(presence = 1L)

pppc_subplot_status <- pppc_subplot_status %>%
  mutate(subplotID = canonical_subplot(subplotID))

# Make 1 m2 species labels match canonical IDs (including legacy NEON names).
pppc_1m_species_detected <- pppc_1m_species_detected %>%
  mutate(subplotID = canonical_subplot(subplotID))

# Exact scale-specific key for joins: a 400 m2 plot has subplotID = '400'.
# Use the observed visit list as the backbone for 1/10/100 m2 outputs.
visit_units <- pppc_subplot_status %>%
  mutate(sampling_scale_m2 = subplot_area_m2) %>%
  select(all_of(subplot_keys), sampling_scale_m2,
         no_plants_observed, no_additional_species,
         conflicting_target_status, unknown_target_status)

# The 1 m2 list includes a placeholder row where no accepted species was found;
# binomialName = NA on these rows, NOT a species named NA. This allows a full
# accounting of surveyed units, including zeros and unresolved identifications.
pppc_1m_species_long <- visit_units %>%
  filter(sampling_scale_m2 == 1L) %>%
  left_join(pppc_1m_species_detected,
            by = subplot_keys) %>%
  mutate(no_accepted_species = is.na(binomialName),
         plot_key = paste(siteID, plotID, sep = "_"),
         plot_event_key = paste(siteID, plotID, eventID, sep = "_"))

# 10 and 100 m2: complete reconstructed nested species inventories from
# neonPlants, including the 1 m2 species located within EACH larger subplot.
# The status columns originate from DIRECT observations of those larger units.
large_species_long <- visit_units %>%
  filter(sampling_scale_m2 %in% c(10L, 100L)) %>%
  left_join(pppc_stacked_species %>% filter(subplot_area_m2 %in% c(10, 100)),
            by = c(subplot_keys, "sampling_scale_m2" = "subplot_area_m2")) %>%
  mutate(no_accepted_species = is.na(binomialName),
         plot_key = paste(siteID, plotID, sep = "_"),
         plot_event_key = paste(siteID, plotID, eventID, sep = "_"))
pppc_10m_species_long <- filter(large_species_long, sampling_scale_m2 == 10L)
pppc_100m_species_long <- filter(large_species_long, sampling_scale_m2 == 100L)

# Identify which species FIRST appeared in each nested 10 or 100 m2 unit.
# Only source observation rows establish a new detection: inherited species
# from smaller subplots must not be incorrectly labeled as newly added.
direct_parents <- pppc_plantDiv %>%
  mutate(direct_m2 = subplot_area_m2,
         id = canonical_subplot(subplotID),
         id_10m = case_when(
           direct_m2 == 1L ~ str_replace(id, "_1_", "_10_"),
           direct_m2 == 10L ~ id,
           TRUE ~ NA_character_),
         id_100m = case_when(
           direct_m2 == 100L ~ id,
           direct_m2 %in% c(1L, 10L) ~
             str_replace(id, "_(1|10)_[0-9]+$", "_100"),
           TRUE ~ NA_character_))

first_in_10m <- direct_parents %>%
  filter(!is.na(id_10m)) %>%
  group_by(across(all_of(plot_keys)), id_10m, binomialName) %>%
  summarise(first_recorded_m2 = min(direct_m2), .groups = "drop") %>%
  rename(subplotID = id_10m)
first_in_100m <- direct_parents %>%
  filter(!is.na(id_100m)) %>%
  group_by(across(all_of(plot_keys)), id_100m, binomialName) %>%
  summarise(first_recorded_m2 = min(direct_m2), .groups = "drop") %>%
  rename(subplotID = id_100m)

pppc_10m_species_long <- pppc_10m_species_long %>%
  left_join(first_in_10m, by = c(subplot_keys, "binomialName")) %>%
  mutate(added_beyond_1m2 = first_recorded_m2 == 10L)
pppc_100m_species_long <- pppc_100m_species_long %>%
  left_join(first_in_100m, by = c(subplot_keys, "binomialName")) %>%
  mutate(added_beyond_1m2 = first_recorded_m2 > 1L,
         added_at_100m2 = first_recorded_m2 == 100L)

# Plot-level 400 m2 inventory: NEON specifies that the species list is the
# union of all DIRECT 1/10/100 m2 detections with the same plotID and eventID.
# This correctly preserves PARTIAL plot inventories in years with only 1 m2
# surveys; later we use neonPlants' independently stacked output as a QC check.
plot_species_names <- pppc_plantDiv %>%
  group_by(across(all_of(plot_keys)), binomialName) %>%
  summarise(scientificNames_all = collapse_values(scientificName),
            scientificName = one_or_na(scientificName),
            taxonIDs_all = collapse_values(taxonID),
            nativeStatusCode = one_or_na(nativeStatusCode),
            .groups = "drop")

pppc_400m_species_long <- pppc_plot_effort %>%
  select(all_of(plot_keys), complete_400m2, incomplete_sampling) %>%
  left_join(plot_species_names, by = plot_keys) %>%
  left_join(source_scale, by = c(plot_keys, "binomialName")) %>%
  mutate(
    presence = if_else(!is.na(binomialName), 1L, NA_integer_),
    no_accepted_species = is.na(binomialName),
    added_beyond_1m2 = if_else(!is.na(first_recorded_m2),
                               first_recorded_m2 > 1L, NA),
    added_at_10m2 = first_recorded_m2 == 10L,
    added_at_100m2 = first_recorded_m2 == 100L,
    plot_key = paste(siteID, plotID, sep = "_"),
    plot_event_key = paste(siteID, plotID, eventID, sep = "_")
  )

# Taxonomic exclusion reason for zero accepted species:
#   'explicit_no_plants' = NEON explicitly says no target plants (1/10/100 m2).
#   'coarser_or_unresolved_only' = plant observations exist but no species-rank
#                                  taxon is usable under our agreed filters.
#   'no_accepted_species_reason_unknown' = neither conclusion is demonstrated.
# At a 400 m2 plot, there is NO direct targetTaxaPresent record: all statuses
# are inferred from nested survey records, not invented.
subplot_taxon <- pppc_allObservations %>%
  filter(valid_sampling, subplot_area_m2 %in% c(1L, 10L, 100L)) %>%
  mutate(subplotID = canonical_subplot(subplotID)) %>%
  group_by(across(all_of(subplot_keys)), subplot_area_m2) %>%
  summarise(n_plant_records = sum(plant_observation, na.rm = TRUE),
            n_accepted_records = sum(accepted_plant, na.rm = TRUE),
            .groups = "drop") %>%
  rename(sampling_scale_m2 = subplot_area_m2)

add_zero_reason <- function(x) {
  x %>%
    left_join(subplot_taxon, by = c(subplot_keys, "sampling_scale_m2")) %>%
    mutate(zero_detection_reason = case_when(
      !no_accepted_species ~ NA_character_,
      no_plants_observed & !conflicting_target_status ~ "explicit_no_plants",
      coalesce(n_plant_records, 0L) > 0L ~ "coarser_or_unresolved_only",
      no_additional_species ~ "no_additional_species_reported",
      TRUE ~ "reason_unknown"
    ))
}
pppc_1m_species_long <- add_zero_reason(pppc_1m_species_long)
pppc_10m_species_long <- add_zero_reason(pppc_10m_species_long)
pppc_100m_species_long <- add_zero_reason(pppc_100m_species_long)

# At 400 m2, there is NO direct targetTaxaPresent row. Explicit no-plant
# status must be supported by ALL nested survey units; otherwise never infer
# ecological absence merely because accepted species records are empty.
plot_no_plant_status <- pppc_subplot_status %>%
  group_by(across(all_of(plot_keys))) %>%
  summarise(all_units_explicit_no_plants = all(no_plants_observed) &
              !any(conflicting_target_status) & !any(unknown_target_status),
            .groups = "drop")

pppc_400m_species_long <- pppc_400m_species_long %>%
  left_join(pppc_plot_effort %>%
              select(all_of(plot_keys), n_plant_observations), by = plot_keys) %>%
  left_join(plot_no_plant_status, by = plot_keys) %>%
  mutate(zero_detection_reason = case_when(
    !no_accepted_species ~ NA_character_,
    !complete_400m2 ~ "incomplete_sampling",
    coalesce(n_plant_observations, 0L) > 0L ~
      "plants_present_but_no_accepted_species",
    coalesce(all_units_explicit_no_plants, FALSE) ~
      "all_nested_units_explicitly_no_plants",
    TRUE ~ "no_accepted_species_reason_unknown"
  ))

# 6. Separate RICHNESS summaries (no repeated species rows) ------------------
# A completed 1 m2 visit with zero qualifying taxa receives richness = 0.
# This is zero IDENTIFIED species richness, not necessarily zero plant cover.
pppc_1m_richness <- pppc_1m_species_long %>%
  group_by(across(all_of(subplot_keys))) %>%
  summarise(S_1m2 = n_distinct(binomialName, na.rm = TRUE),
            no_accepted_species = all(no_accepted_species),
            no_plants_observed = first(no_plants_observed),
            unknown_target_status = first(unknown_target_status),
            conflicting_target_status = first(conflicting_target_status),
            zero_detection_reason = first(zero_detection_reason),
            .groups = "drop") %>%
  mutate(complete_1m_survey = !unknown_target_status &
           !conflicting_target_status)

# Nested 10/100 m2 effort checks. Each 10 m2 subplot contains ONE 1 m2
# subplot with the same corner identifier (e.g. 32_1_2 -> 32_10_2).
# Each 100 m2 subplot contains up to TWO pairs of nested 1 and 10 m2 units.
# The central subplots 31_..._4 and 41_..._1 were discontinued in 2019,
# leaving ONE pair in 31 and 41; 32 and 40 continue with TWO pairs.
# This is an independent effort check and complements neonPlants stacking.
small_parents <- visit_units %>%
  filter(sampling_scale_m2 == 1L) %>%
  mutate(parent10 = str_replace(subplotID, "_1_", "_10_"),
         parent100 = str_replace(subplotID, "_1_[0-9]+$", "_100"))
large_parents <- visit_units %>%
  filter(sampling_scale_m2 == 10L) %>%
  mutate(parent100 = str_replace(subplotID, "_10_[0-9]+$", "_100"))

n_1_under10 <- small_parents %>%
  group_by(across(all_of(plot_keys)), parent10) %>%
  summarise(n_nested_1m = n_distinct(subplotID), .groups = "drop") %>%
  rename(subplotID = parent10)

n_1_under100 <- small_parents %>%
  group_by(across(all_of(plot_keys)), parent100) %>%
  summarise(n_nested_1m = n_distinct(subplotID), .groups = "drop") %>%
  rename(subplotID = parent100)

n_10_under100 <- large_parents %>%
  group_by(across(all_of(plot_keys)), parent100) %>%
  summarise(n_nested_10m = n_distinct(subplotID), .groups = "drop") %>%
  rename(subplotID = parent100)

# One row per reconstructed individual 10 or 100 m2 subplot/event.
# observed_S_* counts detected species in that unit, even if its NESTED
# sampling was incomplete. S_* is NA when necessary subplots are missing.
pppc_10m_richness <- pppc_10m_species_long %>%
  group_by(across(all_of(subplot_keys))) %>%
  summarise(observed_S_10m2 = n_distinct(binomialName, na.rm = TRUE),
            no_accepted_species = all(no_accepted_species),
            .groups = "drop") %>%
  left_join(n_1_under10, by = subplot_keys) %>%
  mutate(n_nested_1m = coalesce(n_nested_1m, 0L),
         complete_nested = n_nested_1m == 1L,
         S_10m2 = if_else(complete_nested, observed_S_10m2, NA_integer_))

pppc_100m_richness <- pppc_100m_species_long %>%
  group_by(across(all_of(subplot_keys))) %>%
  summarise(observed_S_100m2 = n_distinct(binomialName, na.rm = TRUE),
            no_accepted_species = all(no_accepted_species),
            .groups = "drop") %>%
  left_join(n_1_under100, by = subplot_keys) %>%
  left_join(n_10_under100, by = subplot_keys) %>%
  mutate(n_nested_1m = coalesce(n_nested_1m, 0L),
         n_nested_10m = coalesce(n_nested_10m, 0L),
         subplot_corner = str_extract(subplotID, "^[0-9]+"),
         expected_nested = case_when(
           year < 2019L & subplot_corner %in% c("31", "32", "40", "41") ~ 2L,
           year >= 2019L & subplot_corner %in% c("31", "41") ~ 1L,
           year >= 2019L & subplot_corner %in% c("32", "40") ~ 2L,
           TRUE ~ NA_integer_),
         complete_nested = !is.na(expected_nested) &
           n_nested_1m == expected_nested &
           n_nested_10m == expected_nested,
         S_100m2 = if_else(complete_nested, observed_S_100m2, NA_integer_))

# Copy effort/completeness flags back onto the species-identity long files.
# This allows downstream code to filter incomplete nested inventories WITHOUT
# losing the original detected species or the reason for a zero record.
pppc_1m_species_long <- pppc_1m_species_long %>%
  left_join(pppc_1m_richness %>%
              select(all_of(subplot_keys), complete_1m_survey),
            by = subplot_keys)

pppc_10m_species_long <- pppc_10m_species_long %>%
  left_join(pppc_10m_richness %>%
              select(all_of(subplot_keys), complete_nested),
            by = subplot_keys)

pppc_100m_species_long <- pppc_100m_species_long %>%
  left_join(pppc_100m_richness %>%
              select(all_of(subplot_keys), complete_nested),
            by = subplot_keys)

# Observed plot/event richness from DIRECT species detections. These provide
# the four requested metrics, avoiding double-counting a binomial across
# subplots and avoiding reliance on 10/100 m2 geometry for pooled 400 m2 values.
obs_richness <- source_scale %>%
  group_by(across(all_of(plot_keys))) %>%
  summarise(
    observed_S_1m2_pooled = sum(recorded_in_1m2),
    observed_S_1plus10 = sum(recorded_in_1m2 | recorded_in_10m2),
    observed_S_all_scales = n(),
    n_added_10m2 = sum(first_recorded_m2 == 10L),
    n_added_100m2 = sum(first_recorded_m2 == 100L),
    .groups = "drop"
  )

mean_1m <- pppc_1m_richness %>%
  group_by(across(all_of(plot_keys))) %>%
  summarise(observed_mean_S_1m2 = mean(S_1m2),
            n_1m2_used = n(), .groups = "drop")

# 1 m2 taxonomic-exclusion audit, aggregated over its 6 or 8 subplot visits.
# These counts are essential for assessing how much 1 m2 plant information was
# excluded at a plot-event. The cover-based fraction reflects summed plant
# cover, NOT percent ground area or percent of species richness lost.
audit_1m_plot <- pppc_taxonAudit %>%
  filter(subplot_area_m2 == 1L) %>%
  transmute(across(all_of(plot_keys)),
            n_plant_observations_1m = n_plant_observations,
            n_accepted_1m = n_accepted,
            n_excluded_1m = n_excluded,
            n_excluded_coarse_1m = n_excluded_coarse,
            n_excluded_other_1m = n_excluded_other,
            pct_excluded_1m = pct_excluded,
            pct_cover_excluded_1m = pct_cover_excluded)

# DATASET: pppc_plot_richness -- one row per physical 400 m2 plot/event.
# Main measures:
#   mean_S_1m2  = mean richness of the 6 or 8 individual 1 m2 subplots;
#                 NOT total 400 m2 richness or pooled richness.
#   S_1m2_pooled= unique species across all 1 m2 subplots in the plot/event.
#   S_1plus10   = unique species from combined 1 and 10 m2 observations,
#                 NOT richness of one 10 m2 subplot.
#   S_400m2     = unique species from all 1 + 10 + 100 m2 observations,
#                 the cumulative 400 m2 inventory for a COMPLETE survey.
#   n_added_*   = species first recorded at that larger direct sampling scale.
# Main S_* fields are NA if the required survey scale(s) were incomplete.
# observed_* fields retain partial information for diagnostics.
pppc_plot_richness <- pppc_plot_effort %>%
  left_join(mean_1m, by = plot_keys) %>%
  left_join(obs_richness, by = plot_keys) %>%
  left_join(audit_1m_plot, by = plot_keys) %>%
  mutate(
    across(starts_with("observed_S_"), ~ coalesce(.x, 0L)),
    mean_S_1m2 = if_else(complete_1m2, observed_mean_S_1m2, NA_real_),
    S_1m2_pooled = if_else(complete_1m2, observed_S_1m2_pooled, NA_integer_),
    S_1plus10 = if_else(complete_1plus10, observed_S_1plus10, NA_integer_),
    S_400m2 = if_else(complete_400m2, observed_S_all_scales, NA_integer_)
  )

# Compare NEON's stacked 400 m2 inventories to the direct-scale union. These
# SHOULD agree for plot-events where the full set of nested scales was sampled.
# Discrepancies are saved for inspection; do not silently replace one dataset.
stacked_400_count <- pppc_stacked_species %>%
  filter(subplot_area_m2 == 400L) %>%
  group_by(across(all_of(plot_keys))) %>%
  summarise(S_stacked_400m2 = n_distinct(binomialName, na.rm = TRUE), .groups = "drop")

pppc_stacking_check <- pppc_plot_richness %>%
  left_join(stacked_400_count, by = plot_keys) %>%
  mutate(stack_mismatch = complete_400m2 &
           (is.na(S_stacked_400m2) | S_400m2 != S_stacked_400m2))

# Richness in LONG layout (one row per plot/event x metric) for ggplot2 or
# mixed models. Not species identities: the species identity files are above.
pppc_richness_long <- pppc_plot_richness %>%
  select(all_of(plot_keys), plot_key, plot_event_key,
         mean_S_1m2, S_1m2_pooled, S_1plus10, S_400m2) %>%
  pivot_longer(cols = c(mean_S_1m2, S_1m2_pooled, S_1plus10, S_400m2),
               names_to = "richness_metric", values_to = "richness")

# Species long files must contain at most one row per binomial x sampling unit
# (except the single NA-binomial placeholder for a zero-detection unit).
stopifnot(
  !anyDuplicated(pppc_1m_species_long[c(subplot_keys, "binomialName")]),
  !anyDuplicated(pppc_10m_species_long[c(subplot_keys, "binomialName")]),
  !anyDuplicated(pppc_100m_species_long[c(subplot_keys, "binomialName")]),
  !anyDuplicated(pppc_400m_species_long[c(plot_keys, "binomialName")])
)

# 7. Optional WIDE matrices ----------------------------------------------------
# One row per sampled unit; each binomial becomes a species column.
# For presence: 1=identified, 0=not detected within the sampled unit.
# WARNING: a zero is nondetection, NOT confirmed ecological absence.
# For 1 m2 cover: percentCover is retained, but multiple ORIGINAL rows can
# collapse to one binomial (e.g., both species and variety observations).
# Do NOT sum or average such values without resolving the taxonomic overlap.
pppc_1m_cover_conflicts <- pppc_1m_cover_long %>%
  group_by(across(all_of(subplot_keys)), binomialName) %>%
  summarise(n_records = n(),
            n_distinct_cover = n_distinct(percentCover, na.rm = TRUE),
            scientificNames_all = collapse_values(scientificName),
            reported_cover_values = collapse_values(percentCover),
            .groups = "drop") %>%
  filter(n_records > 1L)

if (make_wide) {
  # Keep a separate cover value only when exactly one raw accepted observation
  # maps to a binomial. Multiple records --> NA, not a false cover sum.
  one_cover <- pppc_1m_cover_long %>%
    group_by(across(all_of(subplot_keys)), binomialName) %>%
    summarise(cover = if (n() == 1L) suppressWarnings(as.numeric(percentCover))
              else NA_real_, .groups = "drop")

  pppc_1m_cover_wide <- pppc_1m_richness %>%
    select(all_of(subplot_keys), S_1m2, zero_detection_reason) %>%
    left_join(one_cover %>%
                pivot_wider(names_from = binomialName,
                            values_from = cover, values_fill = 0),
              by = subplot_keys)
  # Cover is not 0 when plant taxa were observed but had missing cover;
  # such entries remain NA. Fill only ABSENT species with 0 for confirmed
  # surveyed 1 m2 units, leaving ambiguous survey statuses as NA.
  cover_species_cols <- setdiff(names(pppc_1m_cover_wide),
                                c(subplot_keys, "S_1m2", "zero_detection_reason"))
  if (length(cover_species_cols)) {
    pppc_1m_cover_wide <- pppc_1m_cover_wide %>%
      left_join(pppc_1m_richness %>%
                  select(all_of(subplot_keys), complete_1m_survey),
                by = subplot_keys) %>%
      mutate(across(all_of(cover_species_cols),
                    ~ if_else(complete_1m_survey & is.na(.x) & S_1m2 == 0L,
                              0, .x)))
  }

  # For wide matrices, 0 means nondetection only when the relevant sampling
  # was completed; missing species cells become NA in incomplete units.
  # Detected species remain 1 even when only a partial survey was available.
  presence_wide <- function(species_long, unit_keys, richness_dat,
                            completeness_col) {
    mat <- species_long %>%
      filter(!is.na(binomialName)) %>%
      distinct(across(all_of(c(unit_keys, "binomialName")))) %>%
      mutate(presence = 1L) %>%
      pivot_wider(names_from = binomialName, values_from = presence,
                  values_fill = 0L)
    species_columns <- setdiff(names(mat), unit_keys)
    result <- left_join(richness_dat, mat, by = unit_keys)
    if (length(species_columns)) {
      result <- result %>%
        mutate(across(all_of(species_columns),
                      ~ if_else(!.data[[completeness_col]] & is.na(.x),
                                NA_integer_, coalesce(.x, 0L))))
    }
    result
  }

  pppc_1m_presence_wide <- presence_wide(
    pppc_1m_species_long, subplot_keys, pppc_1m_richness, "complete_1m_survey")
  pppc_10m_presence_wide <- presence_wide(
    pppc_10m_species_long, subplot_keys, pppc_10m_richness, "complete_nested")
  pppc_100m_presence_wide <- presence_wide(
    pppc_100m_species_long, subplot_keys, pppc_100m_richness, "complete_nested")
  pppc_400m_presence_wide <- presence_wide(
    pppc_400m_species_long, plot_keys, pppc_plot_richness, "complete_400m2")
}

# 8. Quality control -----------------------------------------------------------
cat("\n--- Script 02 summary ---\n")
cat("Sites selected:", length(sites_to_use), "\n")
cat("Sites with unavailable/failed larger-scale stack:",
    paste(failed_stacks, collapse = ", "), "\n")
cat("1 m2 accepted cover observations:", nrow(pppc_1m_cover_long), "\n")
cat("400 m2 species rows (including zero placeholders):",
    nrow(pppc_400m_species_long), "\n")
cat("400 m2 complete plot-events:",
    sum(pppc_plot_effort$complete_400m2, na.rm = TRUE), "\n")
cat("400 m2 stacking mismatches:",
    sum(pppc_stacking_check$stack_mismatch, na.rm = TRUE), "\n")
cat("1 m2 binomial cover collisions:", nrow(pppc_1m_cover_conflicts), "\n")

# 9. Save each output with a clear description --------------------------------
# Saving individually with saveRDS() allows Script 03 to read only the needed
# objects. Files are DERIVED products; original per-site raw_all_SITE.rds
# archives from Script 01 are never changed by this script.

# pppc_subplot_status.rds:
#   One row per successfully recorded direct subplot-event, scales 1/10/100 m2.
#   Flags explicitly reported no plants, no ADDITIONAL species, conflicting or
#   missing target statuses. Essential for interpreting empty species records.
saveRDS(pppc_subplot_status, file.path(pppc_dir, "pppc_subplot_status.rds"))

# pppc_plot_effort.rds:
#   One row per 400 m2 plot-event. Counts surveyed subplots at every direct
#   scale; records counts-based completeness flags, impractical attempts,
#   and pooled taxonomic-exclusion percentages from Script 01.
saveRDS(pppc_plot_effort, file.path(pppc_dir, "pppc_plot_effort.rds"))

# pppc_1m_cover_long.rds:
#   Original ACCEPTED species-level plant observations per 1 m2 subplot-event;
#   contains full scientificName, binomialName, taxonID and percentCover.
#   Multiple raw observations are NOT pooled, averaged or summed.
saveRDS(pppc_1m_cover_long, file.path(pppc_dir, "pppc_1m_cover_long.rds"))

# pppc_1m_species_long.rds:
#   Long presence inventory per 1 m2 subplot-event, one row per unique binomial.
#   Keeps surveyed zero-species units as NA-binomial placeholders with flags,
#   and retains full name variants and reported native status where available.
saveRDS(pppc_1m_species_long, file.path(pppc_dir, "pppc_1m_species_long.rds"))

# pppc_10m_species_long.rds:
#   Reconstructed species identity in each individual 10 m2 subplot-event.
#   Combines 10 m2 observations with the 1 m2 subplot PHYSICALLY nested inside.
#   Tracks species added beyond 1 m2; includes nested-sampling completeness,
#   zero-detection placeholders, and direct survey-status flags.
saveRDS(pppc_10m_species_long, file.path(pppc_dir, "pppc_10m_species_long.rds"))

# pppc_100m_species_long.rds:
#   Reconstructed species identity in each individual 100 m2 subplot-event.
#   Includes records from that subplot plus its correct nested 1/10 m2 units;
#   identifies added species and includes completeness/zero-detection flags.
saveRDS(pppc_100m_species_long, file.path(pppc_dir, "pppc_100m_species_long.rds"))

# pppc_400m_species_long.rds:
#   One species-presence record per unique binomial x 400 m2 plot x event.
#   Pools species reported at any direct scale of the 400 m2 plot/event;
#   neonPlants reconstructs the nesting and provides a separate QC comparison.
#   Includes first-scale-of-detection and additional-species flags.
#   Contains a flagged NA-binomial placeholder when no species qualified.
saveRDS(pppc_400m_species_long, file.path(pppc_dir, "pppc_400m_species_long.rds"))

# pppc_1m_richness.rds:
#   One row per visited 1 m2 subplot-event; distinct binomial species richness.
#   Includes zero richness for surveyed units without qualifying identified
#   species, with a separate reason flag (not necessarily vegetation absence).
saveRDS(pppc_1m_richness, file.path(pppc_dir, "pppc_1m_richness.rds"))

# pppc_10m_richness.rds and pppc_100m_richness.rds:
#   One row per visited larger subplot-event. observed_S_* includes detected
#   species from all physically nested subplots; S_* is NA when any required
#   nested smaller subplot is missing. Includes nested-effort counts and flags.
#   Richness is NOT just the additional species observed at that scale.
saveRDS(pppc_10m_richness, file.path(pppc_dir, "pppc_10m_richness.rds"))
saveRDS(pppc_100m_richness, file.path(pppc_dir, "pppc_100m_richness.rds"))

# pppc_plot_richness.rds:
#   ONE ROW per 400 m2 plot-event. Includes mean 1 m2 subplot richness,
#   pooled richness across 1 m2 units, cumulative 1+10 m2 richness, cumulative
#   all-scales 400 m2 richness, newly recorded species at 10 and 100 m2,
#   observed partial-count diagnostics and effort, overall taxonomic audit,
#   PLUS 1 m2 exclusions by observation count and by summed plant cover.
#   NA in a main S_* field denotes insufficient sampling, NOT zero plants.
saveRDS(pppc_plot_richness, file.path(pppc_dir, "pppc_plot_richness.rds"))

# pppc_richness_long.rds:
#   The four main plot-level richness metrics in long/tidy layout, one row
#   per plot-event x metric; convenient for ggplot2 and comparative models.
#   Species identities are NOT contained here (see *_species_long datasets).
saveRDS(pppc_richness_long, file.path(pppc_dir, "pppc_richness_long.rds"))

# pppc_stacking_check.rds:
#   Independent verification of NEON's reconstructed 400 m2 species list
#   against the direct-scale species union for complete plot-events.
#   Review all stack_mismatch == TRUE cases before ecological analyses.
saveRDS(pppc_stacking_check, file.path(pppc_dir, "pppc_stacking_check.rds"))

# pppc_1m_cover_conflicts.rds:
#   1 m2 plot/subplot-events where more than one accepted observation collapses
#   to the same binomial; review BEFORE aggregating species-specific cover.
saveRDS(pppc_1m_cover_conflicts,
        file.path(pppc_dir, "pppc_1m_cover_conflicts.rds"))

if (make_wide) {
  # WIDE species matrices (one row per sampling unit/event): columns are
  # binomial species names. The 1 m2 cover matrix contains percent cover;
  # the remaining matrices contain 0/1 observed presence, NOT percent cover.
  # Always retain status/completeness fields to avoid misreading nondetections.
  saveRDS(pppc_1m_cover_wide, file.path(pppc_dir, "pppc_1m_cover_wide.rds"))
  saveRDS(pppc_1m_presence_wide, file.path(pppc_dir, "pppc_1m_presence_wide.rds"))
  saveRDS(pppc_10m_presence_wide, file.path(pppc_dir, "pppc_10m_presence_wide.rds"))
  saveRDS(pppc_100m_presence_wide, file.path(pppc_dir, "pppc_100m_presence_wide.rds"))
  saveRDS(pppc_400m_presence_wide, file.path(pppc_dir, "pppc_400m_presence_wide.rds"))
}

# Example: selected sites and scales from Script 01's full dataset:
# subset_obs <- readRDS(file.path(pppc_dir, "pppc_allObservations.rds")) %>%
#   filter(siteID %in% c("HARV", "SCBI"), subplot_area_m2 %in% c(1, 100))
#
# Example: downstream Script 03 can load only what it needs:
# cover <- readRDS(file.path(pppc_dir, "pppc_1m_cover_long.rds"))
# spp400 <- readRDS(file.path(pppc_dir, "pppc_400m_species_long.rds"))
# richness <- readRDS(file.path(pppc_dir, "pppc_plot_richness.rds"))
