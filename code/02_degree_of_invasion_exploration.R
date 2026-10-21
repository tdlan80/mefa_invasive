# ==============================================================================
# Script: 02_degree_of_invasion_exploration.R
# not run yet, this was from Matt's md file
# Title: Degree of Invasion Exploration
# Original analysis: Matthew Aiello-Lammens (2026-07-25)
#
# Purpose:
#   1. Calculate the proportion of invasive species richness and plant cover
#      in NEON 1 m2 vegetation subplots.
#   2. Compare richness-based, cover-based, and combined invasion indices.
#   3. Examine the latest sampling year for each subplot.
#   4. Repeat the analysis for live woody stems using aboveground biomass.
#
# Inputs (relative to the RStudio project root):
#   Preferred: rawData/NEON/PPPC/pppc_all_terrestrial_all_scales.RData
#              (output from 01_pppc_download_process_all_sites.R)
#   Fallback:  processedData/div_1m2_allNeonSites_plantDiv.RData
#   Woody:     processedData/woody_biomass.csv (optional)
#
# Run inside the RStudio Project. Figures appear in the Plots pane.
# This script does not modify or overwrite the source datasets.
# ==============================================================================

# Updated the input to use our newly consolidated all-site NEON PPPC dataset, restricted to 1 m² 
#subplots because these contain percent-cover measurements. The original input remains a fallback.
# Corrected s_inv to count unique invasive taxa, rather than invasive observation records.
# Used endDate to identify sampling years instead of extracting years from eventID.
# Ensured figures display when running the script through RStudio's Source button.
# Made the woody-biomass analysis conditional on its input file being available.



# ---- 1. Packages -------------------------------------------------------------
# Install once, if necessary:
# install.packages(c("tidyverse", "GGally", "here"))

library(tidyverse)
library(GGally)
library(here)

# ---- 2. Load plant presence and percent cover data ---------------------------

# Use the new all-site PPPC dataset when available; otherwise use the original
# 1 m2 dataset used in the R Markdown analysis.
new_pppc_file <- here("rawData", "NEON", "PPPC",
                      "pppc_all_terrestrial_all_scales.RData")
old_pppc_file <- here("processedData",
                      "div_1m2_allNeonSites_plantDiv.RData")

if (file.exists(new_pppc_file)) {

  load(new_pppc_file)  # Restores pppc_plantDiv and related objects.

  # Cover measurements are from the 1 m2 table, so exclude the larger
  # presence-only sampling units for this particular invasion analysis.
  plants <- pppc_plantDiv %>%
    filter(subplot_area_m2 == 1)

  message("Plant data loaded from the new all-site PPPC cache (1 m2 only).")

} else if (file.exists(old_pppc_file)) {

  load(old_pppc_file)  # Restores div_1m2_allNeonSites_plantDiv.
  plants <- div_1m2_allNeonSites_plantDiv

  message("Plant data loaded from the original 1 m2 dataset.")

} else {
  stop("Cannot find either PPPC input file. Run script 01 first, or check paths.")
}

# Use the measurement date to obtain the year, rather than assuming that
# eventID always ends with a four-digit year.
plants <- plants %>%
  mutate(year = lubridate::year(as.Date(endDate)))

# ---- 3. Check for repeated taxa in subplot sampling events ------------------

# A subplot can contain multiple rows for the same taxon. Compare the number
# of rows with the number of distinct taxon IDs before calculating richness.
plants_summary <- plants %>%
  group_by(siteID, plotID, subplotID, eventID) %>%
  summarise(
    n_obs  = n(),
    n_spec = n_distinct(taxonID, na.rm = TRUE),
    .groups = "drop"
  )

# Display events with multiple observations or missing taxon IDs.
possible_duplicates <- plants_summary %>%
  filter(n_obs != n_spec)

message("Subplot-events with repeated/missing taxon IDs: ", nrow(possible_duplicates))
print(head(possible_duplicates, 20))  # Full results remain in possible_duplicates.

# ---- 4. Degree of invasion: 1 m2 plant subplots ------------------------------

# s_inv: number of DISTINCT invasive taxa (nativeStatusCode == "I")
# s_tot: number of distinct recorded taxa
# b_inv: summed percent cover attributed to invasive taxa
# b_tot: summed percent cover for all recorded taxa
# di_s : proportion of taxa that are invasive (s_inv / s_tot)
# di_b : proportion of summed plant cover that is invasive (b_inv / b_tot)
# di_guo: mean of richness-based and cover-based indices
#
# Important correction to the original Markdown:
#   s_inv uses n_distinct(), not sum(nativeStatusCode == "I"), because the
#   latter counts observation rows and can overstate invasive richness.
#
# Missing invasion status is NOT assumed to mean native. Check these records
# if you intend to interpret di_s as the fraction of all taxa that are invasive.
# A zero cover denominator produces NA for di_b, not zero.

degree_of_invasion <- plants %>%
  group_by(siteID, plotID, subplotID, eventID) %>%
  summarise(
    n_obs = n(),
    year  = max(year, na.rm = TRUE),
    s_inv = n_distinct(taxonID[nativeStatusCode == "I"], na.rm = TRUE),
    s_tot = n_distinct(taxonID, na.rm = TRUE),
    b_inv = sum(percentCover[nativeStatusCode == "I"], na.rm = TRUE),
    b_tot = sum(percentCover, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    di_s   = if_else(s_tot > 0, s_inv / s_tot, NA_real_),
    di_b   = if_else(b_tot > 0, b_inv / b_tot, NA_real_),
    di_guo = (di_s + di_b) / 2
  )

# Inspect distributions and relationships among the three invasion indices.
# This uses the full dataset, as in the original analysis.
p_pairs <- degree_of_invasion %>%
  filter(!is.na(di_s) | !is.na(di_b) | !is.na(di_guo)) %>%
  filter(di_s <= 1) %>%
  select(di_s, di_b, di_guo) %>%
  GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.1)))
print(p_pairs)  # Needed when running the whole file with Source in RStudio.

# ---- 5. Select the latest sampling year for each 1 m2 subplot ---------------

# The original analysis retained ALL sampling events in the most recent year
# for each site/plot/subplot, rather than only the single latest event.
# We preserve that behavior here.
degree_of_invasion_recent <- degree_of_invasion %>%
  filter(!is.na(year)) %>%
  group_by(siteID, plotID, subplotID) %>%
  filter(year == max(year)) %>%
  ungroup()

# ---- 6. Plant invasion: linear model and plots -------------------------------

# Association between cover-based and richness-based invasion indices,
# using all sampling events. This is descriptive and does not account for
# repeated observations nested within plots and sites.
di_lm <- lm(di_b ~ di_s, data = degree_of_invasion)
print(summary(di_lm))
# plot(di_lm)  # Uncomment to examine residual diagnostics.

# Relationship between invasive richness fraction and invasive cover fraction
# in each subplot's latest sampling year.
p_cover <- degree_of_invasion_recent %>%
  filter(di_s <= 1) %>%
  ggplot(aes(x = di_s, y = di_b, color = siteID)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE)
print(p_cover)

# Relationship between number of invasive taxa and richness fraction.
p_richness <- degree_of_invasion_recent %>%
  filter(di_s <= 1) %>%
  ggplot(aes(x = s_inv, y = di_s, color = siteID)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE)
print(p_richness)

# Distribution of richness-based invasion fraction across all events.
p_hist_s <- degree_of_invasion %>%
  filter(di_s <= 1) %>%
  ggplot(aes(x = di_s)) +
  geom_histogram()
print(p_hist_s)

# Distribution of cover-based invasion fraction across all events.
p_hist_b <- degree_of_invasion %>%
  filter(di_s <= 1) %>%
  ggplot(aes(x = di_b)) +
  geom_histogram()
print(p_hist_b)

# ---- 7. Woody biomass analysis ----------------------------------------------

# Woody biomass is a separate input, not part of the PPPC download above.
# Skip this section when that file is absent, so the plant-cover analysis
# remains usable on its own.
woody_file <- here("processedData", "woody_biomass.csv")

if (file.exists(woody_file)) {

  # Extract taxonomic native/invasive status from the plant observations.
  # As in the original analysis, retain one status per taxon ID.
  # Check any conflicting statuses before relying on the join.
  native_status_conflicts <- plants %>%
    filter(!is.na(taxonID), !is.na(nativeStatusCode)) %>%
    distinct(taxonID, nativeStatusCode) %>%
    count(taxonID) %>%
    filter(n > 1)

  if (nrow(native_status_conflicts) > 0) {
    warning("Some taxa have multiple nativeStatusCode values. Review before interpreting woody results.")
  }

  native_status <- plants %>%
    select(taxonID, nativeStatusCode) %>%
    distinct(taxonID, .keep_all = TRUE)

  # Read woody biomass measurements and add status by taxon ID.
  woody <- readr::read_csv(woody_file, show_col_types = FALSE) %>%
    left_join(native_status, by = "taxonID")

  # Report the number of woody records without an assigned status.
  message("Woody records missing nativeStatusCode: ",
          sum(is.na(woody$nativeStatusCode)))

  # Retain live woody stems, matching the original analysis.
  # NOTE: Filtering to live BEFORE taking the latest year means that a stem
  # that later died can still contribute its last LIVE measurement.
  woody <- woody %>%
    filter(liveDeadStatus == "live")

  # Retain the most recent live measurement year for each individual.
  # Group by site too, in case individual IDs are not globally unique.
  woody_recent <- woody %>%
    filter(!is.na(year)) %>%
    group_by(siteID, individualID) %>%
    filter(year == max(year)) %>%
    ungroup()

  # Calculate richness and aboveground-biomass invasion indices.
  # All stems of a given taxon contribute to biomass; each taxon contributes
  # only once to the species-richness numerator and denominator.
  woody_degree_of_invasion <- woody_recent %>%
    group_by(siteID, plotID, subplotID) %>%
    summarise(
      n_obs = n(),
      s_inv = n_distinct(taxonID[nativeStatusCode == "I"], na.rm = TRUE),
      s_tot = n_distinct(taxonID, na.rm = TRUE),
      b_inv = sum(agb_kg[nativeStatusCode == "I"], na.rm = TRUE),
      b_tot = sum(agb_kg, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      di_s   = if_else(s_tot > 0, s_inv / s_tot, NA_real_),
      di_b   = if_else(b_tot > 0, b_inv / b_tot, NA_real_),
      di_guo = (di_s + di_b) / 2
    )

  # Number of woody plots/subplots with at least one invasive taxon.
  print(sum(woody_degree_of_invasion$s_inv > 0, na.rm = TRUE))

  # Relationship between richness-based and biomass-based invasion indices.
  p_woody <- ggplot(woody_degree_of_invasion,
                    aes(x = di_s, y = di_b, color = siteID)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)
  print(p_woody)

  # Pairwise exploration of the three woody invasion indices.
  p_woody_pairs <- woody_degree_of_invasion %>%
    select(di_s, di_b, di_guo) %>%
    GGally::ggpairs()
  print(p_woody_pairs)

} else {
  message("Woody analysis skipped: file not found: ", woody_file)
}

# ==============================================================================
# Objects produced:
#   plants                        - PPPC records from 1 m2 subplots
#   plants_summary                - diagnostic taxon-count summary
#   possible_duplicates           - events with repeat taxon records
#   degree_of_invasion            - indices for all 1 m2 subplot events
#   degree_of_invasion_recent     - events in latest year per subplot
#   di_lm                         - descriptive linear model
#   (when woody_biomass.csv exists)
#   native_status, woody, woody_recent, woody_degree_of_invasion
# ==============================================================================
