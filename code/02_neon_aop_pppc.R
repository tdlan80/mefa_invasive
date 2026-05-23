# Script: 02_neon_aop_pppc.R
# Purpose: For a target NEON site, overlay PPPC 1 m² subplot data with the
#          NEON AOP Canopy Height Model (CHM, DP3.30015.001).
#
# Prerequisite: run 01_pppc_download_process.R first to build the PPPC cache.
#
# Workflow:
#   1. Configure target site, AOP product, and repo-relative paths
#   2. Load processed PPPC cache (output of Script 01)
#   3. Download NEON TOS spatial data (subplot centroids) from web if not cached
#   4. Download NEON AOP CHM tiles via the NEON API for all PPPC years
#   5. Loop over all years with AOP tiles on disk:
#        a. Load and merge CHM tiles for the year
#        b. Aggregate invasive percent cover per subplot
#        c. Extract CHM values at subplot centroids
#        d. Produce a spatial overlay map and a cover-vs-height scatter plot
#   6. Combine all years into a single tidy data frame (pppc_final_all)
#
# CyVerse / HPC notes:
#   - Set NEON_TOKEN env var for higher API rate limits:
#       export NEON_TOKEN="your_token_here"    # bash / zsh
#       $env:NEON_TOKEN = "your_token_here"    # PowerShell
#   - All paths are relative to the repo root (anchored by .Rproj via here()).
#   - AOP download is skipped per-year when tiles already exist on disk.
#   - Need to build a gocmd to trandfer data from cyverse storage to instance

# List of packages
packages <- c("tidyverse", "neonUtilities", "sf", "terra", "tidyterra", "here", "patchwork")

# Install/load packages
lapply(packages, function(x){
  if (require(x, character.only = TRUE) == TRUE){
    
    library(x, character.only = TRUE)
    
  } else {
    
    install.packages(x)
    
  }
})



# ==============================================================================
# Step 1: Configuration --------------------------------------------------------
# ==============================================================================

# ---- Site settings ----------------------------------------------------------

# Target NEON site for this run (four-letter code).
# Must be one of the sites in `sites` below (the set processed by Script 01).
site <- "SCBI"

# Sites must match what was used in 01_pppc_download_process.R so that the path
# to the PPPC cache works.
sites <- c("HARV", "SCBI", "GRSM")

# AOP data product: Discrete Return LiDAR Canopy Height Model
aop_product <- "DP3.30015.001"

# NEON API token (optional; reads from environment variable)
## Note: I moved this from being hard coded in the script to something you set
neon_token <- Sys.getenv("NEON_TOKEN")

# ---- Paths (all relative to repo root) ---------------------------

raw_dir     <- here("rawData", "NEON")
pppc_dir    <- file.path(raw_dir, "PPPC")
spatial_dir <- file.path(raw_dir, "All_NEON_TOS_Plots_V12")
aop_dir     <- file.path(raw_dir, "AOP")

# PPPC cache path — must match the naming logic in 01_pppc_download_process.R
cache_tag <- paste(sort(sites), collapse = "_")
pppc_path <- file.path(pppc_dir, paste0(cache_tag, "_1m_plantDiv.RData"))


# ==============================================================================
# Step 2: Load processed PPPC data --------------------------------------------
# ==============================================================================

# Check to see if the pppc data are already downloaded
if (!file.exists(pppc_path)) {
  stop(
    "PPPC cache not found at:\n  ", pppc_path,
    "\nRun 01_pppc_download_process.R first with sites = ",
    deparse(sites), "."
  )
}

# Loads the ppp data from the cache
message("Loading PPPC cache: ", pppc_path)
load(pppc_path)   # restores `pppc_1m_plantDiv`

# Filter to the target site for this run
pppc <- pppc_1m_plantDiv %>% filter(siteID == site)

if (nrow(pppc) == 0) {
  stop(site, " not found in the PPPC cache. ",
       "Add it to `sites` in Script 01 and re-run that script.")
}

# Some print statements to check data availability
message(site, " — years: ",  paste(sort(unique(pppc$year)),  collapse = ", "))
message(site, " — plots: ",  length(unique(pppc$plotID)))
message(site, " — plot_subplots: ", length(unique(pppc$plot_subplot)))


# ==============================================================================
# Step 3: NEON TOS spatial data (subplot centroids) ---------------------------
# ==============================================================================

# Source: NEON TOS Plot Spatial Data V12
## Note: updated this from the previous v11 release
# Download page: https://www.neonscience.org/data-collection/spatiotemporal-data

# Direct download URL --> can update if they release new data
tos_zip_url  <- paste0("https://www.neonscience.org/sites/default/files/",
                       "All_NEON_TOS_Plots_V12.zip")
tos_zip_path <- file.path(raw_dir, "All_NEON_TOS_Plots_V12.zip")

# Again, checking if teh data already exist before redownloading
if (!dir.exists(spatial_dir)) {
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  message("Downloading NEON TOS spatial data ...")
  download.file(tos_zip_url, destfile = tos_zip_path, mode = "wb")
  unzip(tos_zip_path, exdir = raw_dir)
  message("Extracted to: ", raw_dir)
} else {
  message("Using cached NEON TOS spatial data: ", spatial_dir)
}

# Load subplot shapefile and make some helper columns
subplots <- read_sf(
  file.path(spatial_dir, "All_NEON_TOS_Plot_Subplots_V12.shp"),
  quiet = TRUE
) %>%
  mutate(
    siteID       = substr(plotID, 1, 4),
    plot_subplot = paste0(plotID, "_", subplotID)
  )

# Filter to target site
site_subplots <- subplots %>% filter(siteID == site)

# Just check the crs and number of plots
message("CRS: ",          st_crs(site_subplots)$Name)
message("Spatial plots: ", length(unique(site_subplots$plotID)))

# Check what plots might be deprecated
# These would show up in the PPPC data but not have spatial points anymore
# if they aren't being sampled
missing_plots <- unique(pppc$plotID[!(pppc$plotID %in% site_subplots$plotID)])
if (length(missing_plots) > 0) {
  warning(length(missing_plots),
          " plot(s) have PPPC records but no spatial entry:\n  ",
          paste(missing_plots, collapse = ", "))
}


# ==============================================================================
# Step 4: Download NEON AOP CHM tiles via the NEON API ------------------------
# ==============================================================================

# Filter the subplots to only those that we have pppc data for
# this mostly just gets rid of non-PDIV plots like TCK, MAM, MOS, etc
site_subplots_pppc <- site_subplots %>%
  filter(plot_subplot %in% pppc$plot_subplot)

# Just checking we aren't getting anything thats not a 1 m subsplot
message("Subplot size: ",
        paste(unique(site_subplots_pppc$subpltSize), collapse = ", "))

# Get the years we have pppc data for to download AOP for these
years <- sort(unique(pppc$year))

# Make a directory for the AOP data
dir.create(aop_dir, recursive = TRUE, showWarnings = FALSE)

# Download CHM tiles for each year.
# byTileAOP() checks whether files already exist and skips them.
# buffer = 10m just in case any of these are on the edge of AOP tiles.
# check.size = FALSE: avoids neon asking download questions.
# include.provisional = TRUE: gets the mos tup to date data, but they can change
for (yr in years) {
  message("\nRequesting AOP tiles for ", site, " ", yr, " ...")
  tryCatch(
    byTileAOP(
      dpID = aop_product,
      site = site,
      year = yr,
      easting = site_subplots_pppc$easting,
      northing = site_subplots_pppc$northing,
      buffer = 10,
      check.size = FALSE,
      include.provisional = TRUE,
      token = if (nzchar(neon_token)) neon_token else NA_character_,
      savepath = aop_dir
    ),
    error = function(e) {
      message("  No AOP data for ", site, " ", yr, ": ", e$message)
    }
  )
}


# ==============================================================================
# Step 5: Loop over all years with AOP data — extract CHM
# ==============================================================================

# Identify which years in the PPPC record actually have AOP tiles on disk.
# This avoids attempting to process years where the download failed or when
# AOP didn't fly.
years_with_aop <- Filter(function(yr) {
  yr_root <- file.path(aop_dir, aop_product, "neon-aop-products", yr, "FullSite")
  tiles <- list.files(
    path = yr_root,
    pattern = paste0("NEON_.*_", site, "_DP3.*\\.tif$"),
    recursive = TRUE
  )
  length(tiles) > 0
}, years)

# Prints the AOP flight years and PDIV sampling years (RIP 2026)
message("\nYears with PPPC data  : ", paste(years,          collapse = ", "))
message("Years with AOP tiles  : ", paste(years_with_aop, collapse = ", "))

# We now want to loop through all the years with AOP data
# This is an empty list we will accumulate pppc/chm joined data into
pppc_final_all  <- list()
scatter_plots   <- list()   # accumulates per-year scatter plots for end mosaic

for (yr in years_with_aop) {

  message("\n=== ", site, " ", yr, " ===")

  # --- 5a. Load and mosaic CHM tiles for this year ---------------------------
  # This is taking all the individual tiles for an AOP year and merging them
  # into one mosaic raster

  chm_root  <- file.path(aop_dir, aop_product, "neon-aop-products", yr, "FullSite")
  chm_files <- list.files(
    path = chm_root,
    pattern = paste0("NEON_.*_", site, "_DP3.*\\.tif$"),
    full.names = TRUE,
    recursive = TRUE
  )

  message("Merging ", length(chm_files), " CHM tile(s) ...")
  chm_yr <- lapply(chm_files, rast) %>% do.call(merge, .)

  # --- 5b. Filter PPPC and aggregate invasive cover per subplot -------------

  pppc_yr <- pppc %>% filter(year == yr)

  if (nrow(pppc_yr) == 0) {
    message("No PPPC records for ", site, " ", yr, " — skipping.")
    next
  }

  # Flag subplots surveyed more than once in this year
  # Relevant for two-bout PDIV sites like JORN/TEAK but most are once a year
  multi_survey <- pppc_yr %>%
    distinct(plot_subplot, endDate) %>%
    count(plot_subplot, name = "n_surveys") %>%
    filter(n_surveys > 1)

  if (nrow(multi_survey) > 0) {
    warning(nrow(multi_survey),
            " subplot(s) have >1 survey date in ", yr,
            "; review before aggregating.")
  }

  # Sum invasive percent cover per subplot (NI = native & introduced, I = introduced)
  # Subplots with no invasives have 0 tpc
  ## Note: I added this filtering that wasn't present in the neon_aop_pppc_test.R
  ## version. Maybe you were working with data that were already filtered for
  ## invasives?
  pppc_yr_agg <- pppc_yr %>%
    filter(nativeStatusCode %in% c("I", "NI")) %>%
    group_by(plot_subplot) %>%
    summarise(
      Tpc = sum(percentCover, na.rm = TRUE),
      n_inv = n(),
      .groups = "drop"
    )

  # joining the inv % cover total and count to the yearly pppc data
  # each subplot should have the same values
  pppc_yr <- left_join(pppc_yr, pppc_yr_agg, by = "plot_subplot") %>%
    mutate(
      Tpc = replace_na(Tpc, 0),
      n_inv = replace_na(n_inv,   0L)
    )

  # --- 5c. Extract CHM at subplot centroids ---------------------------------

  subplots_yr <- subplots %>%
    filter(plot_subplot %in% pppc_yr$plot_subplot) %>%
    st_transform(st_crs(chm_yr))

  chm_vals <- extract(chm_yr, subplots_yr)
  subplots_yr <- subplots_yr %>%
    mutate(CHM = chm_vals[, 2]) %>%
    left_join(pppc_yr_agg %>% select(plot_subplot, Tpc), by = "plot_subplot") %>%
    mutate(Tpc = replace_na(Tpc, 0))
  
  # Join CHM and PPPC data; drop subplots with no CHM coverage
  # Subplots missing CHM are typically deprecated plots absent from the TOS
  # spatial dataset (e.g. SCBI_005) — flagged above in missing_plots.
  n_subplots_total <- length(unique(pppc_yr$plot_subplot))

  pppc_final_yr <- left_join(
    pppc_yr,
    st_drop_geometry(subplots_yr) %>% select(plot_subplot, CHM),
    by = "plot_subplot"
  ) %>%
    select(
      siteID, plotID, subplotID, plot_subplot,
      year, endDate, scientificName, binomialName, nativeStatusCode,
      percentCover, Tpc, n_inv, CHM
    ) %>%
    distinct() %>%
    filter(!is.na(CHM))

  n_subplots_chm <- length(unique(pppc_final_yr$plot_subplot))

  # Total subplots sampled vs those retained after CHM join
  message("Subplots (total)   : ", n_subplots_total)
  message("Subplots with CHM  : ", n_subplots_chm,
          if (n_subplots_chm < n_subplots_total)
            paste0("  [", n_subplots_total - n_subplots_chm, " dropped — no spatial data]")
          else "")
  
  # Subplots with and without invasives
  message("Subplots Tpc = 0   : ",
          pppc_final_yr %>% distinct(plot_subplot, Tpc) %>% filter(Tpc == 0) %>% nrow())
  
  # Invasives plots
  message("Subplots Tpc > 0   : ",
          pppc_final_yr %>% distinct(plot_subplot, Tpc) %>% filter(Tpc  > 0) %>% nrow())
  
  # append to the list we started at the top of this
  pppc_final_all[[as.character(yr)]] <- pppc_final_yr

  # --- 5d. Visualize --------------------------------------------------------

  pppc_plot_yr <- pppc_final_yr %>% distinct(plot_subplot, Tpc, CHM)

  # Spatial overlay: CHM raster with subplot centroids colored by invasive cover
  p_spatial <- ggplot() +
    geom_spatraster(data = chm_yr) +
    scale_fill_viridis_c(
      name = "CHM (m)",
      na.value = "transparent",
      option = "mako"
    ) +
    geom_sf(
      data = subplots_yr,
      aes(colour = Tpc, size = Tpc > 0),
      alpha = 0.85,
      shape = 16
    ) +
    scale_colour_gradient(
      low = "white",
      high = "red",
      name = "Invasive\ncover (%)"
    ) +
    scale_size_manual(
      values = c("FALSE" = 1.5, "TRUE" = 3),
      labels = c("FALSE" = "None", "TRUE" = "Present"),
      name = "Invasives"
    ) +
    labs(title = paste("Subplot invasive cover overlaid on CHM:", site, yr)) +
    theme_bw() +
    theme(legend.position = "right")

  print(p_spatial)

  # Subset to subplots with CHM for distribution plots and scatter
  pppc_plot_yr <- pppc_plot_yr %>% filter(!is.na(CHM))

  # Histograms: distributions of CHM and Tpc for year
  p_hist_chm <- ggplot(pppc_plot_yr, aes(x = CHM)) +
    geom_histogram(bins = 30, fill = "#A2CD5A", colour = "white") +
    labs(
      x = "Canopy height (m)",
      y = "Subplots",
      title = paste("CHM distribution:", site, yr)
    ) +
    theme_bw()

  p_hist_tpc <- ggplot(pppc_plot_yr, aes(x = Tpc)) +
    geom_histogram(bins = 30, fill = "#CD6600", colour = "white") +
    labs(
      x = "Total invasive percent cover (%)",
      y = "Subplots",
      title = paste("Invasive cover distribution:", site, yr)
    ) +
    theme_bw()

  print(p_hist_chm + p_hist_tpc)

  # Scatter with linear trend line; stored for end-of-run mosaic
  p_scatter <- ggplot(pppc_plot_yr, aes(x = Tpc, y = CHM)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, colour = "#CD9B1D", linewidth = 0.8) +
    labs(
      x = "Invasive cover (%)",
      y = "Canopy height (m)",
      title = as.character(yr),
      subtitle = paste0("n = ", nrow(pppc_plot_yr), " subplots")
    ) +
    theme_bw()

  print(p_scatter)

  scatter_plots[[as.character(yr)]] <- p_scatter
}


# ==============================================================================
# Step 6: Combine all years into a single data frame ---------------------------
# ==============================================================================

pppc_final_all <- bind_rows(pppc_final_all)

message("\n--- Combined output summary (all years) ---")
message("Years processed : ", paste(sort(unique(pppc_final_all$year)), collapse = ", "))
message("Rows            : ", nrow(pppc_final_all))
message("Subplots        : ", length(unique(pppc_final_all$plot_subplot)))
message("Subplots w/ CHM : ",
        pppc_final_all %>% distinct(plot_subplot, CHM) %>% filter(!is.na(CHM)) %>% nrow())

# ==============================================================================
# Step 7: Multi-year scatter ---------------------------------------------------
# ==============================================================================

# Put per-year scatter plots into one figure
if (length(scatter_plots) > 0) {
  mosaic <- wrap_plots(scatter_plots) +
    plot_annotation(
      title = paste("Invasive cover vs. canopy height —", site, "(all years)"),
      caption = "Points = 1 m² subplots; line = OLS with 95% CI",
      theme = theme_bw()
    )
  print(mosaic)
}
