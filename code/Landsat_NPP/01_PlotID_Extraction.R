# ##############################################################################
# Introduction:
#   1) Extract the plot IDs from the PPPC "plantDiv" dataset and identify
#      the corresponding NEON polygons.
# 
# Last updated: 9/26/2025.
# ##############################################################################


# 0) Setup. ---------------------------------------------------------------

# Load the required packages.
library(tidyverse)
library(sf)

# Set the theme of plots.
theme_set(
  theme_bw() +
    theme(
      axis.title = element_text(size = 8, face = "bold"),
      axis.text = element_text(size = 4),
      
      strip.text = element_text(size = 5, margin = margin(1, 1, 1, 1)),
      
      panel.spacing = unit(0.1, "lines")
    )
)

# Disable printing results in scientific notation.
options(scipen = 999)

# Display all columns of a data frame.
options(tibble.width = Inf)

# Set the working directory.
setwd("..")


# 1) Load the data. --------------------------------------------------------

# Load the PPPC "plantDiv" dataset.
load(
  file = file.path(
    "Data",
    "NEON",
    "PPPC2025",
    "Harv_Scbi_Grsm_1m_allPlantsNeon2025_plantDiv.RData"
  )
)

ls()

PPPC_3sites_DF <- Harv_Scbi_Grsm_1m_allPlantsNeon2025_plantDiv

# Load the raw shapefile of NEON polygons.
raw_NEONpolygons_SF <- st_read(
  dsn = file.path(
    "Data",
    "NEON",
    "All_NEON_TOS_Plots_V11",
    "All_NEON_TOS_Plot_Polygons_V11.shp"
  ),
  stringsAsFactors = TRUE
)


# 1) Extract NEON polygons of interest. -----------------------------------

# Extract the distinct plot IDs from PPPC data as a vector.
plotIDs_3sites_Vec <- PPPC_3sites_DF |> 
  pull(plotID) |> 
  unique() |> 
  sort()

plotIDs_3sites_Vec |> length() # 104.

# Extract the NEON polygons that match the plot IDs.
filtered_NEONpolygons_SF <- raw_NEONpolygons_SF |> 
  filter(plotID %in% plotIDs_3sites_Vec)

# Save the result as a Shapefile.
filtered_NEONpolygons_FileName_Str <- "NEONpolygonsV11_3sites_PPPC2025plantDiv"

if (!dir.exists(
  file.path("Data", "NEON", filtered_NEONpolygons_FileName_Str)
  )) {
  dir.create(
    file.path("Data", "NEON", filtered_NEONpolygons_FileName_Str)
  )
}

st_write(
  obj = filtered_NEONpolygons_SF,
  dsn = file.path(
    "Data",
    "NEON",
    filtered_NEONpolygons_FileName_Str,
    paste0(filtered_NEONpolygons_FileName_Str, ".shp")
  )
)

