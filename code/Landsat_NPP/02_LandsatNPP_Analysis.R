# ##############################################################################
# Introduction:
#   1) Analyze the annual Landsat-based NPP data at the NEON base plots of
#      interest (v11).
# 
# Last updated: 9/26/2025.
# 
# Author: Chenyang Wei (chenyangwei.cwei@gmail.com)
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
      
      strip.text = element_text(size = 4, margin = margin(1, 1, 1, 1)),
      
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

# Local NEON base plots with annual NPP data.
annualNPP_NEONplots_FileName_Str <- 
  "WtdAvg_AnnualNPP_2013to2020_3sitesV11_PPPC2025plantDiv"

annualNPP_NEONplots_SF <- st_read(
  dsn = file.path(
    "Data",
    "Landsat_NPP",
    annualNPP_NEONplots_FileName_Str,
    paste0(annualNPP_NEONplots_FileName_Str, ".shp")
  ),
  stringsAsFactors = FALSE
)

annualNPP_NEONplots_DF <- annualNPP_NEONplots_SF |> 
  st_drop_geometry()

annualNPP_NEONplots_DF |> head()
annualNPP_NEONplots_DF |> nrow() # 95

annualNPP_NEONplots_DF <- annualNPP_NEONplots_DF |> 
  mutate(subtype = as.factor(subtype))

annualNPP_NEONplots_DF |> select(subtype) |> 
  summary()


# 2) Address the NPP data gap. --------------------------------------------

# Check the original NPP data gap columns.
annualNPP_NEONplots_DF |>
  select(starts_with("NPP_m_")) |>
  summary()

# Remove the NEON plots with annual NPP data < 0.95.
annualNPP_Filtered_DF <- annualNPP_NEONplots_DF |>
  filter(
    NPP_m_2013 >= 0.95 &
      NPP_m_2014 >= 0.95 &
      NPP_m_2015 >= 0.95 &
      NPP_m_2016 >= 0.95 &
      NPP_m_2017 >= 0.95 &
      NPP_m_2018 >= 0.95 &
      NPP_m_2019 >= 0.95 &
      NPP_m_2020 >= 0.95
  )

# Check the filtered NPP data.
annualNPP_Filtered_DF |> nrow() # 87

annualNPP_Filtered_DF |>
  select(starts_with("NPP_m_")) |>
  summary() # Min.: 0.9650


# 3) Analyze the annual NPP data. -----------------------------------------

# Select the columns of interest.
annualNPP_Wide_DF <- annualNPP_Filtered_DF |>
  select(
    siteID,
    plotID,
    siteName,
    plotType,
    longitude,
    latitude,
    elevation,
    starts_with("NPP_2")
  )

# # Save the data frame as a CSV file.
# write_csv(
#   annualNPP_Wide_DF,
#   file.path(
#     "Data",
#     "Landsat_NPP",
#     "AnnualNPP_2013to2020_AllBasePlots_Filtered_WideTable.csv"
#   )
# )

# Reshape the data to a long format.
annualNPP_Long_DF <- annualNPP_Wide_DF |>
  pivot_longer(
    cols = starts_with("NPP_"),
    names_to = "Year",
    values_to = "Landsat_NPP"
  ) |>
  mutate(
    Year = str_remove(Year, "NPP_") |>
      as.integer()
  )

# # Save the data frame as a CSV file.
# write_csv(
#   annualNPP_Long_DF,
#   file.path(
#     "Data",
#     "Landsat_NPP",
#     "AnnualNPP_2013to2020_AllBasePlots_Filtered_LongTable.csv"
#   )
# )

# # Read the saved long format data frame.
# annualNPP_Long_DF <- read_csv(
#   file.path(
#     "Data",
#     "Landsat_NPP",
#     "AnnualNPP_2013to2020_AllBasePlots_Filtered_LongTable.csv"
#   )
# )

# Convert each column of interest to a factor.
annualNPP_Long_DF <- annualNPP_Long_DF |>
  mutate(
    # Remove the " NEON" suffix from site names.
    siteName = str_remove(siteName, " NEON")
  ) |>
  mutate(
    siteID = factor(
      siteID
    )
  ) |>
  mutate(
    siteName = factor(
      siteName
    )
  ) |>
  mutate(
    plotID = factor(
      plotID
    )
  ) |>
  mutate(
    plotType = factor(
      plotType
    )
  )

annualNPP_Long_DF |> summary()


# 4. Visualize the result. ------------------------------------------------

annualNPP_Long_DF |> head()

# Check the site names.
unique(annualNPP_Long_DF$siteName) # 3 sites.

# Summarize the information about site locations,
#   and order the site names by the summarized latitude.
annualNPP_SiteInfo_DF <- annualNPP_Long_DF |>
  group_by(siteID, siteName) |>
  summarise(
    Long_mean = mean(longitude, na.rm = TRUE),
    Lat_mean = mean(latitude, na.rm = TRUE),
    Elv_mean = mean(elevation, na.rm = TRUE)
  ) |>
  ungroup() |>
  arrange(Lat_mean)

# Extract the ordered site names as a vector.
siteNames_LatOrdered_Vec <- annualNPP_SiteInfo_DF$siteName

# Convert the site names to a factor with the specified order.
annualNPP_Long_DF <- annualNPP_Long_DF |>
  mutate(
    siteName = factor(
      siteName,
      levels = siteNames_LatOrdered_Vec
    )
  )

# Check the number of plot IDs.
annualNPP_Long_DF |> 
  pull(plotID) |> 
  unique() |> 
  length() # 87

# Visualize the annual NPP data at the level of base plots.
annualNPP_AllSites_Plt <- annualNPP_Long_DF |> 
  ggplot(aes(x = Year, y = Landsat_NPP, group = plotID, color = latitude)) +
  geom_line(lwd = 0.3, alpha = 0.3) +
  geom_point(size = 0.1, alpha = 0.5) +
  labs(
    x = "Year",
    y = "Plot-Level Landsat NPP (kg*C/m²)"
  ) +
  facet_wrap(~ siteName, ncol = 3) +
  scale_color_gradientn(
    colors = c("red", "blue"),
    name = "Site\nAverage\nLatitude\n(degrees)"
  ) +
  # scale_color_viridis_c(
  #   name = "Site\nAverage\nLatitude\n(degrees)",
  #   option = "magma",
  #   direction = -1 # Reverse the color scale.
  # ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 5),
    legend.text = element_text(size = 4),
    
    # Reduce the size of legend.
    legend.key.size = unit(0.3, "cm"),
    
    # Reduce the margin of the legend.
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0)
  )

annualNPP_AllSites_Plt

png(filename = file.path(
  "Figures",
  "Annual_LandsatNPP_LatOrdered_3sites_87plots.png"),
  width = 4000, height = 3000,
  units = "px", res = 1000)
annualNPP_AllSites_Plt
dev.off()

# Summarize the annual NPP data by site and year.
annualNPP_Summary_DF <- annualNPP_Long_DF |> 
  group_by(siteID, siteName, Year) |> 
  summarise(
    NPP_median = median(Landsat_NPP, na.rm = TRUE),
    NPP_mean = mean(Landsat_NPP, na.rm = TRUE),
    NPP_sd = sd(Landsat_NPP, na.rm = TRUE),
    
    Long_mean = mean(longitude, na.rm = TRUE),
    Lat_mean = mean(latitude, na.rm = TRUE),
    Elv_mean = mean(elevation, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  arrange(siteID, Year)

annualNPP_Summary_DF |> head()

# Visualize the summarized annual NPP data at the site level.
annualNPP_Summary_Plt <- annualNPP_Summary_DF |> 
  ggplot(aes(x = Year, y = NPP_median, group = Lat_mean, color = Lat_mean)) +
  geom_line(lwd = 0.2, alpha = 0.5) +
  geom_point(size = 0.1, alpha = 0.5) +
  # geom_errorbar( # Add vertical error bars.
  #   aes(
  #     ymin = NPP_mean - NPP_sd,
  #     ymax = NPP_mean + NPP_sd, 
  #     color = Lat_mean
  #   ),
  #   lwd = 0.1,
  #   show.legend = FALSE
  # ) +
  # # Use the viridis color palette.
  # scale_color_viridis_c(
  #   name = "Latitude\n(degrees)",
  #   direction = -1 # Reverse the color scale.
  # ) +
  scale_color_gradientn(
    colors = c("red", "blue"),
    name = "Site\nAverage\nLatitude\n(degrees)"
  ) +
  labs(
    x = "Year",
    y = "Site-Level Median Landsat NPP (kg*C/m²)"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 5),
    legend.text = element_text(size = 4),
    
    # Reduce the size of legend.
    legend.key.size = unit(0.3, "cm"),
    
    # Reduce the margin of the legend.
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0)
  )

annualNPP_Summary_Plt

png(filename = file.path(
  "Figures",
  # "Annual_LandsatNPP_MeanSD_3sites_87plots.png"), 
  "Annual_LandsatNPP_Median_3sites_87plots.png"), 
  width = 4000, height = 3000, 
  units = "px", res = 1000)
annualNPP_Summary_Plt
dev.off()

