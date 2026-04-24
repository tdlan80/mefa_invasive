# this script is to process NEON data of PPPC at one NEON site to overlay the NEON AOP data at subplot level

# the general workflow is:
# 1. define the target site ID used in this data process
# 2. load required NEON plots and subplots spatial data
# 3. load processed PPPC data from all available years by 2023? and filter for the target site
# 4. download NEON AOP data for the subplots in that site for all available years 
# 5. overlay PPPC data with AOP data for each subplot and each year, and export the merged dataset
# 6. visualize data relationships



library(tidyverse)
library(neonUtilities)
library(sf)
library(terra)
library(rasterVis)
library(tidyterra)


# Step 1 define the target site ID ---------------------------------------------
site="SCBI"
site="GRSM"

# Step 2 NEON spatial data -----------------------------------------------------
# load NEON plots and subplots spatial data
# V11
# plot_polygons <- read_sf("C:/Users/xiey2/Documents/Data/NEON/All_NEON_TOS_Plots_V11/All_NEON_TOS_Plot_Polygons_V11.shp",  quiet = T)
# plot_polygons
# colnames(plot_polygons)
# 
# plot_centroids <- read_sf("C:/Users/xiey2/Documents/Data/NEON/All_NEON_TOS_Plots_V11/All_NEON_TOS_Plot_Centroids_V11.shp",  quiet = T)
# plot_centroids
# colnames(plot_centroids)
# 
# site_plots_polygons=plot_polygons %>% filter(siteID==site) 
# site_plots=plot_centroids %>% filter(siteID==site) 
# sort(unique(site_plots$plotID))

plot_points <- read_sf("C:/Users/xiey2/Documents/Data/NEON/All_NEON_TOS_Plots_V11/All_NEON_TOS_Plot_Points_V11.shp",  quiet = T)
plot_points
colnames(plot_points)

subplots <- read_sf("C:/Users/xiey2/Documents/Data/NEON/All_NEON_TOS_Plots_V11/All_NEON_TOS_Plot_Subplots_V11.shp",  quiet = T)

subplots
colnames(subplots)

# get the siteID 
subplots$siteID=substr(subplots$plotID, 1, 4)

unique(subplots$siteID)

# filter subplots for the target site
site_subplots = subplots %>% filter(siteID==site)

# check coordinate reference system for the subplots spatial data
st_crs(site_subplots)   # WGS 84, EPSG 4326

# how many plots in the site
sort(unique(site_subplots$plotID))  # 43 plots at GRSM

# combine id for each plot and subplot
site_subplots$plot_subplot=paste0(site_subplots$plotID,"_",site_subplots$subplotID, sep="")

length(unique(site_subplots$plot_subplot))   
# 2177 GRSM
# 1977 unique plot_subplot, one row for each


# step 3 PPPC data -------------------------------------------------------------
# load PPPC data for the 3 NEON sites 
load("processedData/PPPC2025/Harv_Scbi_Grsm_1m_allPlantsNeon2025_plantDiv.RData")

# check data columns
colnames(Harv_Scbi_Grsm_1m_allPlantsNeon2025_plantDiv)
glimpse(Harv_Scbi_Grsm_1m_allPlantsNeon2025_plantDiv)

table(Harv_Scbi_Grsm_1m_allPlantsNeon2025_plantDiv$subplotID)
# 31_1_1 31_1_4 32_1_2 32_1_4 40_1_1 40_1_3 41_1_1 41_1_4 
# 7439   3722   7746   7523   7052   6966   3612   7371 

unique(Harv_Scbi_Grsm_1m_allPlantsNeon2025_plantDiv$endDate)

# add year column based on the endDate
Harv_Scbi_Grsm_1m_allPlantsNeon2025_plantDiv$year=year(Harv_Scbi_Grsm_1m_allPlantsNeon2025_plantDiv$endDate)

unique(Harv_Scbi_Grsm_1m_allPlantsNeon2025_plantDiv$year)
# [1] 2015 2016 2017 2018 2019 2020 2021 2022 2023 2014

# filter PPPC data for the target site
# SCBI_pppc = Harv_Scbi_Grsm_1m_allPlantsNeon2025_plantDiv %>%
#   filter(siteID=="SCBI")

pppc = Harv_Scbi_Grsm_1m_allPlantsNeon2025_plantDiv %>%
  filter(siteID==site)

# check unique year
unique(pppc$year)

# unique(SCBI_pppc$year)
# [1] 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023

# check unique plots and subplots
sort(unique(pppc$plotID))       # 37 plots GRSM


# sort(unique(SCBI_pppc$plotID))         # 34 plots have PPPC data
# [1] "SCBI_002" "SCBI_003" "SCBI_004" "SCBI_005" "SCBI_006" "SCBI_007" "SCBI_008" "SCBI_010" "SCBI_011" "SCBI_012"
# [11] "SCBI_013" "SCBI_014" "SCBI_015" "SCBI_016" "SCBI_017" "SCBI_018" "SCBI_019" "SCBI_021" "SCBI_022" "SCBI_023"
# [21] "SCBI_033" "SCBI_034" "SCBI_035" "SCBI_037" "SCBI_038" "SCBI_039" "SCBI_040" "SCBI_041" "SCBI_042" "SCBI_043"
# [31] "SCBI_044" "SCBI_045" "SCBI_047" "SCBI_067"

unique(pppc$subplotID)
# "32_1_4" "31_1_4" "41_1_1" "32_1_2" "40_1_1" "31_1_1" "41_1_4" "40_1_3"


# unique(SCBI_pppc$subplotID)
# "40_1_3" "31_1_1" "40_1_1" "32_1_4" "41_1_1" "31_1_4" "41_1_4" "32_1_2"

# combine id for each plot and subplot
pppc$plot_subplot=paste0(pppc$plotID,"_",pppc$subplotID, sep="")

length(unique(pppc$plot_subplot))   # 294 unique plot_subplot GRSM


# SCBI_pppc$plot_subplot=paste0(SCBI_pppc$plotID,"_",SCBI_pppc$subplotID, sep="")
# 
# length(unique(SCBI_pppc$plot_subplot))   # 272 unique plot_subplot




# Step 4 NEON AOP data ---------------------------------------------------------

# set NEON AOP product
# canopy height DP3.30015.001
product = "DP3.30015.001"

# filter the spatial data for all plot_subplot in PPPC data
site_subplots_pppc = site_subplots %>% filter(plot_subplot %in% SCBI_pppc$plot_subplot)


#site_plots_pppc = site_subplots %>% filter(plotID %in% SCBI_pppc$plotID)

# only 200 plot_subplot data found in the spatial dataset, but PPPC data has 272

# what are missing?
unique(SCBI_pppc$plotID[! (SCBI_pppc$plotID %in% site_subplots$plotID)])

# missed: "SCBI_005" "SCBI_012" "SCBI_033" "SCBI_034" "SCBI_037" "SCBI_039" "SCBI_041" "SCBI_042" "SCBI_044" from the subplots data

unique(SCBI_pppc$plotID[! (SCBI_pppc$plotID %in% site_plots$plotID)])
unique(SCBI_pppc$plotID[! (SCBI_pppc$plotID %in% site_plots_polygons$plotID)])

# same plots are missing from the plot_centroids data and plot_polygons data


# check subplots info
unique(site_subplots_pppc$subpltSize)   # they are all 1m subplots

# all years from PPPC data
years=unique(SCBI_pppc$year)

# download AOP product for all subplots at the target site from PPPC data, sets of coordinates
# buffer: Size, in meters, of the buffer to be included around the coordinates when determining which tiles to download. If easting and northing coordinates are the centroids of NEON TOS plots, use buffer=20. Here we use 1m subplots, so buffer = 1.

# in some years there are no AOP data - use tryCatch() to skip the years with no data
for (i in 1:length(years))
{
  tryCatch(byTileAOP(dpID = product, site=site, check.size = T, year=years[i],
          easting=site_subplots_pppc$easting, northing=site_subplots_pppc$northing, buffer=1,
          savepath="C:/Users/xiey2/Documents/Data/NEON"),
          error= function(e) {message("An error was ignored: ", e$message)})
}


# during download, some messages say "... ... classified_point_cloud_colorized.prj could not be downloaded". 
# this does not affect download the AOP product tif files. Just wait for the whole download to finish.
# last message: "Successfully downloaded 66 files to C:/Users/xiey2/Documents/Data/NEON/DP3.30015.001"
# actually only downloaded 11 tiles canopy height data from 2016 due to no cloud points data can be downloaded.

# when attempt to download data for the year no data was collected, get the error message: There are no data at the selected site and year.


# read data for one site one year (SCBI 2016)
dir_SCBI2016="C:/Users/xiey2/Documents/Data/NEON/DP3.30015.001/neon-aop-products/2016/FullSite/D02/2016_SCBI_1/L3/DiscreteLidar/CanopyHeightModelGtif/"
r = list.files(dir_SCBI2016, pattern = "NEON_D02_SCBI_DP3_*", full.names = TRUE)

r <- lapply(r, rast)
r <- do.call(merge, r)

scbi_2016=r

plot(scbi_2016)




# test one year 2022 ------------------------------------------------------------------------------------
# filter SCBI data in 2022
SCBI2022_pppc=Harv_Scbi_Grsm_1m_allPlantsNeon2025_plantDiv %>%
  filter(siteID=="SCBI", year==2022)

# combine id for each plot and subplot
SCBI2022_pppc$plot_subplot=paste0(SCBI2022_pppc$plotID,"_",SCBI2022_pppc$subplotID, sep="")

colnames(SCBI2022_pppc)

# check how many observations for each subplot on different dates
data_freq=as.data.frame(table(SCBI2022_pppc[, c(27,11)]))

data_freq=data_freq %>% filter(Freq>0)

table(data_freq$plot_subplot)   # for each subplot, there was only one survey in 2022, so no duplicates

unique(SCBI2022_pppc$plotID)


# calculate total percent cover of invasives
unique(SCBI2022_pppc$plot_subplot)
SCBI2022_pppc_t = SCBI2022_pppc %>% group_by(plot_subplot) %>% summarise(Tpc=sum(percentCover), n=n())
SCBI2022_pppc=left_join(SCBI2022_pppc, SCBI2022_pppc_t)




# process AOP raster data ---------------------------------------------------------------------
# read data for one site one year (SCBI 2022)
dir_SCBI2022="C:/Users/xiey2/Documents/Data/NEON/DP3.30015.001/neon-aop-products/2022/FullSite/D02/2022_SCBI_5/L3/DiscreteLidar/CanopyHeightModelGtif/"
r = list.files(dir_SCBI2022, pattern = "NEON_D02_SCBI_DP3_*", full.names = TRUE)

r <- lapply(r, rast)
r <- do.call(merge, r)

scbi_2022=r

plot(scbi_2022)




# get all subplot centroid points in SCBI
subplots$plot_subplot=paste0(subplots$plotID,"_",subplots$subplotID, sep="")


SCBI_subplots_points=subplots %>% filter(plot_subplot %in% SCBI2022_pppc$plot_subplot)      
SCBI_subplots_points=st_transform(SCBI_subplots_points, st_crs(scbi_2022))

# get canopy height for all points
scbi2022_chm = extract(scbi_2022, SCBI_subplots_points)
SCBI_subplots_points$CHM= scbi2022_chm[, 2]

SCBI_subplots_points1=st_drop_geometry(SCBI_subplots_points)
colnames(SCBI_subplots_points1)
colnames(SCBI2022_pppc)

SCBI2022_pppc1=left_join(SCBI2022_pppc, SCBI_subplots_points1[, c(22, 23)]) %>% 
  select(3:10, 26:30) %>% distinct()

ggplot(SCBI2022_pppc1, aes(x=Tpc, y=CHM))+
  geom_point()


