
library(tidyverse)
library(neonUtilities)
library(sf)
library(terra)
library(rasterVis)
library(tidyterra)


# load NEON sites spatial data
# V11
plot_polygons <- read_sf("C:/Users/xiey2/Documents/Data/NEON/All_NEON_TOS_Plots_V11/All_NEON_TOS_Plot_Polygons_V11.shp",  quiet = T)
plot_polygons
colnames(plot_polygons)

plot_centroids <- read_sf("C:/Users/xiey2/Documents/Data/NEON/All_NEON_TOS_Plots_V11/All_NEON_TOS_Plot_Centroids_V11.shp",  quiet = T)
plot_centroids
colnames(plot_centroids)

plot_points <- read_sf("C:/Users/xiey2/Documents/Data/NEON/All_NEON_TOS_Plots_V11/All_NEON_TOS_Plot_Points_V11.shp",  quiet = T)
plot_points
colnames(plot_points)

subplots <- read_sf("C:/Users/xiey2/Documents/Data/NEON/All_NEON_TOS_Plots_V11/All_NEON_TOS_Plot_Subplots_V11.shp",  quiet = T)
subplots
colnames(subplots)

# V10
plot_points_v10 <- read_sf("C:/Users/xiey2/Documents/Data/NEON/All_NEON_TOS_Plots_V10/All_NEON_TOS_Plot_Points_V10.shp",  quiet = T)
plot_points_v10
colnames(plot_points_v10)

plot_polygons_v10 <- read_sf("C:/Users/xiey2/Documents/Data/NEON/All_NEON_TOS_Plots_V10/All_NEON_TOS_Plot_Polygons_V10.shp",  quiet = T)


# download AOP data through byTileAOP ---------------------------------------------
# set product
# canopy height DP3.30015.001
product = "DP3.30015.001"

# set sites
sites=c("HARV","GRSM","SCBI")

# set years
years=c("2021","2022","2023")


# all files for the site and year
byFileAOP(dpID = product, site="SCBI", year="2022", check.size = T,
          savepath="C:/Users/xiey2/Documents/Data/NEON")


# # get plot centroids for one site
# SCBI_p=filter(plot_centroids,siteID=="SCBI")
# 
# # all plots at one site, sets of coordinates
# byTileAOP(dpID = product, site=sites[3], year="2022", check.size = T,
#           easting=SCBI_p$easting, northing=SCBI_p$northing, buffer=20,
#           savepath="C:/Users/xiey2/Documents/Data/NEON")
# 
# 
# # all plots at all sites
# plots = filter(plot_centroids, siteID %in% sites) %>% 
#   st_set_geometry(NULL) 
# 
# # all plots at multiple sites, one year
# for (i in 1:nrow(plot))
# {
#   byTileAOP(dpID = product, site=plot$siteID[i], year="2022", check.size = T,
#             easting=plots$easting[i], northing=plots$northing[i], 
#             savepath="C:/Users/xiey2/Documents/Data/NEON")
# }
# 
# 
# # all plots at multiple sites and years
# for (j in 1:length(years))
# {
#   for (i in 1:nrow(plot))
#   {
#     byTileAOP(dpID = product, site=plots$siteID[i], year=years[j], check.size = T,
#               easting=plots$easting[i], northing=plots$northing[i], 
#               savepath="C:/Users/xiey2/Documents/Data/NEON")
#   }
# }



# process AOP raster data ---------------------------------------------------------------------
# read data for one site one year (SCBI 2022)
dir_SCBI2022="C:/Users/xiey2/Documents/Data/NEON/DP3.30015.001/neon-aop-products/2022/FullSite/D02/2022_SCBI_5/L3/DiscreteLidar/CanopyHeightModelGtif/"
r = list.files(dir_SCBI2022, pattern = "NEON_D02_SCBI_DP3_*", full.names = TRUE)

r <- lapply(r, rast)
r <- do.call(merge, r)

scbi_2022=r

plot(scbi_2022)


# plot one basePlot as a test  SCBI_002
SCBI_002_p=filter(plot_polygons, plotID=="SCBI_002", subtype=="basePlot")
SCBI_002_p=st_transform(SCBI_002_p, st_crs(scbi_2022))

SCBI_002_2022=crop(scbi_2022, SCBI_002_p)            # plot SCBI_002
SCBI_002_points=plot_points %>% filter(plotID=="SCBI_002", subtype=="basePlot")       # points at plot SCBI_002
SCBI_002_points=st_transform(SCBI_002_points, st_crs(scbi_2022))

# V11
SCBI_002_subplots=subplots %>% filter(plotID=="SCBI_002", subtype=="basePlot")       # points at plot SCBI_002
SCBI_002_subplots=st_transform(SCBI_002_subplots, st_crs(scbi_2022))


ggplot()+
  geom_spatraster(data = SCBI_002_2022, aes(fill=NEON_D02_SCBI_DP3_741000_4305000_CHM))+
  scale_fill_gradientn(
    colours=c("orange","yellow","green","darkgreen"),
    name="Canopy Height")+
  geom_sf(data=SCBI_002_p,fill=NA,color="black", linewidth=1)+
  geom_sf(data=SCBI_002_points,color="red")+
  geom_sf_text(data=SCBI_002_points,
               aes(label=pointID)  )+
  geom_sf(data=SCBI_002_subplots,color="blue")+
  geom_sf_text(data=SCBI_002_subplots,
               aes(label=subplotID)  )+
  xlab("longitude")+ylab("latitude")


# V10
SCBI_002_points=plot_points_v10 %>% filter(plotID=="SCBI_002", subtype=="basePlot")       # points at plot SCBI_002
SCBI_002_points=st_transform(SCBI_002_points, st_crs(scbi_2022))


ggplot()+
  geom_spatraster(data = SCBI_002_2022, aes(fill=NEON_D02_SCBI_DP3_741000_4305000_CHM))+
  scale_fill_gradientn(
    colours=c("orange","yellow","green","darkgreen"),
    name="Canopy Height")+
  geom_sf(data=SCBI_002_p,fill=NA,color="black", linewidth=1)+
  geom_sf(data=SCBI_002_points,color="red")+
  geom_sf_text(data=SCBI_002_points,
               aes(label=pointID)  )+
  xlab("longitude")+ylab("latitude")



# get all subplot points in SCBI
SCBI_points=plot_points_v10 %>% filter(subtype=="basePlot")       # points at baseplot of SCBI
SCBI_points=st_transform(SCBI_points, st_crs(scbi_2022))

# get canopy height for all points
chm = extract(scbi_2022, SCBI_points)
SCBI_points$CHM= chm[, 2]


# load processed VPPP data
load("processedData/Harv_Scbi_Grsm_invasiveNeon2023.RData")

glimpse(Harv_Scbi_Grsm_invasiveNeon2023)

Harv_Scbi_Grsm_invasiveNeon2023$year=year(Harv_Scbi_Grsm_invasiveNeon2023$endDate)
Harv_Scbi_Grsm_invasiveNeon2023$plot=paste0(Harv_Scbi_Grsm_invasiveNeon2023$plotID,"_",Harv_Scbi_Grsm_invasiveNeon2023$subplotID, sep="")


# VPPP invasive SCBI in 2022
SCBI_in=Harv_Scbi_Grsm_invasiveNeon2023 %>% filter(siteID=="SCBI", year==2022)
unique(SCBI_in$subplotID)

# match subplot between pointID and subplotID, then merge data
for (i in 1:nrow(SCBI_in))
{
  SCBI_in$pointID= paste0(substr(SCBI_in$subplotID, 1, 2), ".", substr(SCBI_in$subplotID, 6, 6), ".", substr(SCBI_in$subplotID, 4, 4))
}

SCBI_points=st_drop_geometry(SCBI_points)
colnames(SCBI_points)
SCBI_in1=left_join(SCBI_in, SCBI_points[, c(1:2, 7:10, 13, 18)])


# make plots
ggplot(SCBI_in1, aes(x=percentCover, y=CHM, color=taxonID))+
  geom_point()

ggplot(SCBI_in1, aes(x=percentCover, y=CHM, color=binomialName))+
  geom_point()+
  facet_wrap(~binomialName, ncol=4)+
  theme(legend.position = "none")


# calculate total percent cover of invasives
unique(SCBI_in1$plot)
SCBI_in1_t = SCBI_in1 %>% group_by(plot) %>% summarise(Tpc=sum(percentCover), n=n())
SCBI_in1=left_join(SCBI_in1, SCBI_in1_t)


ggplot(SCBI_in1, aes(x=Tpc, y=CHM))+
  geom_point()
