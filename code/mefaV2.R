
# not the most recent

setwd('C:/Users/tsurasinghe/OneDrive - Bridgewater State University/Research2019/invasiveMEFA\mefa1')


library(readr)
library(readxl)
library(magrittr)
library(tidyverse)
library(tibble)
library(janitor)
library(rnpn)
library(googledrive)
library(googlesheets4)
library(data.table)
library(writexl)
library(neonUtilities)
library(neonOS)
library(stringr)
library(stringi)
library(forcats)
library(pivottabler)
library(openxlsx)
library(elevatr)

load(file = "Harv_Scbi_Grsm_plots_allPlantsPerNlcd.RData")
load(file = "div_1m2_NeonPlantsHarv2023_all.RData")
load(file = "PLANTSdurationGrowthHabit.RData")
load(file = "potentialInvasive_1m.RData")
load(file = "neonInvSpeciesPerDuration.RData")
load(file = "neonInvasivePergrowthHabit.RData")
load(file = "plantDivData.RData") # all plant percent cover data (plus woody plant structure) by 2023
load(file = "riparianVegData.RData")
load(file = "plantsTaxTab.RData")
load(file = "div_1m2_div_10m2Data100m2_NeonPlantsHarv2023.RData")
load(file = "div_1m2_div_10m2Data100m2_NeonPlantsSCBI2023.RData")
load(file = "div_1m2_div_10m2Data100m2_NeonPlantsGRSM2023.RData")
load(file = "Harv_Scbi_Grsm_1m_allPlantsNeon2023.RData")
load(file = "Harv_Scbi_Grsm_1m_invasiveNeon2023.RData")
load(file = "Harv_Scbi_Grsm_1m_invasiveNeon2023_wide.RData")
load(file = "Harv_Scbi_Grsm_1m_allPlantsNeon2023_wide.RData")
load(file = "PLANTS_durationHabit.RData")
load(file = "potentialInvasive_10_100m.RData")
load(file = "invasiveSpPerSite_10_100m.RData")
load(file = "Harv_Scbi_Grsm_plots_invPerNlcd.RData")
load(file = "Harv_Scbi_Grsm_1m_plots_allPlantsPerNlcd.RData")
load(file = "Harv_Scbi_Grsm_1m_plots_invPerNlcd.RData")
load(file = "Harv_Scbi_Grsm_1m_allPlants_growthHabitDuration.RData")
load(file = "Harv_Scbi_Grsm_1m_invasive_growthHabitDuration.RData")
load(file = "Harv_Scbi_Grsm_1m_invasivePergrowthHabit.RData")
load(file = "Harv_Scbi_Grsm_1m_allPlantsPergrowthHabit.RData")
load(file = "harv_scbi_grsm_10_100m_allPlants2023.RData")
load(file = "harv_scbi_grsm_10_100m_allPlants2023_habitDuration.RData")
load(file = "harv_scbi_grsm_10_100m_invasive2023_wide.RData")
load(file = "harv_scbi_grsm_10_100m_native2023_wide.RData")
load(file = "harv_scbi_grsm_10_100m_allPlants2023_wide.RData")


# USDA growth habit and duration data to be used as functional traits
PLANTS_duration <- read_csv(file = "USDA_PLANTS_duration.csv")
PLANTS_growthHabit <- read_csv(file = "USDA_PLANTS_growthhabit.csv")
save(PLANTS_duration, PLANTS_growthHabit, file = "PLANTSdurationGrowthHabit.RData")

# join the two above
PLANTS_durationHabit = full_join(PLANTS_duration, PLANTS_growthHabit, copy = FALSE, keep = F, na_matches = "na", 
                                 multiple = "all", relationship = "many-to-many") %>% 
  dplyr::select(-`Synonym Symbol`, -`Common Name`, -smp_name) %>% 
  dplyr::rename('species epithet' = species_name) %>% 
  unite(col = 'binomial name', genus_name, 'species epithet', sep = " ", remove = F) %>% 
  relocate('binomial name', .after = `Scientific Name`) %>% 
  relocate(Duration, .before = growth_habit) %>% 
  janitor::clean_names("lower_camel") %>% 
  distinct()
  
save(PLANTS_durationHabit, file = "PLANTS_durationHabit.RData")

browseURL("https://www.neonscience.org/data-collection/terrestrial-plants")

# Get the names of objects in the list: can be useful to check the tables tacked from neonUtilities pkg
object_names <- names(my_list)

# Print the names and structure of each object
for (name in object_names) {
  print(name)
  print(str(my_list[[name]]))
}


#===================================================================
# access & download neon data
# looking into NEON data to get nativity status and taxonomic details https://data.neonscience.org/data-products/DP1.10058.001
plantsTaxTab = getTaxonList( taxonType = "PLANT", recordReturnLimit = NA, stream = "true", verbose = "false")

save(plantsTaxTab, file = "plantsTaxTab.RData")

# neon data from Plant presence and percent cover
# Plant presence and percent cover (DP1.10058.001)
# Plant species cover-abundance and presence observed in multi-scale plots. 
# Plant species and associated percent cover in 1m2 subplots and plant species presence in 10m2 and 100m2 subplots are reported from 400m2 plots. 
# The presence and percent cover of species is documented in square, multi-scale plots. 
# The presence and percent cover of plant species and ground cover is observed in six 1m2 subplots per plot. 
# The presence of species is observed in six 10m2 subplots and four 100m2 subplots per plot, 
# which can be combined for a list of species at the 400m2 plot scale. 
browseURL("https://data.neonscience.org/data-products/DP1.10058.001")

NEONplants2023 = neonUtilities::loadByProduct( dpID = "DP1.10058.001", site = "all", package = "expanded", release = "current", timeIndex = "all",
                                           tabl = "all", check.size = F,nCores = 1, forceParallel = T)

list2env(NEONplants2023, globalenv())


# Woody plant vegetation structure
WoodyPlantVegStruc2003 = neonUtilities::loadByProduct( dpID = "DP1.10096.001", site = "all", package = "expanded", release = "current", timeIndex = "all",
                                               tabl = "all", check.size = F,nCores = 1, forceParallel = T)
list2env(NEONplants2023, globalenv())

# Riparian composition and structure
riparianVeg2023 = neonUtilities::loadByProduct( dpID = "DP1.20275.001", site = "all", package = "expanded", release = "current", timeIndex = "all",
                                                        tabl = "all", check.size = F,nCores = 1, forceParallel = T)
list2env(riparianVeg2023, globalenv())

# non-herb
nonHerbVegStruc2023 = neonUtilities::loadByProduct( dpID = "DP1.10098.001", site = "all", package = "expanded", release = "current", timeIndex = "all",
                                              tabl = "all", check.size = F,nCores = 1, forceParallel = T)
list2env(nonHerbVegStruc2023, globalenv())

save(div_10m2Data100m2Data, div_1m2Data, NEONplants2023, variables_10058, file = "plantDivData.RData")
save(nonHerbVegStruc2023, file = "riparianVegData.RData")

#================================================================================

# what sites have invasive species?
# "UNK" NA    "NI"  "I"   "A"   "N?"  "I?"  "NI?"
# nativeStatusCode: 'A': Presumed absent, due to lack of data indicating a taxon's presence in a given location; 
# 'N': Native;  # 'N?': Probably Native; 
# 'I': Introduced; 'I?': Probably Introduced; 
# 'NI': Native and Introduced, some infrataxa are native and others are introduced; 
# 'NI?': Probably Native and Introduced, some infrataxa are native and others are introduced; 
# 'UNK': Status unknown.


# screen data with invasive-ish species for 10m and 100 m data for all neon sites 
potentialInvasive_10_100m <- div_10m2Data100m2Data %>% 
  dplyr::filter( is.na(samplingImpractical) | samplingImpractical == "OK" ) %>% 
  dplyr::filter(targetTaxaPresent == "Y" ) %>% 
    dplyr::filter(nativeStatusCode != "N" ) %>% 
  dplyr::select(-geodeticDatum, -coordinateUncertainty, -elevationUncertainty, -samplingProtocolVersion, -targetTaxaPresent, 
                -(identificationQualifier:release))

# only retain ID at species, sub-species, or var ranks  
potentialInvasiveSpSubSp_10_100m = potentialInvasive_10_100m %>% 
  filter(taxonRank %in% c("species", "subspecies", "variety"))

# add binomial names since neon's scientific names are very complex
potentialInvasiveSpSubSp_10_100m = potentialInvasiveSpSubSp_10_100m %>% 
   mutate(NeonbinomialName = word(scientificName, start = 1, end = 2))


# check what species are present in which sites in the wide format for 10m and 100m plots, at neon site scale, regardless of the plots 
potentialInvasiveSpSubSp_10_100m_wide = potentialInvasiveSpSubSp_10_100m %>% 
  dplyr::select(-uid, -decimalLatitude, -decimalLongitude, -taxonID, -taxonRank, -family, -nativeStatusCode, -namedLocation, -plotType, 
                -nlcdClass, -plotID, -subplotID, -endDate, -boutNumber, -eventID) %>% 
  distinct(domainID, siteID, scientificName, .keep_all = T) %>% 
  mutate(presence = ifelse(!is.na(scientificName), 1, 0)) %>% 
  pivot_wider(id_cols = c(domainID, siteID), 
              names_from = scientificName, values_from = presence, values_fill = 0, names_repair = "unique") %>% 
  clean_names()
  
save(potentialInvasiveSpSubSp_10_100m_wide, potentialInvasiveSpSubSp_10_100m, potentialInvasive_10_100m, 
     file = "potentialInvasive_10_100m.RData")


# what sites have invasive species? 47 sites have invasive species
invSitesAll_10_100m = potentialInvasiveSpSubSp_10_100m %>% 
  distinct(domainID, siteID)

# how many sites have invasive species
potentialInvasiveSpSubSp_10_100m %>% 
  distinct(siteID) %>% tally()
# 47 sites

# how many domains have invasive species
potentialInvasiveSpSubSp_10_100m %>% 
  distinct(domainID) %>% tally()
# 20 domains

# how many invasive species or subspecies overall
potentialInvasiveSpSubSp_10_100m %>% 
  distinct(scientificName) %>% tally()
# 677

# how many invasive species or subspecies overall per NEON site
invasiveSpPerSite_10_100m = potentialInvasiveSpSubSp_10_100m %>%
  distinct(domainID, siteID, scientificName) %>% 
 dplyr::group_by(domainID, siteID) %>%
  dplyr::summarise(noInvSpecies = n() )

invasiveSpPerSite_10_100m = potentialInvasiveSpSubSp_10_100m %>%
  distinct(domainID, siteID, scientificName) %>% 
  dplyr::group_by(domainID, siteID) %>%
  tally()

save(invSitesAll_10_100m, invasiveSpPerSite_10_100m, file = "invasiveSpPerSite_10_100m.RData")

# this dataset contains data on invasive plants at the following ranks: species, subspecies, and var-- from all neon sites for 10 and 100 m plots
writexl::write_xlsx(list(invasivePlantsWide = potentialInvasiveSpSubSp_10_100m_wide , 
                         invasivePlantsLong = potentialInvasiveSpSubSp_10_100m ,
                         invasiveSpSubspPerSite = invasiveSpPerSite_10_100m
                         ), 
                    path = "invasiveNEONdata_10_100m.xlsx") # all these are for 10 and 100m2 plots

# exploring the 10 and 100m datasets for the selected neon sites
#clean-up the 10_100m dataset a bit more
harv_scbi_grsm_10_100m_allPlants2023 = bind_rows(div_10m2Data100m2_NeonPlantsHarv2023, div_10m2Data100m2_NeonPlantsSCBI2023, 
                                                 div_10m2Data100m2_NeonPlantsGRSM2023) %>% 
  dplyr::select(-uid, -geodeticDatum, -coordinateUncertainty, -elevation, -elevationUncertainty, -taxonIDRemarks, -morphospeciesID,
                -morphospeciesIDRemarks, -identificationReferences, -identificationHistoryID, -measuredBy, -recordedBy, 
                -samplingImpracticalRemarks, -biophysicalCriteria, -publicationDate, -release) %>%
  dplyr::filter( is.na(samplingImpractical) | samplingImpractical %in% "OK" ) %>%  # remove data where sampling was not done
 filter(taxonRank %in% c("species", "subspecies", "variety")) %>% # filter when tax rank is either sp level or of higher resolution
  filter(targetTaxaPresent %in% "Y") %>%  # keep records with plants
  # # Remove extra white spaces from all columns
  mutate(across(where(is.character), str_squish)) %>%
  # mutate(across(where(is.numeric), \(x) gsub(pattern = "^\\s+|\\s+$", replacement = "", x))) %>% # another way to remove spaces
  mutate(binomialName = str_extract(scientificName, "^\\S+ \\S+")) %>% # extract the first and next non-white space strings. 
  relocate(binomialName, .after = scientificName) 

save(harv_scbi_grsm_10_100m_allPlants2023, file = "harv_scbi_grsm_10_100m_allPlants2023.RData")


# join the functional traits data
harv_scbi_grsm_10_100m_allPlants2023_habitDuration = left_join(x = harv_scbi_grsm_10_100m_allPlants2023, y = PLANTS_durationHabit, 
          by = join_by(taxonID == acceptedSymbol, scientificName), 
          copy = FALSE, keep = F, na_matches = "na", multiple = "all",
          unmatched = "drop", relationship = "many-to-many") 

save(harv_scbi_grsm_10_100m_allPlants2023_habitDuration, file = "harv_scbi_grsm_10_100m_allPlants2023_habitDuration.RData")  


# wide format for 10, 100 m plots
harv_scbi_grsm_10_100m_allPlants2023_wide = harv_scbi_grsm_10_100m_allPlants2023 %>%  
  mutate(presence = ifelse(!is.na(scientificName), 1, 0)) %>% 
  pivot_wider(id_cols = c(namedLocation, domainID, siteID, decimalLatitude, decimalLongitude, plotType, nlcdClass,
                          plotID, subplotID, endDate, boutNumber, eventID), 
              names_from = binomialName, values_from = presence, values_fill = 0, values_fn = sum, names_repair = "unique")

save(harv_scbi_grsm_10_100m_allPlants2023_wide, file = "harv_scbi_grsm_10_100m_allPlants2023_wide.RData")


# natives into wide formats
harv_scbi_grsm_10_100m_native2023_wide = harv_scbi_grsm_10_100m_allPlants2023 %>%  
  filter(nativeStatusCode != "UNK" ) %>% 
  filter(nativeStatusCode == "N" |  nativeStatusCode == "N?") %>% 
  mutate(presence = ifelse(!is.na(scientificName), 1, 0)) %>% 
  pivot_wider(id_cols = c(namedLocation, domainID, siteID, decimalLatitude, decimalLongitude, plotType, nlcdClass,
                          plotID, subplotID, endDate, boutNumber, eventID), 
              names_from = binomialName, values_from = presence, values_fill = 0, values_fn = sum, names_repair = "unique")

save(harv_scbi_grsm_10_100m_native2023_wide, file = "harv_scbi_grsm_10_100m_native2023_wide.RData")


# invasives into wide formats
harv_scbi_grsm_10_100m_invasive2023_wide = harv_scbi_grsm_10_100m_allPlants2023 %>%  
  filter(nativeStatusCode != "UNK" ) %>% 
  filter(nativeStatusCode != "N") %>% 
  mutate(presence = ifelse(!is.na(scientificName), 1, 0)) %>% 
  pivot_wider(id_cols = c(namedLocation, domainID, siteID, decimalLatitude, decimalLongitude, plotType, nlcdClass,
                          plotID, subplotID, endDate, boutNumber, eventID), 
              names_from = binomialName, values_from = presence, values_fill = 0, values_fn = sum, names_repair = "unique")

save(harv_scbi_grsm_10_100m_invasive2023_wide, file = "harv_scbi_grsm_10_100m_invasive2023_wide.RData")




###==========================================================================================================
# 10 and 100m plots do not have percent cover, only presence
# screen data with invasive-ish species for 1 m data
potentialInvasive_1m <- div_1m2Data %>% 
  dplyr::filter( is.na(samplingImpractical) | samplingImpractical == "OK" ) %>% 
  dplyr::filter(targetTaxaPresent == "Y" ) %>% 
  dplyr::filter(nativeStatusCode != "N" ) %>% 
  dplyr::select(-geodeticDatum, -coordinateUncertainty, -elevationUncertainty, -samplingProtocolVersion, -targetTaxaPresent, 
                -(identificationQualifier:release))

# only retain ID at species, sub-species, or var rank rank  
potentialInvasiveSpSubSp_1m = potentialInvasive_1m %>% 
  filter(taxonRank %in% c("species", "subspecies", "variety"))

# check what species are present in which sites
potentialInvasiveSpSubSpWide_1m = potentialInvasiveSpSubSp_1m %>% 
  dplyr::select(-uid, -decimalLatitude, -decimalLongitude, -taxonID, -taxonRank, -family, -nativeStatusCode, -namedLocation, 
                -plotType, -nlcdClass, -plotID, -subplotID, -endDate, -boutNumber, -eventID) %>% 
  distinct(domainID, siteID, scientificName, .keep_all = T) %>% 
  mutate(presence = ifelse(!is.na(scientificName), 1, 0)) %>% 
  pivot_wider(id_cols = c(domainID, siteID), 
              names_from = scientificName, values_from = presence, values_fill = 0, names_repair = "unique") %>% 
  clean_names()

save(potentialInvasiveSpSubSpWide_1m, potentialInvasiveSpSubSp_1m, potentialInvasive_1m, file = "potentialInvasive_1m.RData")


# what sites have invasive species? 47 sites have invasive species
invSitesAll_1m = potentialInvasiveSpSubSp_1m %>% 
  distinct(domainID, siteID)

# how many sites have invasive species
potentialInvasiveSpSubSp_1m %>% 
  distinct(siteID) %>% tally()
# 47 sites

# how many invasive species or subspecies overall
potentialInvasiveSpSubSp_1m %>% 
  distinct(scientificName) %>% tally()
# 677

# how many invasive species or subspecies overall per NEON site
invasiveSpPerSite_1m = potentialInvasiveSpSubSp_1m %>%
  distinct(domainID, siteID, scientificName) %>% 
  dplyr::group_by(domainID, siteID) %>%
  dplyr::summarise(noInvSpecies = n() )

invasiveSpPerSite = potentialInvasiveSpSubSp_1m %>%
  distinct(domainID, siteID, scientificName) %>% 
  dplyr::group_by(domainID, siteID) %>%
  tally()

save(invSitesAll_1m, invasiveSpPerSite_1m, file = "invasiveSpPerSite_1m.RData")

# this dataset includes taxa at species, subspecies, var rank 
writexl::write_xlsx(list(invasivePlantsWide_1m = potentialInvasiveSpSubSpWide_1m , 
                         invasivePlantsLong_1m = potentialInvasiveSpSubSp_1m,
                         invasiveSpSubspPerSite_1m = invasiveSpPerSite_1m
), path = "invasiveNEONdata_1m.xlsx")


# what NEON sites are closest to NPN sites
# spatial proximity  - EPA ecoregion L4
# map neon and NPN sites
# based on land-cover type
# how many species records per site? 
# check out this paper: https://www.fs.usda.gov/research/treesearch/54132


#==========================================================================
# pull neon data for HARV
NEONplantsHarv2023 = neonUtilities::loadByProduct( dpID = "DP1.10058.001", site = "HARV", package = "expanded", release = "current", 
                                                   timeIndex = "all", tabl = "all", check.size = F,nCores = 1, forceParallel = T)

list2env(NEONplantsHarv2023, globalenv())


# pull data for SCBI
NEONplantsSCBI2023 = neonUtilities::loadByProduct( dpID = "DP1.10058.001", site = "SCBI", package = "expanded", release = "current", 
                                                   timeIndex = "all", tabl = "all", check.size = F,nCores = 1, forceParallel = T)

list2env(NEONplantsSCBI2023, globalenv())


# pull data for GRSM
NEONplantsGRSM2023 = neonUtilities::loadByProduct( dpID = "DP1.10058.001", site = "GRSM", package = "expanded", release = "current", 
                                                   timeIndex = "all", tabl = "all", check.size = F,nCores = 1, forceParallel = T)

list2env(NEONplantsGRSM2023, globalenv())
#==========================================================================


# this is the plant cover and presence data for HARV
div_10m2Data100m2_NeonPlantsHarv2023 <- div_10m2Data100m2Data
div_1m2_NeonPlantsHarv2023 <- div_1m2Data


# this is the plant cover and presence data for SCBI
div_10m2Data100m2_NeonPlantsSCBI2023 <- div_10m2Data100m2Data
div_1m2_NeonPlantsSCBI2023 <- div_1m2Data


# this is the plant cover and presence data for GRSM
div_10m2Data100m2_NeonPlantsGRSM2023 <- div_10m2Data100m2Data
div_1m2_NeonPlantsGRSM2023 <- div_1m2Data


# remove white spaces from col headings
names(div_1m2_NeonPlantsHarv2023) <- str_squish(names(div_1m2_NeonPlantsHarv2023))
names(div_1m2_NeonPlantsSCBI2023) <- str_squish(names(div_1m2_NeonPlantsSCBI2023))
names(div_1m2_NeonPlantsGRSM2023) <- str_squish(names(div_1m2_NeonPlantsGRSM2023))


save(div_10m2Data100m2_NeonPlantsHarv2023, div_1m2_NeonPlantsHarv2023, file = "div_1m2_div_10m2Data100m2_NeonPlantsHarv2023.RData")
save(div_10m2Data100m2_NeonPlantsSCBI2023, div_1m2_NeonPlantsSCBI2023, file = "div_1m2_div_10m2Data100m2_NeonPlantsSCBI2023.RData")
save(div_10m2Data100m2_NeonPlantsGRSM2023, div_1m2_NeonPlantsGRSM2023, file = "div_1m2_div_10m2Data100m2_NeonPlantsGRSM2023.RData")



# dive into 1m2 data, the entire, long dataset, the same 3 taxon ranks (species, sub-sp, var), both natives and invasives
# for harv
div_1m2_NeonPlantsHarv2023_all = div_1m2_NeonPlantsHarv2023 %>% 
  dplyr::select(-uid, -geodeticDatum, -coordinateUncertainty, -elevation, -elevationUncertainty, -taxonIDRemarks, -morphospeciesID,
                -morphospeciesIDRemarks, -identificationReferences, -identificationHistoryID, -measuredBy, -recordedBy, 
                -samplingImpracticalRemarks, -biophysicalCriteria, -publicationDate, -release) %>% 
  dplyr::filter( is.na(samplingImpractical) | samplingImpractical %in% "OK" ) %>%  # remove data where sampling was not done
  filter(taxonRank %in% c("species", "subspecies", "variety")) %>% # filter when tax rank is either sp level or of higher resolution
  filter(targetTaxaPresent %in% "Y") %>%  # keep records with plants
 # # Remove extra white spaces from all columns
  mutate(across(where(is.character), str_squish)) %>%
 # mutate(across(where(is.numeric), \(x) gsub(pattern = "^\\s+|\\s+$", replacement = "", x))) %>% # another way to remove spaces
   mutate(binomialName = str_extract(scientificName, "^\\S+ \\S+")) %>% # estract the first and next non-white space strings. 
  relocate(binomialName, .after = scientificName) 

save(div_1m2_NeonPlantsHarv2023_all, file = "div_1m2_NeonPlantsHarv2023_all.RData")


# this is the same as above
# dive into 1m2 data, the entire, long dataset, all taxon ranks, both natives and invasives
# for SCBI
div_1m2_NeonPlantsSCBI2023_all = div_1m2_NeonPlantsSCBI2023 %>% 
  dplyr::select(-uid, -geodeticDatum, -coordinateUncertainty, -elevation, -elevationUncertainty, -taxonIDRemarks, -morphospeciesID,
                -morphospeciesIDRemarks, -identificationReferences, -identificationHistoryID, -measuredBy, -recordedBy, 
                -samplingImpracticalRemarks, -biophysicalCriteria, -publicationDate, -release) %>% 
  dplyr::filter( is.na(samplingImpractical) | samplingImpractical %in% "OK" ) %>%  # remove data where sampling was not done
  filter(taxonRank %in% c("species", "subspecies", "variety")) %>% # filter when tax rank is either sp level or of higher resolution
  filter(targetTaxaPresent %in% "Y") %>%  # keep records with plants
  # # Remove extra white spaces from all columns
  mutate(across(where(is.character), str_squish)) %>%
  # mutate(across(where(is.numeric), \(x) gsub(pattern = "^\\s+|\\s+$", replacement = "", x))) %>% # another way to remove spaces
  mutate(binomialName = str_extract(scientificName, "^\\S+ \\S+")) %>% # estract the first and next non-white space strings. 
  relocate(binomialName, .after = scientificName) 

save(div_1m2_NeonPlantsSCBI2023_all, file = "div_1m2_NeonPlantsSCBI2023_all.RData")


# this is the same as above
# dive into 1m2 data, the entire, long dataset, all taxon ranks, both natives and invasives
# for GRSM
div_1m2_NeonPlantsGRSM2023_all = div_1m2_NeonPlantsGRSM2023 %>% 
  dplyr::select(-uid, -geodeticDatum, -coordinateUncertainty, -elevation, -elevationUncertainty, -taxonIDRemarks, -morphospeciesID,
                -morphospeciesIDRemarks, -identificationReferences, -identificationHistoryID, -measuredBy, -recordedBy, 
                -samplingImpracticalRemarks, -biophysicalCriteria, -publicationDate, -release) %>% 
  dplyr::filter( is.na(samplingImpractical) | samplingImpractical %in% "OK" ) %>%  # remove data where sampling was not done
  filter(taxonRank %in% c("species", "subspecies", "variety")) %>% # filter when tax rank is either sp level or of higher resolution
  filter(targetTaxaPresent %in% "Y") %>%  # keep records with plants
  # # Remove extra white spaces from all columns
  mutate(across(where(is.character), str_squish)) %>%
  # mutate(across(where(is.numeric), \(x) gsub(pattern = "^\\s+|\\s+$", replacement = "", x))) %>% # another way to remove spaces
  mutate(binomialName = str_extract(scientificName, "^\\S+ \\S+")) %>% # estract the first and next non-white space strings. 
  relocate(binomialName, .after = scientificName) 

save(div_1m2_NeonPlantsGRSM2023_all, file = "div_1m2_NeonPlantsGRSM2023_all.RData")


# bind all three locations together
Harv_Scbi_Grsm_1m_allPlantsNeon2023 = bind_rows(div_1m2_NeonPlantsGRSM2023_all, div_1m2_NeonPlantsSCBI2023_all, div_1m2_NeonPlantsHarv2023_all)

save(div_1m2_NeonPlantsGRSM2023_all, div_1m2_NeonPlantsSCBI2023_all, div_1m2_NeonPlantsHarv2023_all, Harv_Scbi_Grsm_1m_allPlantsNeon2023,
          file = "Harv_Scbi_Grsm_1m_allPlantsNeon2023.RData")


# make wide format
Harv_Scbi_Grsm_1m_allPlantsNeon2023_wide = Harv_Scbi_Grsm_1m_allPlantsNeon2023 %>% 
  pivot_wider(id_cols = c(namedLocation, domainID, siteID, decimalLatitude, decimalLongitude, plotType, nlcdClass, plotID, 
                          subplotID, endDate, boutNumber, eventID),
              names_from = binomialName, names_repair = "check_unique", values_from = percentCover, values_fill = 0, 
              values_fn = sum ) %>% 
  group_by(domainID, siteID, namedLocation, nlcdClass, plotID, subplotID, endDate, boutNumber, eventID)

save(Harv_Scbi_Grsm_1m_allPlantsNeon2023_wide, file = "Harv_Scbi_Grsm_1m_allPlantsNeon2023_wide.RData")

# now, let us filter just the invasive species
Harv_Scbi_Grsm_1m_invasiveNeon2023 = Harv_Scbi_Grsm_1m_allPlantsNeon2023 %>% 
  dplyr::filter(nativeStatusCode  != "N" )

# make wide format
Harv_Scbi_Grsm_1m_invasiveNeon2023_wide = Harv_Scbi_Grsm_1m_invasiveNeon2023 %>% 
  pivot_wider(id_cols = c(namedLocation, domainID, siteID, decimalLatitude, decimalLongitude, plotType, nlcdClass, plotID, 
                          subplotID, endDate, boutNumber, eventID),
              names_from = binomialName, names_repair = "check_unique", values_from = percentCover, values_fill = 0, 
              values_fn = sum ) %>% 
  group_by(domainID, siteID, namedLocation, nlcdClass, plotID, subplotID, endDate, boutNumber, eventID)

save(Harv_Scbi_Grsm_1m_invasiveNeon2023, file = "Harv_Scbi_Grsm_1m_invasiveNeon2023.RData")
save(Harv_Scbi_Grsm_1m_invasiveNeon2023_wide, file = "Harv_Scbi_Grsm_1m_invasiveNeon2023_wide.RData")


##=====================================================================
# copying data into excel
# saving these outputs into xl
# Create a blank workbook
# div_1m2_NeonPlantsHarv2023 <- createWorkbook()
Harv_Scbi_Grsm_1m_allNeonData <- createWorkbook()

# Add some sheets to the workbook
addWorksheet(wb = Harv_Scbi_Grsm_1m_allNeonData,
  sheetName = "allSitesInv_long")

addWorksheet(wb = Harv_Scbi_Grsm_1m_allNeonData,
             sheetName = "allSitesInv_wide")

addWorksheet(wb = Harv_Scbi_Grsm_1m_allNeonData,
             sheetName = "allSitesAllPlants_wide")

addWorksheet(wb = Harv_Scbi_Grsm_1m_allNeonData,
             sheetName = "allSitesAllPlants_long")

# Write the data to the sheets of the above wb
writeData(wb = Harv_Scbi_Grsm_1m_allNeonData, # name of the created wb
  sheet = "allSitesInv_long", # name of the sheet
  x = Harv_Scbi_Grsm_1m_invasiveNeon2023, # the df
  startCol = 1, startRow = 1, # starting point
  colNames = TRUE, rowNames = FALSE, 
  headerStyle = createStyle(fontName = NULL, # like Calibri
                            fontSize = 11, fontColour = "black", numFmt = "GENERAL", 
                            border = NULL, borderColour = "black", borderStyle = "thin", 
                            bgFill = NULL, fgFill = NULL, # background and foreground color
                            halign = "center", valign = "center", textDecoration = "bold",
                            wrapText = T, # cell contents will wrap to fit in column.
                            textRotation = NULL, indent = NULL, # Horizontal indentation of cell contents.
                            ),
  borders = "none", borderColour = "black", borderStyle = "thin", # table formats
  withFilter = "T",  # add filter 
  keepNA = FALSE)


# Write the data to the sheets of the above wb
writeData(wb = Harv_Scbi_Grsm_1m_allNeonData, # name of the created wb
          sheet = "allSitesInv_wide", # name of the sheet
          x = Harv_Scbi_Grsm_1m_invasiveNeon2023_wide, # the df
          startCol = 1, startRow = 1, # starting point
          colNames = TRUE, rowNames = FALSE, 
          headerStyle = createStyle(fontName = NULL, # like Calibri
                                    fontSize = 11, fontColour = "black", numFmt = "GENERAL", 
                                    border = NULL, borderColour = "black", borderStyle = "thin", 
                                    bgFill = NULL, fgFill = NULL, # background and foreground color
                                    halign = "center", valign = "center", textDecoration = "bold",
                                    wrapText = T, # cell contents will wrap to fit in column.
                                    textRotation = NULL, indent = NULL, # Horizontal indentation of cell contents.
          ),
          borders = "none", borderColour = "black", borderStyle = "thin", # table formats
          withFilter = "T",  # add filter 
          keepNA = FALSE)


# Write the data to the sheets of the above wb
writeData(wb = Harv_Scbi_Grsm_1m_allNeonData, # name of the created wb
          sheet = "allSitesAllPlants_wide", # name of the sheet
          x = Harv_Scbi_Grsm_1m_allPlantsNeon2023_wide, # the df 
          startCol = 1, startRow = 1, # starting point
          colNames = TRUE, rowNames = FALSE, 
          headerStyle = createStyle(fontName = NULL, # like Calibri
                                    fontSize = 11, fontColour = "black", numFmt = "GENERAL", 
                                    border = NULL, borderColour = "black", borderStyle = "thin", 
                                    bgFill = NULL, fgFill = NULL, # background and foreground color
                                    halign = "center", valign = "center", textDecoration = "bold",
                                    wrapText = T, # cell contents will wrap to fit in column.
                                    textRotation = NULL, indent = NULL, # Horizontal indentation of cell contents.
          ),
          borders = "none", borderColour = "black", borderStyle = "thin", # table formats
          withFilter = "T",  # add filter 
          keepNA = FALSE)


# Write the data to the sheets of the above wb
writeData(wb = Harv_Scbi_Grsm_1m_allNeonData, # name of the created wb
          sheet = "allSitesAllPlants_long", # name of the sheet
          x = Harv_Scbi_Grsm_1m_allPlantsNeon2023, # the df
          startCol = 1, startRow = 1, # starting point
          colNames = TRUE, rowNames = FALSE, 
          headerStyle = createStyle(fontName = NULL, # like Calibri
                                    fontSize = 11, fontColour = "black", numFmt = "GENERAL", 
                                    border = NULL, borderColour = "black", borderStyle = "thin", 
                                    bgFill = NULL, fgFill = NULL, # background and foreground color
                                    halign = "center", valign = "center", textDecoration = "bold",
                                    wrapText = T, # cell contents will wrap to fit in column.
                                    textRotation = NULL, indent = NULL, # Horizontal indentation of cell contents.
          ),
          borders = "none", borderColour = "black", borderStyle = "thin", # table formats
          withFilter = "T",  # add filter 
          keepNA = FALSE)


# Reorder worksheets
# worksheetOrder(OUT) <- c(2,1)

# Export the file, this can be updated
saveWorkbook(wb = Harv_Scbi_Grsm_1m_allNeonData, file = "Harv_Scbi_Grsm_1m_allNeonData.xlsx",
             overwrite = FALSE)
##========================================================================================================

# how many plots are there for each land-cover type for the selected (harv, scbi, grsm) neon sites for invasive species?
Harv_Scbi_Grsm_1m_plots_invPerNlcd = Harv_Scbi_Grsm_1m_invasiveNeon2023 %>% 
  distinct(domainID, siteID, nlcdClass, plotID) %>% 
  group_by(domainID, siteID, nlcdClass) %>% 
  count(sort = T, name = "no_of_plots_inv", .drop = F) %>%  # if FALSE will include counts for empty groups (i.e. for levels of factors that don't exist in the data).
  dplyr::arrange( .by_group = T, desc(no_of_plots_inv) )

# how many subplots are there for each land-cover type for the selected (harv, scbi, grsm) neon sites for all plant species?
Harv_Scbi_Grsm_1m_subplots_invPerNlcd = Harv_Scbi_Grsm_1m_invasiveNeon2023 %>% 
  distinct(domainID, siteID, nlcdClass, plotID, subplotID) %>% 
  group_by(domainID, siteID, nlcdClass) %>% 
  count(sort = T, name = "no_of_subplots_inv", .drop = F) %>%  # if FALSE will include counts for empty groups (i.e. for levels of factors that don't exist in the data).
  dplyr::arrange( .by_group = T, desc(no_of_subplots_inv) )

save(Harv_Scbi_Grsm_1m_plots_invPerNlcd, Harv_Scbi_Grsm_1m_subplots_invPerNlcd, file = "Harv_Scbi_Grsm_1m_plots_invPerNlcd.RData")


# how many plots are there for each land-cover type for the selected (harv, scbi, grsm) neon sites for ALL invasive and native species?
Harv_Scbi_Grsm_1m_plots_allPlantsPerNlcd = Harv_Scbi_Grsm_1m_allPlantsNeon2023 %>% 
  distinct(domainID, siteID, nlcdClass, plotID) %>% 
  group_by(domainID, siteID, nlcdClass) %>% 
  count(sort = T, name = "no_of_plots_allPlants", .drop = F) %>%  # if FALSE will include counts for empty groups (i.e. for levels of factors that don't exist in the data).
  dplyr::arrange( .by_group = T, desc(no_of_plots_allPlants) )
Harv_Scbi_Grsm_1m_plots_allPlantsPerNlcd

# how many subplots are there for each land-cover type for the selected (harv, scbi, grsm) neon sites for all plant species?
Harv_Scbi_Grsm_1m_subplots_allPlantsPerNlcd = Harv_Scbi_Grsm_1m_allPlantsNeon2023 %>% 
  distinct(domainID, siteID, nlcdClass, plotID, subplotID) %>% 
  group_by(domainID, siteID, nlcdClass) %>% 
  count(sort = T, name = "no_of_subplots_allPlants", .drop = F) %>%  # if FALSE will include counts for empty groups (i.e. for levels of factors that don't exist in the data).
  dplyr::arrange( .by_group = T, desc(no_of_subplots_allPlants) )
Harv_Scbi_Grsm_1m_subplots_allPlantsPerNlcd

save(Harv_Scbi_Grsm_1m_plots_allPlantsPerNlcd, Harv_Scbi_Grsm_1m_subplots_allPlantsPerNlcd, file = "Harv_Scbi_Grsm_1m_plots_allPlantsPerNlcd.RData")


# join inv and all plants to compare plot distribution
Harv_Scbi_Grsm_1m_plots_inv_allPlants_PerNlcd = left_join(Harv_Scbi_Grsm_1m_plots_allPlantsPerNlcd, Harv_Scbi_Grsm_1m_plots_invPerNlcd )

# this dataset includes taxa at species, subspecies, var rank
writexl::write_xlsx(list(inv_allPlants_PerNlcd = Harv_Scbi_Grsm_1m_plots_inv_allPlants_PerNlcd,
                         plots_allPlants_Nlcd = Harv_Scbi_Grsm_1m_plots_allPlantsPerNlcd, 
                         subpl_allPlants_Nlcd = Harv_Scbi_Grsm_1m_subplots_allPlantsPerNlcd, 
                         plots_inv_Nlcd = Harv_Scbi_Grsm_1m_plots_invPerNlcd, 
                         subpl_inv_Nlcd = Harv_Scbi_Grsm_1m_subplots_invPerNlcd), 
                    path = "plotsPerNLDS_nativeInv_harv_scbi_grsm_1m.xlsx")

# join the growth habit and duration to neon inv data from the selected sites
Harv_Scbi_Grsm_1m_invasive_growthHabitDuration = left_join(x = Harv_Scbi_Grsm_1m_invasiveNeon2023, y = PLANTS_durationHabit, 
                                             by = join_by(taxonID == acceptedSymbol, scientificName),
                                             copy = FALSE, keep = F, na_matches = "na", multiple = "all", 
                                             unmatched = "drop", relationship = "many-to-many") %>% 
  mutate(binomialName = word(scientificName, start = 1, end = 2) )

save(Harv_Scbi_Grsm_1m_invasive_growthHabitDuration, file = "Harv_Scbi_Grsm_1m_invasive_growthHabitDuration.RData")


# join the growth habit and duration to neon all plants data from the selected sites
Harv_Scbi_Grsm_1m_allPlants_growthHabitDuration = left_join(x = Harv_Scbi_Grsm_1m_allPlantsNeon2023 , y = PLANTS_durationHabit, 
                                                           by = join_by(taxonID == acceptedSymbol, scientificName),
                                                           copy = FALSE, keep = F, na_matches = "na", multiple = "all", 
                                                           unmatched = "drop", relationship = "many-to-many") %>% 
  mutate(binomialName = word(scientificName, start = 1, end = 2) )

save(Harv_Scbi_Grsm_1m_allPlants_growthHabitDuration, file = "Harv_Scbi_Grsm_1m_allPlants_growthHabitDuration.RData")

# this dataset includes taxa at species, subspecies, var rank
writexl::write_xlsx(list(allPlants_1m_HabitDuration = Harv_Scbi_Grsm_1m_allPlants_growthHabitDuration, 
                         invasive_1m_HabitDuration = Harv_Scbi_Grsm_1m_invasive_growthHabitDuration), 
                    path = "Harv_Scbi_Grsm_1m_invasiveNative_growthHabitDuration.xlsx")


# what is the representation of growth habits among the selected sites for 1m plots, all plant species included?
Harv_Scbi_Grsm_1m_allPlantsPergrowthHabit = Harv_Scbi_Grsm_1m_allPlants_growthHabitDuration %>%  
  dplyr::select(domainID, siteID, growthHabit, binomialName) %>% 
  distinct(domainID, siteID, growthHabit, binomialName) %>% 
  group_by(domainID, siteID, growthHabit) %>% 
  summarise(no_speciesPerHabit = n()) %>% 
  dplyr::arrange( .by_group = T, desc(no_speciesPerHabit) )

save(Harv_Scbi_Grsm_1m_allPlantsPergrowthHabit, file = "Harv_Scbi_Grsm_1m_allPlantsPergrowthHabit.RData")


# what is the representation of growth habits of the invasive sp among the selected sites for 1m plots
Harv_Scbi_Grsm_1m_invasivePergrowthHabit = Harv_Scbi_Grsm_1m_invasive_growthHabitDuration %>%  
  dplyr::select(domainID, siteID, growthHabit, binomialName) %>% 
  distinct(domainID, siteID, growthHabit, binomialName) %>% 
  group_by(domainID, siteID, growthHabit) %>% 
  summarise(no_speciesPerHabit = n()) %>% 
  dplyr::arrange( .by_group = T, desc(no_speciesPerHabit) )

save(Harv_Scbi_Grsm_1m_invasivePergrowthHabit, file = "Harv_Scbi_Grsm_1m_invasivePergrowthHabit.RData")


# what is the representation of duration among the selected sites for 1m plots, all plant species included?
Harv_Scbi_Grsm_1m_allPlantsPerDuration = Harv_Scbi_Grsm_1m_allPlants_growthHabitDuration %>%  
  dplyr::select(domainID, siteID, duration, binomialName) %>% 
  distinct(domainID, siteID, duration, binomialName) %>% 
  group_by(domainID, siteID, duration) %>% 
  summarise(no_speciesPerDuration = n()) %>% 
  dplyr::arrange( .by_group = T, desc(no_speciesPerDuration) )

save(Harv_Scbi_Grsm_1m_allPlantsPerDuration, file = "Harv_Scbi_Grsm_1m_allPlantsPerDuration.RData")


# what is the representation of duration of the invasive sp among the selected sites for 1m plots
Harv_Scbi_Grsm_1m_invasvePerDuration = Harv_Scbi_Grsm_1m_invasive_growthHabitDuration %>%  
  dplyr::select(domainID, siteID, duration, binomialName) %>% 
  distinct(domainID, siteID, duration, binomialName) %>% 
  group_by(domainID, siteID, duration) %>% 
  summarise(no_speciesPerDuration = n()) %>% 
  dplyr::arrange( .by_group = T, desc(no_speciesPerDuration) )

save(Harv_Scbi_Grsm_1m_invasvePerDuration, file = "Harv_Scbi_Grsm_1m_invasvePerDuration.RData")


# this dataset includes taxa at species, subspecies, var rank
writexl::write_xlsx(list(invasive_1m_PerDuration = Harv_Scbi_Grsm_1m_invasvePerDuration,
                         allPlants_1m_PerDuration = Harv_Scbi_Grsm_1m_allPlantsPerDuration,
                         invasive_1m_PerHabit = Harv_Scbi_Grsm_1m_invasivePergrowthHabit,
                         allPlants_1m_PerHabit = Harv_Scbi_Grsm_1m_allPlantsPergrowthHabit), 
                    path = "Harv_Scbi_Grsm_1m_invasiveNative_growthHabit_Duration.xlsx")
