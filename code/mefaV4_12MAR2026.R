

# setwd('C:/Users/tsurasinghe/OneDrive - Bridgewater State University/Research2019/invasiveMEFA\mefaInv2025Jul')

here::here("invasiveMEFA", "mefaInv2025Jul")

list.files(here("2026Mar_download"), pattern = "\\.RData$")

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
library(here)

load(file = "PLANTS_durationHabit.RData")
load(file = "PLANTSdurationGrowthHabit.RData")
load(file = here("data", "plantsTaxTab.RData"))
# load(file = "potentialInvasive_10_100m.RData") # all neon plant presence and percent cover data from all sites at 10 and 100 m2 scale, invasive only
# load(file = "invasiveSpPerSite_10_100m.RData") # invasive species per neon site
# load(file = "div_1m2_div_10m2Data100m2_NeonPlantsHarv2025.RData") # unprocessed raw dataset from neon api for selected neon sites 
# load(file = "div_1m2_div_10m2Data100m2_NeonPlantsSCBI2025.RData")
# load(file = "div_1m2_div_10m2Data100m2_NeonPlantsGRSM2025.RData")
# load(file = here("data", "harv_scbi_grsm_10_100m_allPlants2025.RData")) # unprocessed data from selected neon sites, includes both native and non-natives
# load(file = here("data", "harv_scbi_grsm_10_100m_allPlants2025_habitDuration.RData"))  # above dataset with trait data
# load(file = here("data", "Harv_Scbi_Grsm_1m_allPlantsNeon2025.RData"))
# load(file = here("data", "Harv_Scbi_Grsm_1m_allPlantsNeon2025_wide.RData"))


load(here("2026Mar_download", "div_1m2_allNeonSites_plantDiv.RData")) # PPPC 1m, plant only, all neon sites, long version
load(here("2026Mar_download", "div_1m2_NeonPlants2026.RData")) # PPPC 1m, plant and other vars, all neon sites
load(here("2026Mar_download", "plantDivData.RData")) # PPPC data for 10m2, 100m2, 1m2: div_10m2Data100m2Data, div_1m2Data, 
load(here("2026Mar_download", "NEONplants2025.RData")) # all PPPC data for all terrestrial sites 
load(here("2026Mar_download", "div_1m2_allNeonSites_plantDiv_wide.RData")) # wide version of div_1m2_allNeonSites_plantDiv.RData
load(here("2026Mar_download", "div_1m2_allNeonSites_Plants_growthHabitDuration.RData")) # the long format with habit & duration


# USDA growth habit and duration data to be used as functional traits
PLANTS_duration <- read_csv(here("data", "USDA_PLANTS_duration.csv"))
PLANTS_growthHabit <- read_csv(here("data", "USDA_PLANTS_growthhabit.csv"))

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
save(PLANTS_duration, PLANTS_growthHabit, file = "PLANTSdurationGrowthHabit.RData")

browseURL("https://www.neonscience.org/data-collection/terrestrial-plants")


#===========================================================================================================
# Get the names of objects in the list: can be useful to check the tables tacked from neonUtilities pkg
object_names <- names(my_list)

# Print the names and structure of each object
for (name in object_names) {
  print(name)
  print(str(my_list[[name]]))
}
#===========================================================================================================


#===================================================================
# access & download neon data
# looking into NEON data to get nativity status and taxonomic details https://data.neonscience.org/data-products/DP1.10058.001
plantsTaxTab = getTaxonList( taxonType = "PLANT", recordReturnLimit = NA, stream = "true", verbose = "false")

save(plantsTaxTab, file = here("data", "plantsTaxTab.RData"))

# neon data from Plant presence and percent cover
# Plant presence and percent cover (DP1.10058.001)
# Plant species cover-abundance and presence observed in multi-scale plots. 
# Plant species and associated percent cover in 1m2 subplots and plant species presence in 10m2 and 100m2 subplots are reported from 400m2 plots. 
# The presence and percent cover of species is documented in square, multi-scale plots. 
# The presence and percent cover of plant species and ground cover is observed in six 1m2 subplots per plot. 
# The presence of species is observed in six 10m2 subplots and four 100m2 subplots per plot, 
# which can be combined for a list of species at the 400m2 plot scale. 
browseURL("https://data.neonscience.org/data-products/DP1.10058.001")

myPlantsTocken <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJ0c3VyYXNpbmdoZUBicmlkZ2V3LmVkdSIsInNjb3BlIjoicmF0ZTpwdWJsaWMiLCJpc3MiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnLyIsImV4cCI6MTkxMTY1NDYyMywiaWF0IjoxNzUzOTc0NjIzLCJlbWFpbCI6InRzdXJhc2luZ2hlQGJyaWRnZXcuZWR1In0.ZEonFGxXjd3jY7Azk28c9L_pq_CgKR--LuhLf6ZO2izUO7_eZHIE4XA_zyqeF0gmQ4aSCbTqLfPcL7GnBo3zrg"
neonUtilities::tokenDate(myPlantsTocken)

# accessed on JUL 31, 2025, # provistional data, i.e., data w/o QC are excluded
# this is Plant presence and percent cover, aka PPPC
NEONplants2025 = neonUtilities::loadByProduct( dpID = "DP1.10058.001", site = "all", 
                                               package = "expanded", release = "current", 
                                               timeIndex = "all", token  = myPlantsTocken,
                                           tabl = "all", check.size = F,nCores = 1, forceParallel = T)

list2env(NEONplants2025, globalenv())

dir.create(here("2026Mar_download"))

save(div_10m2Data100m2Data, div_1m2Data, variables_10058, readme_10058, 
     file = here("2026Mar_download", "plantDivData.RData"))

save(NEONplants2025, file =  here("2026Mar_download", "NEONplants2025.RData"))

# Write the vector to a text file called 'output.txt' in the current working directory
# Each element of the vector will be written on a new line
write_lines(x = readme_10058, file = here("data", "readme_10058.txt"), sep = "\n", na = "NA", append = FALSE)
write_csv(x = variables_10058, file = here("data", "variables_10058.csv"))


# Woody plant vegetation structure
WoodyPlantVegStruc2023 = neonUtilities::loadByProduct( dpID = "DP1.10096.001", site = "all", package = "expanded", release = "current", timeIndex = "all",
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

# save(div_10m2Data100m2Data, div_1m2Data, NEONplants2023, variables_10058, file = "plantDivData.RData")


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
  dplyr::select(-geodeticDatum, -coordinateUncertainty, -elevationUncertainty, -identificationQualifier, -taxonIDRemarks, 
                -(morphospeciesIDRemarks:identificationHistoryID), -(remarks:recordedBy), -(samplingImpracticalRemarks:release))

###==================================================
# FYI: there are several versions of protocols:
# "NEON.DOC.014042vG" "NEON.DOC.014042vJ" "NEON.DOC.014042vL" "NEON.DOC.014042vM" "NEON.DOC.014042vK" "NEON.DOC.014042vC" "NEON.DOC.014042vD" "NEON.DOC.014042vE" "NEON.DOC.014042vF"
###==================================================


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


# check what species are present in which sites in the wide format for 10m and 100m plots, at neon site scale, per plot 
potentialInvasiveSpSubSp_10_100m_wide_plotScale = potentialInvasiveSpSubSp_10_100m %>% 
  dplyr::select(-uid, -decimalLatitude, -decimalLongitude, -taxonID, -taxonRank, -family, -nativeStatusCode, -namedLocation, -endDate, -boutNumber, -eventID) %>% 
  distinct(domainID, siteID, scientificName, .keep_all = T) %>% 
  mutate(presence = ifelse(!is.na(scientificName), 1, 0)) %>% 
  pivot_wider(id_cols = c(domainID, siteID, nlcdClass, plotType, plotID, subplotID), 
              names_from = scientificName, values_from = presence, values_fill = 0, names_repair = "unique") %>% 
  clean_names()

  
save(potentialInvasiveSpSubSp_10_100m_wide, potentialInvasiveSpSubSp_10_100m, potentialInvasive_10_100m, potentialInvasiveSpSubSp_10_100m_wide_plotScale,
     file = here("2026Mar_download", "potentialInvasive_10_100m.RData"))


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

# save(invSitesAll_10_100m, invasiveSpPerSite_10_100m, file = "invasiveSpPerSite_10_100m.RData")

save(invasiveSpPerSite_10_100m, file = here("2026Mar_download", "invasiveSpPerSite_10_100m.RData"))


# this dataset contains data on invasive plants at the following ranks: species, subspecies, and var-- from all neon sites for 10 and 100 m plots
# writexl::write_xlsx(list(invasivePlantsWide = potentialInvasiveSpSubSp_10_100m_wide , 
#                          invasivePlants_scale_Wide = potentialInvasiveSpSubSp_10_100m_wide_plotScale,
#                          invasivePlantsLong = potentialInvasiveSpSubSp_10_100m ,
#                          invasiveSpSubspPerSite = invasiveSpPerSite_10_100m
#                          ), 
#                     path = here("results", "invasiveNEONdata_10_100m.xlsx") ) # all these are for 10 and 100m2 plots
# 

#==========================================================================

# accessed on AUG 2, 2025, # provisional data, i.e., data w/o QC are excluded

# # token: 
# myPlantsTocken <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJ0c3VyYXNpbmdoZUBicmlkZ2V3LmVkdSIsInNjb3BlIjoicmF0ZTpwdWJsaWMiLCJpc3MiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnLyIsImV4cCI6MTkxMTY1NDYyMywiaWF0IjoxNzUzOTc0NjIzLCJlbWFpbCI6InRzdXJhc2luZ2hlQGJyaWRnZXcuZWR1In0.ZEonFGxXjd3jY7Azk28c9L_pq_CgKR--LuhLf6ZO2izUO7_eZHIE4XA_zyqeF0gmQ4aSCbTqLfPcL7GnBo3zrg"
# 
# 
# # pull neon data for HARV
# NEONplantsHarv2025 = neonUtilities::loadByProduct( dpID = "DP1.10058.001", site = "HARV", package = "expanded", release = "current", 
#                                                    timeIndex = "all", tabl = "all", check.size = F,nCores = 1, forceParallel = T, token  = myPlantsTocken)
# 
# list2env(NEONplantsHarv2025, globalenv())
# 
# # this is the plant cover and presence data for HARV
# div_10m2Data100m2_NeonPlantsHarv2025 <- div_10m2Data100m2Data
# div_1m2_NeonPlantsHarv2025 <- div_1m2Data
# 
# 
# # pull data for SCBI
# NEONplantsSCBI2025 = neonUtilities::loadByProduct( dpID = "DP1.10058.001", site = "SCBI", package = "expanded", release = "current", 
#                                                    timeIndex = "all", tabl = "all", check.size = F,nCores = 1, forceParallel = T, token  = myPlantsTocken)
# 
# list2env(NEONplantsSCBI2025, globalenv())
# 
# # this is the plant cover and presence data for SCBI
# div_10m2Data100m2_NeonPlantsSCBI2025 <- div_10m2Data100m2Data
# div_1m2_NeonPlantsSCBI2025 <- div_1m2Data
# 
# 
# # pull data for GRSM
# NEONplantsGRSM2025 = neonUtilities::loadByProduct( dpID = "DP1.10058.001", site = "GRSM", package = "expanded", release = "current", 
#                                                    timeIndex = "all", tabl = "all", check.size = F,nCores = 1, forceParallel = T, token  = myPlantsTocken)
# 
# list2env(NEONplantsGRSM2025, globalenv())
# 
# # this is the plant cover and presence data for GRSM
# div_10m2Data100m2_NeonPlantsGRSM2025 <- div_10m2Data100m2Data
# div_1m2_NeonPlantsGRSM2025 <- div_1m2Data
# 
# 
# # remove white spaces from col headings
# names(div_1m2_NeonPlantsHarv2025) <- str_squish(names(div_1m2_NeonPlantsHarv2025))
# names(div_1m2_NeonPlantsSCBI2025) <- str_squish(names(div_1m2_NeonPlantsSCBI2025))
# names(div_1m2_NeonPlantsGRSM2025) <- str_squish(names(div_1m2_NeonPlantsGRSM2025))
# 
# 
# save(div_10m2Data100m2_NeonPlantsHarv2025, div_1m2_NeonPlantsHarv2025, file = "div_1m2_div_10m2Data100m2_NeonPlantsHarv2025.RData")
# save(div_10m2Data100m2_NeonPlantsSCBI2025, div_1m2_NeonPlantsSCBI2025, file = "div_1m2_div_10m2Data100m2_NeonPlantsSCBI2025.RData")
# save(div_10m2Data100m2_NeonPlantsGRSM2025, div_1m2_NeonPlantsGRSM2025, file = "div_1m2_div_10m2Data100m2_NeonPlantsGRSM2025.RData")
# 


#==========================================================================

### NOTE
# at this stage we are not worried about the accuracy of species ID, 
# in future, something to consider
# harv_scbi_grsm_10_100m_allPlants2025$identificationQualifier %>% unique()
# [1] NA               "cf. family"     "cf. species"    "cf. genus"      "cf. variety"    "aff. species"   "aff. family"    "aff. genus"     "cf. subspecies"


# exploring the 10 and 100m datasets for the selected neon sites
#clean-up the 10_100m dataset a bit more
# harv_scbi_grsm_10_100m_allPlants2025 = bind_rows(div_10m2Data100m2_NeonPlantsHarv2025, div_10m2Data100m2_NeonPlantsSCBI2025, 
#                                                  div_10m2Data100m2_NeonPlantsGRSM2025) %>% 
#   dplyr::select(-uid, -geodeticDatum, -coordinateUncertainty, -elevationUncertainty, -taxonIDRemarks, -morphospeciesID,
#                 -morphospeciesIDRemarks, -identificationReferences, -identificationHistoryID, -measuredBy, -recordedBy, 
#                 -samplingImpracticalRemarks, -biophysicalCriteria, -publicationDate, -release, -identificationQualifier, -additionalSpecies, -remarks) %>%
#   dplyr::filter( is.na(samplingImpractical) | samplingImpractical %in% "OK" ) %>%  # remove data where sampling was not done
#  dplyr::select(-samplingImpractical) %>% # now, we do not need this col
#   filter(taxonRank %in% c("species", "subspecies", "variety")) %>% # filter when tax rank is either sp level or of higher resolution
# #  filter(targetTaxaPresent %in% "Y") %>%  # keep records with plants, I skip this since i wanted zero data in the code
#   # # Remove extra white spaces from all columns
#   mutate(across(where(is.character), str_squish)) %>%
#   # mutate(across(where(is.numeric), \(x) gsub(pattern = "^\\s+|\\s+$", replacement = "", x))) %>% # another way to remove spaces
#   mutate(binomialName = str_extract(scientificName, "^\\S+ \\S+")) %>% # extract the first and next non-white space strings. 
#   relocate(binomialName, .after = scientificName) 
# 
# save(harv_scbi_grsm_10_100m_allPlants2025, file = here("data", "harv_scbi_grsm_10_100m_allPlants2025.RData"))
# 
# 
# # join the functional traits data
# harv_scbi_grsm_10_100m_allPlants2025_habitDuration = left_join(x = harv_scbi_grsm_10_100m_allPlants2025, y = PLANTS_durationHabit, 
#           by = join_by(taxonID == acceptedSymbol, scientificName), 
#           copy = FALSE, keep = F, na_matches = "na", multiple = "all",
#           unmatched = "drop", relationship = "many-to-many") 
# 
# save(harv_scbi_grsm_10_100m_allPlants2025_habitDuration, file = here("data", "harv_scbi_grsm_10_100m_allPlants2025_habitDuration.RData"))  
# 
# 
# # wide format for 10, 100 m plots
# harv_scbi_grsm_10_100m_allPlants2025_wide = harv_scbi_grsm_10_100m_allPlants2025 %>%  
#   mutate(presence = ifelse(!is.na(scientificName), 1, 0)) %>% 
#   pivot_wider(id_cols = c(namedLocation, domainID, siteID, decimalLatitude, decimalLongitude, plotType, nlcdClass,
#                           plotID, subplotID, endDate, boutNumber, eventID), 
#               names_from = binomialName, values_from = presence, values_fill = 0, values_fn = sum, names_repair = "unique")
# 
# save(harv_scbi_grsm_10_100m_allPlants2025_wide, file = here("data", "harv_scbi_grsm_10_100m_allPlants2025_wide.RData"))



###==========================================================================================================
# 10 and 100m plots do not have percent cover, only presence
# screen data with invasive-ish species for 1 m data

# dive into 1m2 data, the entire, long dataset, the same 3 taxon ranks (species, sub-sp, var), both natives and invasive
# all neon sites, accessed mar 12, 2026
div_1m2_NeonPlants2026 = div_1m2Data %>% 
  dplyr::select(-uid, -geodeticDatum, -coordinateUncertainty, -elevationUncertainty, -taxonIDRemarks, -morphospeciesID,
                -morphospeciesIDRemarks, -identificationReferences, -identificationHistoryID, -measuredBy, -recordedBy, 
                -samplingImpracticalRemarks, -biophysicalCriteria, -publicationDate, -release, -identificationQualifier, -remarks) %>% 
  dplyr::filter( is.na(samplingImpractical) | samplingImpractical %in% "OK" ) %>%  # remove data where sampling was not done
 dplyr::select(-samplingImpractical) %>% 
   filter(taxonRank %in% c("species", "subspecies", "variety")| is.na(taxonRank)) %>% # filter when tax rank is either sp level or of higher resolution AND those that are NA
#  filter(targetTaxaPresent %in% "Y") %>%  # we need absent records too
 # # Remove extra white spaces from all columns
  mutate(across(where(is.character), str_squish)) %>%
 # mutate(across(where(is.numeric), \(x) gsub(pattern = "^\\s+|\\s+$", replacement = "", x))) %>% # another way to remove spaces
   mutate(binomialName = str_extract(scientificName, "^\\S+ \\S+")) %>% # estract the first and next non-white space strings. 
  relocate(binomialName, .after = scientificName) 

save(div_1m2_NeonPlants2026, file = here("2026Mar_download", "div_1m2_NeonPlants2026.RData"))


# # this is the same as above
# # dive into 1m2 data, the entire, long dataset, all taxon ranks, both natives and invasives
# # for SCBI
# div_1m2_NeonPlantsSCBI2025_all = div_1m2_NeonPlantsSCBI2025 %>% 
#   dplyr::select(-uid, -geodeticDatum, -coordinateUncertainty, -elevationUncertainty, -taxonIDRemarks, -morphospeciesID,
#                 -morphospeciesIDRemarks, -identificationReferences, -identificationHistoryID, -measuredBy, -recordedBy, 
#                 -samplingImpracticalRemarks, -biophysicalCriteria, -publicationDate, -release, -identificationQualifier, -remarks) %>% 
#   dplyr::filter( is.na(samplingImpractical) | samplingImpractical %in% "OK" ) %>%  # remove data where sampling was not done
#   dplyr::select(-samplingImpractical) %>% 
#   filter(taxonRank %in% c("species", "subspecies", "variety")| is.na(taxonRank)) %>% # filter when tax rank is either sp level or of higher resolution AND those that are NA
#  # filter(targetTaxaPresent %in% "Y") %>%  # keep records with plants
#   # # Remove extra white spaces from all columns
#   mutate(across(where(is.character), str_squish)) %>%
#   # mutate(across(where(is.numeric), \(x) gsub(pattern = "^\\s+|\\s+$", replacement = "", x))) %>% # another way to remove spaces
#   mutate(binomialName = str_extract(scientificName, "^\\S+ \\S+")) %>% # estract the first and next non-white space strings. 
#   relocate(binomialName, .after = scientificName) 
# 
# save(div_1m2_NeonPlantsSCBI2025_all, file = "div_1m2_NeonPlantsSCBI2025_all.RData")
# 
# 
# # this is the same as above
# # dive into 1m2 data, the entire, long dataset, all taxon ranks, both natives and invasives
# # for GRSM
# div_1m2_NeonPlantsGRSM2025_all = div_1m2_NeonPlantsGRSM2025 %>% 
#   dplyr::select(-uid, -geodeticDatum, -coordinateUncertainty, -elevationUncertainty, -taxonIDRemarks, -morphospeciesID,
#                 -morphospeciesIDRemarks, -identificationReferences, -identificationHistoryID, -measuredBy, -recordedBy, -remarks,
#                 -samplingImpracticalRemarks, -biophysicalCriteria, -publicationDate, -release, -identificationQualifier) %>% 
#   dplyr::filter( is.na(samplingImpractical) | samplingImpractical %in% "OK" ) %>%  # remove data where sampling was not done
#  # check this one before running
#    dplyr::select(-samplingImpractical) %>% # taxa present for all, no other vars listed
#   filter(taxonRank %in% c("species", "subspecies", "variety")| is.na(taxonRank)) %>% # filter when tax rank is either sp level or of higher resolution AND those that are NA
# #  filter(targetTaxaPresent %in% "Y") %>%  # keep records with plants
#   # # Remove extra white spaces from all columns
#   mutate(across(where(is.character), str_squish)) %>%
#   # mutate(across(where(is.numeric), \(x) gsub(pattern = "^\\s+|\\s+$", replacement = "", x))) %>% # another way to remove spaces
#   mutate(binomialName = str_extract(scientificName, "^\\S+ \\S+")) %>% # estract the first and next non-white space strings. 
#   relocate(binomialName, .after = scientificName) 
# 
# save(div_1m2_NeonPlantsGRSM2025_all, file = "div_1m2_NeonPlantsGRSM2025_all.RData")
# 

# bind all three sites together, this one will have non-plant documentation (moss, non-vascular, etc)
# Harv_Scbi_Grsm_1m_allPlantsNeon2025 = bind_rows(div_1m2_NeonPlantsGRSM2025_all, div_1m2_NeonPlantsSCBI2025_all, div_1m2_NeonPlantsHarv2025_all)
# 
# save(Harv_Scbi_Grsm_1m_allPlantsNeon2025, file = here("data", "Harv_Scbi_Grsm_1m_allPlantsNeon2025.RData"))

# retain only plant vars
div_1m2_allNeonSites_plantDiv <- div_1m2_NeonPlants2026 %>% 
  filter(divDataType %in% "plantSpecies") %>% 
  dplyr::select(-otherVariables, -otherVariablesPresent )

save(div_1m2_allNeonSites_plantDiv, file = here("2026Mar_download", "div_1m2_allNeonSites_plantDiv.RData"))


# make wide format
div_1m2_allNeonSites_plantDiv_wide = div_1m2_allNeonSites_plantDiv %>% 
  pivot_wider(id_cols = c(namedLocation, domainID, siteID, decimalLatitude, decimalLongitude, elevation, plotType, nlcdClass, plotID, 
                          subplotID, endDate, boutNumber, eventID),
              names_from = binomialName, names_repair = "check_unique", values_from = percentCover, values_fill = 0, 
              values_fn = sum ) %>% 
  group_by(domainID, siteID, namedLocation, nlcdClass, plotID, subplotID, endDate, boutNumber, eventID)

save(div_1m2_allNeonSites_plantDiv_wide, file = here("2026Mar_download", "div_1m2_allNeonSites_plantDiv_wide.RData"))


##=====================================================================
# copying data into excel
# saving these outputs into xl
# Create a blank workbook
# div_1m2_NeonPlantsHarv2023 <- createWorkbook()
# allNeonPlantData_1m_2026 <- createWorkbook()
# 
# addWorksheet(wb = allNeonPlantData_1m_2026,
#              sheetName = "allSitesAllPlants_wide")
# 
# addWorksheet(wb = allNeonPlantData_1m_2026,
#              sheetName = "allSitesAllPlants_long")
# 
# 
# # Write the data to the sheets of the above wb
# writeData(wb = allNeonPlantData_1m_2026, # name of the created wb
#           sheet = "allSitesAllPlants_wide", # name of the sheet
#           x = div_1m2_allNeonSites_plantDiv_wide, # the df 
#           startCol = 1, startRow = 1, # starting point
#           colNames = TRUE, rowNames = FALSE, 
#           headerStyle = createStyle(fontName = NULL, # like Calibri
#                                     fontSize = 11, fontColour = "black", numFmt = "GENERAL", 
#                                     border = NULL, borderColour = "black", borderStyle = "thin", 
#                                     bgFill = NULL, fgFill = NULL, # background and foreground color
#                                     halign = "center", valign = "center", textDecoration = "bold",
#                                     wrapText = T, # cell contents will wrap to fit in column.
#                                     textRotation = NULL, indent = NULL, # Horizontal indentation of cell contents.
#           ),
#           borders = "none", borderColour = "black", borderStyle = "thin", # table formats
#           withFilter = "T",  # add filter 
#           keepNA = FALSE)
# 
# 
# # Write the data to the sheets of the above wb
# writeData(wb = Harv_Scbi_Grsm_1m_allNeonData, # name of the created wb
#           sheet = "allSitesAllPlants_long", # name of the sheet
#           x = div_1m2_allNeonSites_plantDiv, # the df
#           startCol = 1, startRow = 1, # starting point
#           colNames = TRUE, rowNames = FALSE, 
#           headerStyle = createStyle(fontName = NULL, # like Calibri
#                                     fontSize = 11, fontColour = "black", numFmt = "GENERAL", 
#                                     border = NULL, borderColour = "black", borderStyle = "thin", 
#                                     bgFill = NULL, fgFill = NULL, # background and foreground color
#                                     halign = "center", valign = "center", textDecoration = "bold",
#                                     wrapText = T, # cell contents will wrap to fit in column.
#                                     textRotation = NULL, indent = NULL, # Horizontal indentation of cell contents.
#           ),
#           borders = "none", borderColour = "black", borderStyle = "thin", # table formats
#           withFilter = "T",  # add filter 
#           keepNA = FALSE)
# 

# Reorder worksheets
# worksheetOrder(OUT) <- c(2,1)

# Export the file, this can be updated
# saveWorkbook(wb = Harv_Scbi_Grsm_1m_allNeonData, file = here("data", "Harv_Scbi_Grsm_1m_allNeonData.xlsx"),
#              overwrite = T)
##========================================================================================================


# how many plots are there for each land-cover type for all neon sites for invasive species?
div_1m2_allNeonSites_plots_invasivePerNlcd = div_1m2_allNeonSites_plantDiv %>% 
  dplyr::filter(nativeStatusCode != "N" ) %>% 
  distinct(domainID, siteID, nlcdClass, plotID) %>% 
  group_by(domainID, siteID, nlcdClass) %>% 
  count(sort = T, name = "no_of_plots_invPlants", .drop = F) %>%  # if FALSE will include counts for empty groups (i.e. for levels of factors that don't exist in the data).
  dplyr::arrange( .by_group = T, desc(no_of_plots_invPlants) )
div_1m2_allNeonSites_plots_invasivePerNlcd

# how many subplots are there for each land-cover type for the selected (harv, scbi, grsm) neon sites for invasive plant species?
div_1m2_allNeonSites_subplots_invasivePerNlcd = div_1m2_allNeonSites_plantDiv %>% 
  dplyr::filter(nativeStatusCode != "N" ) %>% 
  distinct(domainID, siteID, nlcdClass, plotID, subplotID) %>% 
  group_by(domainID, siteID, nlcdClass) %>% 
  count(sort = T, name = "no_of_subplots_invPlants", .drop = F) %>%  # if FALSE will include counts for empty groups (i.e. for levels of factors that don't exist in the data).
  dplyr::arrange( .by_group = T, desc(no_of_subplots_invPlants) )
div_1m2_allNeonSites_subplots_invasivePerNlcd

save(div_1m2_allNeonSites_plots_invasivePerNlcd, div_1m2_allNeonSites_subplots_invasivePerNlcd, 
     file = here("2026Mar_download", "div_1m2_allNeonSites_plots_invPlantsPerNlcd.RData"))


# this dataset includes taxa at species, subspecies, var rank
# writexl::write_xlsx(list(plots_inv_Nlcd = Harv_Scbi_Grsm_1m_plots_invasivePerNlcd, 
#                          subpl_inv_Nlcd = Harv_Scbi_Grsm_1m_subplots_invasivePerNlcd), 
#                     path = here("data", "plotsPerNLDS_invPlants_harv_scbi_grsm_1m.xlsx"))

# join the growth habit and duration to neon all plants data from the selected sites
div_1m2_allNeonSites_Plants_growthHabitDuration = left_join(x = div_1m2_allNeonSites_plantDiv , y = PLANTS_durationHabit, 
                                                           by = join_by(taxonID == acceptedSymbol, scientificName, binomialName),
                                                           copy = FALSE, keep = F, na_matches = "na", multiple = "all", 
                                                           unmatched = "drop", relationship = "many-to-many") %>% 
  mutate(binomialName = word(scientificName, start = 1, end = 2) )

save(div_1m2_allNeonSites_Plants_growthHabitDuration, 
     file = here("2026Mar_download", "div_1m2_allNeonSites_Plants_growthHabitDuration.RData"))


# this dataset includes taxa at species, subspecies, var rank
# writexl::write_xlsx(list(allPlant_1m_traits = Harv_Scbi_Grsm_1m_allPlants_growthHabitDuration, 
#                          allPlantsDivData_1m_traits = Harv_Scbi_Grsm_1m_allPlants_plantDiv_growthHabitDuration), 
#                     path = here("data", "Harv_Scbi_Grsm_1m_allVars_plantDiv_growthHabitDuration.xlsx"))
# 

# what is the representation of growth habits among the selected sites for 1m plots, all plant species included?
div_1m2_allNeonSites_Plants_growthHabit <- div_1m2_allNeonSites_Plants_growthHabitDuration %>%  
  dplyr::select(domainID, siteID, nativeStatusCode, growthHabit, binomialName) %>% 
  distinct(domainID, siteID, nativeStatusCode, growthHabit, binomialName) %>% 
  group_by(domainID, siteID, nativeStatusCode, growthHabit) %>% 
  summarise(no_speciesPerHabit = n(), .groups = "drop") %>% 
  dplyr::arrange(.by_group = T, domainID, siteID, nativeStatusCode, desc(no_speciesPerHabit))

# what is the representation of duration among the selected sites for 1m plots, all plant species included?
div_1m2_allNeonSites_Plants_PerDuration = div_1m2_allNeonSites_Plants_growthHabitDuration %>%  
  dplyr::select(domainID, siteID, nativeStatusCode, duration, binomialName) %>% 
  distinct(domainID, siteID, nativeStatusCode, duration, binomialName) %>% 
  group_by(domainID, siteID, nativeStatusCode, duration) %>% 
  summarise(no_speciesPerDuration = n(), .groups = "drop") %>%
  dplyr::arrange(.by_group = T, domainID, siteID, nativeStatusCode, desc(no_speciesPerDuration))
 

# # this dataset includes taxa at species, subspecies, var rank
# writexl::write_xlsx(list(allPlants_1m_PerDuration = Harv_Scbi_Grsm_1m_allPlantsPerDuration,
#                          allPlants_1m_PerHabit = Harv_Scbi_Grsm_1m_allPlantsPergrowthHabit), 
#                     path = here("data", "Harv_Scbi_Grsm_1m_growthHabit_Duration_species.xlsx"))


# # upload to google drive
# # authenticate your Google account (first-time only or to switch account)
# drive_auth()
# 
# # Define parent folder by its ID (from the URL)
# # URL:https://drive.google.com/drive/folders/198G5ohy91doZ37OWwe15u2VYJ8D0epFt
# parent_googleFolder <- as_id("198G5ohy91doZ37OWwe15u2VYJ8D0epFt")  # replace with actual ID
# 
# # Create a new subfolder inside that folder
# target_googleFolder <- drive_mkdir(name = "PPPC2025", path = parent_googleFolder)
# 
# # Files to upload (using here() for relative paths)
# files_to_upload <- c(
#   here("data", "Harv_Scbi_Grsm_1m_allNeonData.xlsx"),
#   here("data", "variables_10058.csv"),
#   here("data", "readme_10058.txt"),
#   here("data", "plantsTaxTab.RData"),
#   here("data", "Harv_Scbi_Grsm_1m_allVars_plantDiv_growthHabitDuration.xlsx"),
#   here("data", "Harv_Scbi_Grsm_1m_growthHabit_Duration_species.xlsx"),
#   here("data", "plotsPerNLDS_invPlants_harv_scbi_grsm_1m.xlsx"),
#   here("data", "Harv_Scbi_Grsm_1m_allPlants_plantDiv_growthHabitDuration.RData"),
#   here("data", "Harv_Scbi_Grsm_1m_allPlantsNeon2025.RData"),
#   here("data", "Harv_Scbi_Grsm_1m_allPlantsNeon2025_plantDiv.RData"),
#   here("data", "Harv_Scbi_Grsm_1m_allPlantsNeon2025_wide.RData"),
#   here("script", "mefaV3.R")
# )
# 
# # Upload files without conversion
# for (f in files_to_upload) {
#   drive_upload(
#     media = f,
#     path = as_id(target_googleFolder),  # uploads to the new subfolder
#     name = basename(f),
#     type = NULL,
#     overwrite = TRUE
#   )
# }
# 
# 
# # write a readme/metadata file
# # Define file names and descriptions together, clearly
# 
# file_info_df <- tribble(
#   ~filename, ~description,
#   
#   "Harv_Scbi_Grsm_1m_allNeonData.xlsx",
#   "NEON Plant precent cover data at 1m subplots from the three neon sites accessed in 2025, 
# in both wide (allSitesAllPlants_wide) and long (allSitesAllPlants_long) formats without traits data",
#   
#   "variables_10058.csv",
#   "NEON PPPC vars from the API",
#   
#   "readme_10058.txt",
#   "a readme file from the API",
#   
#   "plantsTaxTab.RData",
#   "all neon plants accessed from the NEON API",
#   
#   "Harv_Scbi_Grsm_1m_allVars_plantDiv_growthHabitDuration.xlsx",
#   "NEON Plant precent cover data at 1m subplots from the three neon sites accessed in 2025 with USDA PLANTS traits (duration, habit), in long format, 
# some species might have multiple trait categories, and this table has plots with plant metrics and other vars (moss, lichen, etc), 
# two tabs- one with all vars (allPlant_1m_traits) and another just with plants (allPlantsDivData_1m_traits). see divDataType col for this info",
#   
#   "Harv_Scbi_Grsm_1m_growthHabit_Duration_species.xlsx",
#   "NEON Plant precent cover data at 1m subplots from the three neon sites accessed in 2025, 
# how many invasive/native species in each trait category per NEON site, each trait in seperate tabs",
#   
#   "plotsPerNLDS_invPlants_harv_scbi_grsm_1m.xlsx",
#   "NEON Plant precent cover data at 1m subplots from the three neon sites accessed in 2025,
# how many plots or subplots have invasive species per each land cover type per site",
#   
#   "Harv_Scbi_Grsm_1m_allPlants_plantDiv_growthHabitDuration.RData",
#   "RData file for NEON Plant precent cover data at 1m subplots from the three neon sites accessed in 2025 with USDA PLANTS traits (duration, habit), in long format, 
# some species might have multiple trait categories, and this df ONLY includes plant metrics, other vars (moss, lichen, etc) have been removed", 
#   
#   "Harv_Scbi_Grsm_1m_allPlantsNeon2025.RData",
#   "RData file for NEON Plant precent cover data at 1m subplots from the three neon sites accessed in 2025 without traits (duration, habit), in long format, 
# some species might have multiple trait categories, and this df includes plant metrics AND other vars (moss, lichen, etc) in divDataType col",
#   
#   "Harv_Scbi_Grsm_1m_allPlantsNeon2025_plantDiv.RData",
#   "RData file for NEON Plant precent cover data at 1m subplots from the three neon sites accessed in 2025 without traits (duration, habit), in long format, 
# some species might have multiple trait categories, and this df includes plant metrics ONLY and other vars (moss, lichen, etc) are removed",
#   
#   "Harv_Scbi_Grsm_1m_allPlantsNeon2025_wide.RData",
#   "RData file for NEON Plant precent cover data at 1m subplots from the three neon sites accessed in 2025 without traits (duration, habit), in wide format, 
# some species might have multiple trait categories, and this df includes plant metrics ONLY, other vars (moss, lichen, etc) is removed",
#   
#   "mefaV3.R",
#   "the script for all cleaning"
# )
# 
# # Write to CSV, preserving multiline descriptions
# write_csv(file_info_df, file = here("data", "file_manifest.csv"))
# 
# 
# drive_upload(
#   media = here("data", "file_manifest.csv"),
#   path = target_googleFolder,
#   name = "file_manifest.csv",  # Keep same name
#   type = NULL,                 # Prevents conversion to Google Sheets
#   overwrite = TRUE             # Replace if it already exists
# )
# 
# 
# 
# 
