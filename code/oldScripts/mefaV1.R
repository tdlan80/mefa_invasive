

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

load(file = "plantDivData.RData")
load(file = "riparianVegData.RData")
load("plantsTaxTab.RData")
load(file = "potentialInvasive.RData")
load(file = "invasiveSpPerSite.RData")
load(file = "div_1m2_div_10m2Data100m2_NeonPlantsHarv2023.RData")
load(file = "div_1m2_div_10m2Data100m2_NeonPlantsSCBI2023.RData")
load(file = "div_1m2_div_10m2Data100m2_NeonPlantsGRSM2023.RData")
load(file = "div_1m2_NeonPlantsHarv2023_species.RData")
load(file = "div_1m2_NeonPlantsHarv2023_all.RData")
load(file = "div_1m2_NeonPlantsHarv2023_subSpecies.RData")
load(file = "div_1m2_NeonPlantsHarv2023_var.RData")
load(file = "div_1m2_NeonPlantsHarv2023_spSubVar.RData")


browseURL("https://www.neonscience.org/data-collection/terrestrial-plants")

# Get the names of objects in the list
object_names <- names(my_list)

# Print the names and structure of each object
for (name in object_names) {
  print(name)
  print(str(my_list[[name]]))
}

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


# what sites have invasive species?
# "UNK" NA    "NI"  "I"   "A"   "N?"  "I?"  "NI?"
# nativeStatusCode: 'A': Presumed absent, due to lack of data indicating a taxon's presence in a given location; 
# 'N': Native;  # 'N?': Probably Native; 
# 'I': Introduced; 'I?': Probably Introduced; 
# 'NI': Native and Introduced, some infrataxa are native and others are introduced; 
# 'NI?': Probably Native and Introduced, some infrataxa are native and others are introduced; 
# 'UNK': Status unknown.

# screen data with invasive-ish species for 10m and 100 m data
potentialInvasive <- div_10m2Data100m2Data %>% 
  dplyr::filter( is.na(samplingImpractical) | samplingImpractical == "OK" ) %>% 
  dplyr::filter(targetTaxaPresent == "Y" ) %>% 
    dplyr::filter(nativeStatusCode != "N" ) %>% 
  dplyr::select(-geodeticDatum, -coordinateUncertainty, -elevationUncertainty, -samplingProtocolVersion, -targetTaxaPresent, 
                -(identificationQualifier:release))

# only retain ID at species or sub-species rank  
potentialInvasiveSpSubSp = potentialInvasive %>% 
  filter(taxonRank %in% c("species", "subspecies", "variety"))

# check what species are present in which sites
potentialInvasiveSpSubSpWide = potentialInvasiveSpSubSp %>% 
  dplyr::select(-uid, -decimalLatitude, -decimalLongitude, -taxonID, -taxonRank, -family, -nativeStatusCode) %>% 
 dplyr::select(-namedLocation, -plotType, -nlcdClass, -plotID, -subplotID, -endDate, -boutNumber, -eventID) %>% 
  distinct(domainID, siteID, scientificName, .keep_all = T) %>% 
  mutate(presence = ifelse(!is.na(scientificName), 1, 0)) %>% 
  pivot_wider(id_cols = c(domainID, siteID), 
              names_from = scientificName, values_from = presence, values_fill = 0, names_repair = "unique") %>% 
  clean_names()
  
save(potentialInvasiveSpSubSpWide, potentialInvasiveSpSubSp, potentialInvasive, file = "potentialInvasive.RData")


# what sites have invasive species? 47 sites have invasive species
invSitesAll = potentialInvasiveSpSubSp %>% 
  distinct(domainID, siteID)

# how many sites have invasive species
potentialInvasiveSpSubSp %>% 
  distinct(siteID) %>% tally()
# 47 sites

# how many domains have invasive species
potentialInvasiveSpSubSp %>% 
  distinct(domainID) %>% tally()
# 20 domains

# how many invasive species or subspecies overall
potentialInvasiveSpSubSp %>% 
  distinct(scientificName) %>% tally()
# 677

# how many invasive species or subspecies overall per NEON site
invasiveSpPerSite = potentialInvasiveSpSubSp %>%
  distinct(domainID, siteID, scientificName) %>% 
 dplyr::group_by(domainID, siteID) %>%
  dplyr::summarise(noInvSpecies = n() )

invasiveSpPerSite = potentialInvasiveSpSubSp %>%
  distinct(domainID, siteID, scientificName) %>% 
  dplyr::group_by(domainID, siteID) %>%
  tally()

save(invSitesAll, invasiveSpPerSite, file = "invasiveSpPerSite.RData")

writexl::write_xlsx(list(invasivePlantsWide = potentialInvasiveSpSubSpWide , 
                         invasivePlantsLong = potentialInvasiveSpSubSp,
                         invasiveSpSubspPerSite = invasiveSpPerSite
                         ), 
                    path = "invasiveNEONdataV2.xlsx")



# screen data with invasive-ish species for 1 m data
potentialInvasive_1m <- div_1m2Data %>% 
  dplyr::filter( is.na(samplingImpractical) | samplingImpractical == "OK" ) %>% 
  dplyr::filter(targetTaxaPresent == "Y" ) %>% 
  dplyr::filter(nativeStatusCode != "N" ) %>% 
  dplyr::select(-geodeticDatum, -coordinateUncertainty, -elevationUncertainty, -samplingProtocolVersion, -targetTaxaPresent, 
                -(identificationQualifier:release))

# only retain ID at species or sub-species rank  
potentialInvasiveSpSubSp_1m = potentialInvasive_1m %>% 
  filter(taxonRank %in% c("species", "subspecies", "variety"))

# check what species are present in which sites
potentialInvasiveSpSubSpWide_1m = potentialInvasiveSpSubSp_1m %>% 
  dplyr::select(-uid, -decimalLatitude, -decimalLongitude, -taxonID, -taxonRank, -family, -nativeStatusCode) %>% 
  dplyr::select(-namedLocation, -plotType, -nlcdClass, -plotID, -subplotID, -endDate, -boutNumber, -eventID) %>% 
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


# pull data for HARV
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



# this is the plant cover and presence data for HARV
div_10m2Data100m2_NeonPlantsHarv2023 <- div_10m2Data100m2Data
div_1m2_NeonPlantsHarv2023 <- div_1m2Data

div_10m2Data100m2_NeonPlantsHarv2023 <- div_10m2Data100m2Data
div_1m2_NeonPlantsHarv2023 <- div_1m2Data



# this is the plant cover and presence data for SCBI
div_10m2Data100m2_NeonPlantsSCBI2023 <- div_10m2Data100m2Data
div_1m2_NeonPlantsSCBI2023 <- div_1m2Data

div_10m2Data100m2_NeonPlantsSCBI2023 <- div_10m2Data100m2Data
div_1m2_NeonPlantsSCBI2023 <- div_1m2Data


# this is the plant cover and presence data for GRSM
div_10m2Data100m2_NeonPlantsGRSM2023 <- div_10m2Data100m2Data
div_1m2_NeonPlantsGRSM2023 <- div_1m2Data

div_10m2Data100m2_NeonPlantsGRSM2023 <- div_10m2Data100m2Data
div_1m2_NeonPlantsGRSM2023 <- div_1m2Data


# remove white spaces from col headings
names(div_1m2_NeonPlantsHarv2023) <- str_squish(names(div_1m2_NeonPlantsHarv2023))

save(div_10m2Data100m2_NeonPlantsHarv2023, div_1m2_NeonPlantsHarv2023, file = "div_1m2_div_10m2Data100m2_NeonPlantsHarv2023.RData")
save(div_10m2Data100m2_NeonPlantsSCBI2023, div_1m2_NeonPlantsSCBI2023, file = "div_1m2_div_10m2Data100m2_NeonPlantsSCBI2023.RData")
save(div_10m2Data100m2_NeonPlantsGRSM2023, div_1m2_NeonPlantsGRSM2023, file = "div_1m2_div_10m2Data100m2_NeonPlantsGRSM2023.RData")

# this is the same as above
# dive into im2 data, the entire, long dataset, all taxon ranks
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
   mutate(binomialName = str_extract(scientificName, "^\\S+ \\S+")) %>% 
  relocate(binomialName, .after = scientificName) 

save(div_1m2_NeonPlantsHarv2023_all, file = "div_1m2_NeonPlantsHarv2023_all.RData")

# saving these outputs into xl
# Create a blank workbook
div_1m2_NeonPlantsHarv2023 <- createWorkbook()

# Add some sheets to the workbook
addWorksheet(wb = div_1m2_NeonPlantsHarv2023,
  sheetName = "allHarvPlants")

# Write the data to the sheets
writeData(wb = div_1m2_NeonPlantsHarv2023, # name of the created wb
  sheet = "allHarvPlants", # name of the sheet
  x = div_1m2_NeonPlantsHarv2023_all, # the df
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
saveWorkbook(wb = div_1m2_NeonPlantsHarv2023, file = "div_1m2_NeonPlantsHarv2023.xlsx",
             overwrite = FALSE)

# just the full dataset
writexl::write_xlsx(list(allHarvPlants = div_1m2_NeonPlantsHarv2023_all), # rename the sheet as "allHarvPlants".
                    path = "div_1m2_NeonPlantsHarv2023_all.xlsx",
                    col_names = TRUE, format_headers = TRUE)



# let us clean up sci names more 
# first for species level ID
# dive into im2 data, the entire, long dataset, just for species ranks
div_1m2_NeonPlantsHarv2023_species = div_1m2_NeonPlantsHarv2023_all %>% 
  filter(taxonRank == "species" ) %>% 
  mutate(binomialName = str_extract(scientificName, "^\\S+ \\S+")) %>% # extract the first two words
  mutate(binomialName = if_else(
  condition = str_detect(scientificName, " f\\.$"), # detect for specific strings at the end
  true = paste0(binomialName, " f.") , # add back the f., if that is present in scientificName
  false = binomialName)) %>% 
  relocate(binomialName, .after = scientificName) %>% 
  mutate(sciName_noAuth = binomialName) %>%
  relocate(sciName_noAuth, .after = scientificName)

save(div_1m2_NeonPlantsHarv2023_species, file = "div_1m2_NeonPlantsHarv2023_species.RData")


# let us clean up sci names more 
# first for subspecies level ID
# dive into im2 data, the entire, long dataset, just for subspecies ranks
div_1m2_NeonPlantsHarv2023_subSpecies = div_1m2_NeonPlantsHarv2023_all %>% 
  filter(taxonRank == "subspecies" ) %>% 
  mutate(sciName_noAuth = str_extract(scientificName, "^\\S+ \\S+")) %>% # extract the first two words
  mutate(sciName_noAuth = if_else(
    condition = str_detect(scientificName, "\\s(ssp\\.)\\s"), # detect for spp. with two spaces on either side
    true = paste0(binomialName, str_extract(scientificName, "\\s(ssp\\.)\\s(\\w+)")) , # add spp. and first word after that
    # Explanation of the regex:
    # - (?<=\\bssp\\.\\s): Positive lookbehind assertion to match " ssp. " preceded by a word boundary and followed by a space.
    # - \\w+: Matches one or more word characters (letters, digits, or underscores), which corresponds to the word following "ssp.".
        # - \\s: Matches any whitespace character (space, tab, newline) before "ssp.".
    # - (ssp\\.): Captures "ssp." literally. The backslashes escape the period to ensure it's treated as a literal period in the regex.
    # - \\s: Matches any whitespace character (space, tab, newline) after "ssp.".
    false = binomialName)) %>% 
  relocate(sciName_noAuth, .after = scientificName)

save(div_1m2_NeonPlantsHarv2023_subSpecies, file = "div_1m2_NeonPlantsHarv2023_subSpecies.RData")



# let us clean up sci names more 
# first for variety level ID
# dive into im2 data, the entire, long dataset, just for variety-level ranks
div_1m2_NeonPlantsHarv2023_var = div_1m2_NeonPlantsHarv2023_all %>% 
  filter(taxonRank == "variety" ) %>% 
  mutate(sciName_noAuth = str_extract(scientificName, "^\\S+ \\S+")) %>% # extract the first two words
  mutate(sciName_noAuth = if_else(
    condition = str_detect(scientificName, "\\s(var\\.)\\s"), #  # detect for var. with two spaces on either side
    true = paste0(binomialName, str_extract(scientificName, "\\s(var\\.)\\s(\\w+)")) , # add var. and the first word after that
    false = binomialName)) %>% 
  relocate(sciName_noAuth, .after = scientificName)

save(div_1m2_NeonPlantsHarv2023_var, file = "div_1m2_NeonPlantsHarv2023_var.RData")


# now, we can put all var, spp, and species together
div_1m2_NeonPlantsHarv2023_spSubVar <- bind_rows(div_1m2_NeonPlantsHarv2023_species, 
                                                div_1m2_NeonPlantsHarv2023_subSpecies, 
                                                div_1m2_NeonPlantsHarv2023_var)
save(div_1m2_NeonPlantsHarv2023_spSubVar, file = "div_1m2_NeonPlantsHarv2023_spSubVar.RData")


# update the wb created before
# Add some sheets to the workbook
addWorksheet(wb = div_1m2_NeonPlantsHarv2023, sheetName = "HarvPlants_subSpecies")
addWorksheet(wb = div_1m2_NeonPlantsHarv2023, sheetName = "HarvPlants_var")

# Write the data to the sheets
writeData(wb = div_1m2_NeonPlantsHarv2023, # name of the created wb
          sheet = "HarvPlants_subSpecies", # name of the sheet
          x = div_1m2_NeonPlantsHarv2023_subSpecies, # the df
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

writeData(wb = div_1m2_NeonPlantsHarv2023, # name of the created wb
          sheet = "HarvPlants_var", # name of the sheet
          x = div_1m2_NeonPlantsHarv2023_var , # the df
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
saveWorkbook(wb = div_1m2_NeonPlantsHarv2023, file = "div_1m2_NeonPlantsHarv2023.xlsx",
             overwrite = T)

