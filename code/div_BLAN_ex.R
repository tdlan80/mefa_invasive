library(devtools)
library(neonUtilities)
devtools::install_github("NEONScience/NEON-geolocation/geoNEON")
library(geoNEON)

# if you're not familiar with using a token, go here: https://www.neonscience.org/resources/learning-hub/tutorials/neon-api-tokens-tutorial
# or leave out the token, but including it will make this faster

# get plant presence and percent cover data from BLAN
div <- loadByProduct('DP1.10058.001', site='BLAN', 
                     startdate='2023-01', package='expanded',
                     include.provisional=F, check.size=F,
                     token=Sys.getenv('NEON_TOKEN'))

# get subplot locations and convert BLAN 18N locations to 17N to match remote sensing
div1m2loc <- getLocTOS(div$div_1m2Data, dataProd='div_1m2Data',
                       convertBLAN=T, token=Sys.getenv('NEON_TOKEN'))

# convertBLAN() is also available as an independent function
# but this example code won't do anything in this script, because the locations are already converted
div1m2loc17 <- convertBLAN(div1m2loc, 
                           easting="adjEasting",
                           northing="adjNorthing")
