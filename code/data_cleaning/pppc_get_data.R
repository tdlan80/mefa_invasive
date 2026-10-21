# this script downloads all available pppc data and generates the raw combined table 
# containing both 10m2 and larger sub/plot data

library(neonUtilities)

p_all <- loadByProduct("DP1.10058.001",
                       package="expanded",
                       check.size=F,
                       include.provisional=T)
saveRDS(p_all, paste(getwd(), "pppc_all.rds", sep="/"))

pppc_allScales <- data.table::rbindlist(list(p_all$div_1m2Data, 
                                             p_all$div_10m2Data100m2Data), 
                                        fill=T)
pppc_allScales <- data.frame(pppc_allScales)
saveRDS(pppc_allScales, paste(getwd(), "pppc_allScales.rds", sep="/"))
