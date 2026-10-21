
########## PPPC SUBPLOT METRICS ##########

# by Jeremy Collings 10/21/26

# GOAL: From the full subplot PPPC data, generate a 
# dataframe with metrics of degree of invasion and 
# some summaries of the plant community at the subplot
# level. Then, create a dataframe per 20 x 20m plot,
# by aggregating the 1 x 1 subplot metrics. 

library(vegan)
library(tidyverse)

# Bring in the data -------------------------------------------------------

load(file.path("rawData", "NEON", "PPPC", "full_1m_plantDiv.RData"))

# Calculate subplot level metrics -----------------------------------------

# native metrics exclude "N?"

subplot_df <- pppc_1m_plantDiv %>%
  group_by(plotID, plot_subplot, eventID) %>%
  summarize(tot.rich = n_distinct(scientificName),
            nat.rich = n_distinct(scientificName[nativeStatusCode == "N"]),
            inv.rich = n_distinct(scientificName[nativeStatusCode == "I"]),
            tot.cov = sum(percentCover, na.rm = TRUE),
            nat.cov = sum(percentCover[nativeStatusCode == "N"], na.rm = TRUE),
            inv.cov = sum(percentCover[nativeStatusCode == "I"], na.rm = TRUE),
            tot.div = diversity(percentCover),
            nat.div = diversity(percentCover[nativeStatusCode == "N"]),
            inv.div = diversity(percentCover[nativeStatusCode == "I"]),
            s = inv.rich / tot.rich,
            b = inv.cov / tot.cov,
            di = (s + b)/2)

# Aggregate to the plot level ---------------------------------------------

# We wanna aggregate the s, b, and di metrics from the subplot_df 
# to the 20x20 m plot level.
# We have a couple options for the aggregationg method...
# Option #1 (agg.mean): Take the average of these values
# Option #2 (agg.sum): Recalculate the values as the sum of invaders over 
# the sum of total values per plot.

plot_df <- subplot_df %>%
  group_by(plotID, eventID) %>%
  summarize(s.agg.sum = sum(inv.rich)/sum(tot.rich), 
            s.agg.mean = mean(s),
            b.agg.sum = sum(inv.cov)/sum(tot.cov),
            b.agg.mean = mean(b), 
            di.agg.sum = (s.agg.sum + b.agg.sum)/2,
            di.agg.mean = (s.agg.mean + b.agg.mean)/2, 
            di.mean.squared = mean(di))

# making a plot to very that the two di mean values are equivalent

plot_df %>%
  ggplot(aes(x = di.agg.mean, y = di.mean.squared)) + 
  geom_point()

# now checkin' 'em all out

plot_df %>%
  ungroup() %>%
  select(-c(plotID, eventID)) %>%
  ggpairs()
