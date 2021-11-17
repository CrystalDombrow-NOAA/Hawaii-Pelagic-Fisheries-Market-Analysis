#Calculate Hawai`i contribution to US tuna supply, by year and species.

#Data queried: 7/9/2021 from FOSS data portal www.fisheries.noaa.gov/foss/

library(tidyverse)

#Clean US tuna totals
ustunatotalsbyspecies <- read.csv("Rscripts/HI contributions/foss_landings_tunasbyspecies_50states.csv") %>%
  select(-Collection, -Scientific.Name, -Tsn, -Confidentiality) %>%
  mutate(Pounds = as.numeric(gsub(",", "", Pounds)),
         Dollars = as.numeric(gsub(",", "", Dollars))) %>%
  rename(USVolume = Pounds, 
         USValue = Dollars)

#Clean HI tuna totals
hitunatotalsbyspecies <- read.csv("Rscripts/HI contributions/foss_landings_tunasbyspecies_HI.csv") %>%
  select(-Collection, -Scientific.Name, -Tsn, -Confidentiality) %>%
  mutate(Pounds = as.numeric(gsub(",", "", Pounds)),
         Dollars = as.numeric(gsub(",", "", Dollars))) %>%
  rename(HIVolume = Pounds, 
         HIValue = Dollars)

#Combine and calculate percentage that HI contributes
ushitunatotalsbyspecies <- bind_cols(inner_join(ustunatotalsbyspecies, 
                                                hitunatotalsbyspecies)) %>%
  drop_na() %>%
  rename(Species = NMFS.Name) %>%
  mutate(HIContrVol_percent = round(HIVolume * 10 / USVolume * 10), 
         HIContrVal_percent = round(HIValue * 10 / USValue * 10))

write_csv(ushitunatotalsbyspecies, "Rscripts/HI contributions/HIContributiontoUSTuna_bySpecies.csv")
