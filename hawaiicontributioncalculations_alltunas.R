#Calculate Hawai`i contribution to US tuna supply, by year.

#Data queried: 7/9/2021 from FOSS data portal www.fisheries.noaa.gov/foss/ 

library(tidyverse)

#Clean US tuna totals
ustunatotals <- read.csv("Rscripts/HI contributions/foss_landings_alltunas_50states.csv") %>%
  select(-Collection) %>%
  mutate(Pounds = as.numeric(gsub(",", "", Pounds)),
         Dollars = as.numeric(gsub(",", "", Dollars))) %>%
  rename(USVolume = Pounds, 
         USValue = Dollars)
  
#Clean HI tuna totals
hitunatotals <- read.csv("Rscripts/HI contributions/foss_landings_alltunas_HI.csv") %>%
  select(-Collection) %>%
  mutate(Pounds = as.numeric(gsub(",", "", Pounds)),
         Dollars = as.numeric(gsub(",", "", Dollars))) %>%
  rename(HIVolume = Pounds, 
         HIValue = Dollars)
  
#Combine and calculate percentage that HI contributes
ushitunatotals <- bind_cols(inner_join(ustunatotals, hitunatotals)) %>%
  mutate(HIContrVol_percent = round(HIVolume * 10 / USVolume * 10), 
         HIContrVal_percent = round(HIValue * 10 / USValue * 10))

write_csv(ushitunatotals, "Rscripts/HI contributions/HIContributiontoUSTuna_Total.csv")