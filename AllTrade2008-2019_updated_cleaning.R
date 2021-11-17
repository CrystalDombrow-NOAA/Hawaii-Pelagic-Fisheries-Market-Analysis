# Generate Emily's All Trade 1975-2017.csv from FOSS data

rm(list = ls())

library(tidyverse)

# On Crystal's computer
setwd("C:/Users/crystal.dombrow/Desktop/Crystal/HI Pel Fish Market/Pelagic market analysis R UPDATED DATA, cleaned code/Data/2008-2019 data")

AllTrade_updated <- read.csv("foss_foreigntrade_monthlyHNL.csv")

# On H: Drive
#AllTrade_updated <- read.csv("Data/2008-2019_data/foss_foreigntrade_monthlyHNL.csv")

AllTrade_updated_clean <- AllTrade_updated %>% 
  select(-Month, -Country.Name, -FAO.Country.Code, -RFMO, 
         -Trade.Associations, -US.Customs.District.Name) %>%
  rename(YR = Year, 
         Month = Month.number, 
         Edible = Edible.code, 
         "Product ID" = HTS.Number,
         "Country ID" = Census.Country.Code,
         "District ID" = US.Customs.District.Code,
         Pounds = Volume..kg.,
         Dollars = Value..USD.,
         Trade = Trade.Type) %>%
  mutate(Pounds = as.numeric(gsub(",", "", Pounds)),
         Dollars = as.numeric(gsub(",", "", Dollars))) %>%
  mutate(Pounds = Pounds / 0.45359237) %>%
  relocate(Month, Edible, "Product ID", "Country ID", "District ID",
           Pounds, Dollars, Trade, .after = YR) %>%
  mutate(Trade = if_else(Trade == "EXP", "E",
                         if_else(Trade == "IMP", "I", "R")))

# On Crystal's computer
write_csv(AllTrade_updated_clean, "AllTrade2008-2019.csv")

# On H: Drive
#write_csv(AllTrade_updated_clean, "2008-2019_data/All Trade 2008-2019.csv")