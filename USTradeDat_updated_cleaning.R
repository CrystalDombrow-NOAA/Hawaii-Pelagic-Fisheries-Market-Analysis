# Generate Emily's USTradeDat.csv from FOSS data

rm(list = ls())

library(tidyverse)

# On Crystal's computer
setwd("C:/Users/crystal.dombrow/Desktop/Crystal/1_HI Seafood Market/Pelagic market analysis R/Data/2008-2019_data")

USTradeDat_updated_2008.2012 <- read.csv("foss_foreigntrade_monthly2008-2012.csv")
USTradeDat_updated_2013.2016 <- read.csv("foss_foreigntrade_monthly2013-2016.csv")
USTradeDat_updated_2017.2019 <- read.csv("foss_foreigntrade_monthly2017-2019.csv")

# On H: Drive
#USTradeDat_updated_2008.2012 <- read.csv("2008-2019_data/foss_foreigntrade_monthly2008-2012.csv")
#USTradeDat_updated_2013.2016 <- read.csv("2008-2019_data/foss_foreigntrade_monthly2013-2016.csv")
#USTradeDat_updated_2017.2019 <- read.csv("2008-2019_data/foss_foreigntrade_monthly2017-2019.csv")

USTradeDat_updated_combined <- bind_rows(USTradeDat_updated_2008.2012, 
                                         USTradeDat_updated_2013.2016,
                                         USTradeDat_updated_2017.2019)

USTradeDat_updated_combined_clean <- USTradeDat_updated_combined %>% 
  select(-Month, -Product.Name, -Country.Name, -FAO.Country.Code, -RFMO, 
         -Trade.Associations, -US.Customs.District.Name) %>%
  rename(YR = Year, 
         Month = Month.number, 
         Edible = Edible.code, 
         "Product ID" = HTS.Number,
         "Country ID" = Census.Country.Code,
         "District ID" = US.Customs.District.Code,
         Volume = Volume..kg., # Kg converted to lbs in 1-Installations.R
         Dollars = Value..USD.,
         "Trade Type" = Trade.Type) %>%
  mutate(Volume = as.numeric(gsub(",", "", Volume)),
         Dollars = as.numeric(gsub(",", "", Dollars))) %>%
  relocate(Month, Edible, "Product ID", "Country ID", "District ID",
           Volume, Dollars, "Trade Type", .after = YR) %>%
  mutate(`Trade Type` = if_else(`Trade Type` == "IMP", 0, 1))

# On Crystal's computer
write_csv(USTradeDat_updated_combined_clean, "USTradeDat.csv")

# On H: Drive
#write_csv(USTradeDat_updated_combined_clean, "2008-2019_data/USTradeDat.csv")