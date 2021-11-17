# Generate Emily's USLandings.csv from FOSS data

rm(list = ls())

library(tidyverse)

# On Crystal's computer
setwd("C:/Users/crystal.dombrow/Desktop/Crystal/HI Pel Fish Market/Pelagic market analysis R UPDATED DATA/Data/2008-2019_data SOURCE UPDATED")

USLandings_updated_AllStates_exceptHI <- read_csv("foss_landings_AllStates_exceptHI.csv")

USLandings_updated_HI <- read_csv("foss_landings_HI.csv")

# On H: Drive
# USLandings_updated_AllStates_exceptHI <- read_csv("Data/2008-2019_data SOURCE UPDATED/foss_landings_AllStates_exceptHI.csv")
# USLandings_updated_HI <- read_csv("Data/2008-2019_data SOURCE UPDATED/foss_landings_HI.csv")

all_states_all_species <- USLandings_updated_AllStates_exceptHI %>%
  group_by(Year) %>%
  summarize(Pounds = sum(`Sum Pounds`, na.rm = T),
            Dollars = sum(`Sum Dollars`, na.rm = T)) %>%
  mutate(State = "All",
         `NMFS Name` = "All") %>%
  rename(YR = Year,
         Species = `NMFS Name`)

HI_all_species <- USLandings_updated_HI %>%
  group_by(Year) %>%
  summarize(Pounds = sum(`Sum Pounds`, na.rm = T),
            Dollars = sum(`Sum Dollars`, na.rm = T)) %>%
  mutate(State = "Hawaii",
         `NMFS Name` = "All") %>%
  rename(YR = Year, 
         Species = `NMFS Name`)

all_states_select_species <- USLandings_updated_AllStates_exceptHI %>%
  filter(`NMFS Name` %in% c("TUNA, BIGEYE", 
                            "TUNA, ALBACORE",
                            "TUNA, SKIPJACK",
                            "TUNA, YELLOWFIN",
                            "OPAH",
                            "DOLPHINFISH",
                            "MARLIN, BLACK",
                            "MARLIN, STRIPED",
                            "MARLIN, BLUE",
                            "SWORDFISH",
                            "POMFRETS **",
                            "WAHOO")) %>%
  mutate(`NMFS Name` = if_else(`NMFS Name` == "POMFRETS **", "POMFRETS", 
                               `NMFS Name`)) %>%
  group_by(Year, `NMFS Name`) %>%
  summarize(Pounds = sum(`Sum Pounds`, na.rm = T),
            Dollars = sum(`Sum Dollars`, na.rm = T)) %>%
  mutate(State = "All") %>%
  rename(YR = Year, 
         Species = `NMFS Name`)

HI_select_species <- USLandings_updated_HI %>%
  filter(`NMFS Name` %in% c("TUNA, BIGEYE", 
                            "TUNA, ALBACORE",
                            "TUNA, SKIPJACK",
                            "TUNA, YELLOWFIN",
                            "OPAH",
                            "DOLPHINFISH",
                            "MARLIN, BLACK",
                            "MARLIN, STRIPED",
                            "MARLIN, BLUE",
                            "SWORDFISH",
                            "POMFRETS **",
                            "WAHOO")) %>%
  mutate(`NMFS Name` = if_else(`NMFS Name` == "POMFRETS **", "POMFRETS", `NMFS Name`)) %>%
  group_by(Year, `NMFS Name`) %>%
  summarize(Pounds = sum(`Sum Pounds`, na.rm = T),
            Dollars = sum(`Sum Dollars`, na.rm = T)) %>%
  mutate(State = "Hawaii") %>%
  rename(YR = Year, 
         Species = `NMFS Name`)

USLandings_updated_combined <- bind_rows(all_states_all_species, HI_all_species,
                                         all_states_select_species, 
                                         HI_select_species)    

USLandings_updated_combined_clean <- USLandings_updated_combined %>% 
  mutate(Pounds = as.numeric(gsub(",", "", Pounds)), 
         Dollars = as.numeric(gsub(",", "", Dollars))) %>% 
  mutate(UnitValue = Dollars / Pounds) %>%
  mutate(MetricTons = Pounds * 0.00045359237) %>%
  relocate(State, Species, MetricTons, Pounds, Dollars, UnitValue, .after = YR)


# On Crystal's computer
write_csv(USLandings_updated_combined_clean, "USLandings.csv")

# On H: Drive
#(USLandings_updated_combined_clean, "Data/2008-2019_data SOURCE UPDATED/USLandings.csv")