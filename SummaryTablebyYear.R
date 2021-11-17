#Functions to create annual summary tables for pelagic species profiles.
#Not reproduced in manuscript, but calculations used in manuscript text
#& final function used for calculations in ConsumptionTrends.R.

#--------------
# Annual Hawai'i landings and trade
#--------------
#Hawai'i landings and trade
Annual_Hawaii_Trade_Fun <- function(Spec, SpecAbb) {
  y <- dat %>%
    mutate(Species = recode(Species, "Broadbill swordfish" = "Swordfish",
                            "Swordfishes (only one valid species)" = "Swordfish")) %>%
    filter(YR %in% c(2008:2019)) %>%
    filter(Trade != "R") %>%
    filter(grepl(Spec, Species, ignore.case = TRUE)) %>%
    group_by(YR, Trade, Form) %>%
    summarize(Volume = round(sum(cPounds)),
              Value = round(sum(RDollars))) %>%
    mutate(Species = ifelse(Trade == "I", "Hawaii_Imports",
                            ifelse(Trade == "E", "Hawaii_Exports", ""))) %>%
    ungroup(YR, Trade, Form) %>%
    select(-Trade) %>%
    select(YR, Species, everything())
  
  y$Form <- as.character(y$Form)
  
  assign(paste("Hawaii", SpecAbb, "Annual_Trade", sep = "_"), y)
}

# Hawai`i landings, then combines HI landings and
Annual_Hawaii_Summary_Fun <- function(Spec, SpecAbb) {
  Y <- Landat %>%
    mutate(Species = recode(Species,
      "Dolphinfish (unspecified)" = "MAHIMAHI")) %>%
    filter(grepl(Spec, Species, ignore.case = TRUE)) %>%
    filter(YR %in% c(2008:2019)) %>%
    group_by(YR, Species) %>%
    summarize(Volume = round(sum(Sold)), 
              Value = round(sum(Value))) %>%
    mutate(Species = "Hawaii_Production", Form = "Fresh") %>%
    ungroup() %>%
    select(YR, Species, Form, everything())
  
  assign(paste("Hawaii", SpecAbb, "Annual_Trade&Prod", sep = "_"),
         rbind(Y, Annual_Hawaii_Trade_Fun(Spec, SpecAbb)))
}


#--------------
# Continental US landings and trade
#--------------
# Continental US trade
Annual_ML_Trade_Fun <- function(Spec, SpecAbb) {
  assign(paste(SpecAbb, "Annual_MLTrade", sep = "_"), USdat %>%
    filter(YR %in% c(2008:2019)) %>%
    filter(Trade != "R") %>%
    filter(District != "HONOLULU, HI") %>%
    filter(grepl(Spec, Species, ignore.case = TRUE)) %>%
    group_by(YR, Species, Form, Trade) %>% 
    summarize(Volume = round(sum(cPounds)),
              Value = round(sum(RDollars))) %>%
    ungroup() %>% 
    mutate(Species = ifelse(Trade == "I", "ML_Imports",
                            ifelse(Trade == "E", "ML_Exports", ""))) %>% 
    select(-Trade))
}


# Continental US landings, then combined
Annual_ML_Summary_fun <- function(Spec, SpecAbb) {
  Y <- USLanddat %>%
    filter(grepl(SpecAbb, Species, ignore.case = TRUE)) %>%
    group_by(YR, State) %>%
    filter(YR %in% c(2008:2019)) %>%
    summarize(Volume = round(sum(Pounds), 0), 
              Value = round(sum(Dollars))) %>%
    gather(Var, num, -YR, -State) %>%
    spread(State, num) %>%
    mutate(Mainland = All) %>%
    select(-All, -Hawaii) %>%
    spread(Var, Mainland) %>%
    mutate(Species = "ML_Production", Form = "Fresh") %>%
    ungroup() %>%
    select(YR, Species, Form, Volume, Value)
  
  assign(paste("Mainland", SpecAbb, "Annual_Trade&Prod", sep = "_"),
         rbind(Y, Annual_ML_Trade_Fun(Spec, SpecAbb)))
}


#--------------
# Combine and save Summary table.
#--------------
Annual_US_Summary_fun <- function(Spec, SpecAbb) {
  y <- assign("US_Annual", rbind(Annual_Hawaii_Summary_Fun(Spec, SpecAbb),
                                 Annual_ML_Summary_fun(Spec, SpecAbb)),
              envir = .GlobalEnv)
  
  write.csv(y, paste("Tables/US_Annual_Summary", SpecAbb, ".csv"),
            row.names = TRUE)
}