#Functions that create average annual supply and value of species in the
#pelagic species profiles.


#--------------
#Average annual Hawai'i landings and trade
#--------------

#Hawai'i trade
#10/11: Make sure exports display raw volume, imports display converted volume
Hawaii_Trade_Fun <- function(Spec, SpecAbb) {
  Y <- dat %>%
    mutate(Species = recode(Species, "Broadbill swordfish" = "Swordfish",
                            "Swordfishes (only one valid species)" = "Swordfish")) %>%
    filter(YR > 2007 & YR < 2020) %>%
    filter(Trade != "R") %>%
    filter(grepl(Spec, Species, ignore.case = TRUE)) %>%
    group_by(YR, Trade, FormDesc) %>%
    summarize(pounds = sum(cPounds), 
              dollars = sum(RDollars), 
              rawVol = sum(Pounds)) %>%
    group_by(Trade, FormDesc) %>%
    summarize(Volume = round(mean(pounds)), 
              Value = round(mean(dollars)), 
              rawVol = round(mean(rawVol))) %>%
    mutate(Price = round(Value / Volume, 2)) %>%  
    mutate(Species = ifelse(Trade == "I", "Hawaii Imports", 
                             ifelse(Trade == "E", "Hawaii Exports", ""))) %>%
    ungroup() %>%
    select(-Trade, -rawVol) %>%
    select(Species, everything())
  names(Y)[2] <- "Form"
  assign(paste("Hawaii", SpecAbb, "Trade", sep = "_"), Y, envir = .GlobalEnv)
}



#Hawai'i landings, then combined with Hawai'i trade
Hawaii_Summary_Fun <- function(Spec, SpecAbb) {
  Y <- Landat %>%
    mutate(Species = recode(Species, "Dolphinfish (unspecified)" = "Mahimahi", 
                            "Swordfishes (only one valid species)" = "Swordfish",
                            "Broadbill swordfish" = "Swordfish")) %>%
    drop_na(Price) %>%
    filter(grepl(Spec, Species, ignore.case = TRUE)) %>%
    filter(YR > 2007 & YR < 2020) %>%
    group_by(Species) %>%
    summarize(Volume = round(mean(Sold)), 
              Value = round(mean(Value)), 
              Price = round(Value / Volume, 2)) %>%
    mutate(Species = "Hawaii Production", Form = "Fresh") %>%
    select(Species, Form, everything())
  assign(paste("Hawaii", SpecAbb, "Trade&Prod", sep = "_"), 
         rbind(Y, Hawaii_Trade_Fun(Spec, SpecAbb)), 
         envir = .GlobalEnv)
}



#--------------
#Average annual continental US landings and trade.
#--------------

#Continental US trade
#10/11: Make sure exports display raw volume, imports display converted volume
ML_Trade_Fun <- function(Spec, SpecAbb) {
  Y <- USdat %>%
    filter(YR > 2007 & YR < 2020) %>%
    filter(Trade != "R") %>%
    filter(District != "HONOLULU, HI") %>%
    filter(grepl(Spec, Species, ignore.case = TRUE)) %>%
    group_by(YR, Trade, FormDesc) %>%
    summarize(pounds = round(sum(cPounds)), 
              dollars = round(sum(RDollars)), 
              rawVol = round(sum(Pounds))) %>%
    group_by(Trade, FormDesc) %>%
    summarize(Volume = round(mean(pounds)), 
              Value = round(mean(dollars)), 
              rawVol = round(mean(rawVol))) %>%
    ungroup() %>%
    mutate(Price = round(Value / Volume, 2)) %>%  
    mutate(Species = ifelse(Trade == "I", "US Imports", 
                            ifelse(Trade == "E", "US Exports", ""))) %>%
    select(-Trade, -rawVol)
  names(Y)[1] <- "Form"
  assign(paste("TradeML", SpecAbb, sep = "_"), Y, envir = .GlobalEnv)
}


#Continental US landings, then combined with continental US trade
Mainland_Summary_fun <- function(Spec, SpecAbb) {
  Y <- USLanddat %>%
    filter(grepl(SpecAbb, Species, ignore.case = TRUE)) %>%
    group_by(State) %>%
    filter(YR > 2007 & YR < 2020) %>%
    summarize(Volume = round(mean(Pounds)), 
              Value = round(mean(Dollars)), 
              Price = round(Value / Volume, 2)) %>%
    gather(Var, num, -State) %>%
    spread(State, num) %>%
    mutate(Mainland = All) %>%
    ungroup() %>%
    select(-"All", -"Hawaii") %>%
    spread(Var, Mainland) %>%
    mutate(Species = "Mainland Production", 
           Form = "Fresh", 
           Price = round(Value / Volume, 2)) %>%
    select(Species, Form, Volume, Value, Price)
  assign(paste("Mainland", SpecAbb, sep = "_"), 
         rbind(Y, ML_Trade_Fun(Spec, SpecAbb)), envir = .GlobalEnv)
}


#--------------
#Save average annual supply and value table.
#--------------
US_Summary_fun <- function(Spec, SpecAbb) {
  y <- assign(paste("Average_US", SpecAbb, sep = "_"), 
              rbind(Hawaii_Summary_Fun(Spec, SpecAbb), 
                    Mainland_Summary_fun(Spec, SpecAbb)), 
              envir = .GlobalEnv)
  write.csv(y, paste("Tables/US", SpecAbb, ".csv"), row.names = TRUE)
}
