#This script calculates the share of total Hawai'i seafood market from pelagic 
#species (all species combined both Hawai'i landings and imports)

rm(list = ls())
getwd()
source("Rscripts/SourceFiles/Installations.R")

#----------------------------------------------
#1.Calculate Hawai'i commercial total estimated market supply

totalhilandings <- Landat %>%
  filter(YR > 2007 & YR < 2020) %>%
  mutate(totalvolume = sum(Sold)) %>%
  mutate(totalvalue = sum(Value))

totalhiimports <- dat %>%
  filter(YR > 2007 & YR < 2020) %>%
  filter(Trade == "I") %>%
  mutate(totalvolume = sum(cPounds)) %>%
  mutate(totalvalue = sum(RDollars))

totalhiexports <- dat %>%
  filter(YR > 2007 & YR < 2020) %>%
  filter(Trade == "E") %>%
  mutate(totalvolume = sum(cPounds)) %>%
  mutate(totalvalue = sum(RDollars))

#A = Total Hawaii commercial landings + imports - exports
totalhimarketsupply_vol <- totalhilandings$totalvolume[1] + 
  totalhiimports$totalvolume[1] - totalhiexports$totalvolume[1]

totalhimarketsupply_val <- totalhilandings$totalvalue[1] + 
  totalhiimports$totalvalue[1] - totalhiexports$totalvalue[1]


#----------------------------------------------
#2.Calculate Hawai'i commercial pelagic species market supply. 

pelagicspecies <- c("tuna|marlin|swordfish|dolphinfish|mahimahi|moonfish|opah|monchong|pomfret|wahoo|spearfish|sailfish|thresher|squid|shortfin mako|escolar|shark") 

pelagichilandings <- Landat %>%
  filter(YR > 2007 & YR < 2020) %>%
  filter(grepl(pelagicspecies, Species, ignore.case = TRUE)) %>%
  filter(Species != "Frigate mackerel, Frigate tuna",
         Species != "Sharks (misc.): Spiny dogfish, Green-eyed sharks") %>%
  mutate(totalvolume = sum(Sold)) %>%
  mutate(totalvalue = sum(Value))


pelagichiimports <- dat %>%
  filter(YR > 2007 & YR < 2020) %>%
  filter(Trade == "I") %>%
  filter(grepl(pelagicspecies, Species, ignore.case = TRUE)) %>%
  mutate(totalvolume = sum(cPounds)) %>%
  mutate(totalvalue = sum(RDollars))
  

pelagichiexports <- dat %>%
  filter(YR > 2007 & YR < 2020) %>%
  filter(Trade == "E") %>%
  filter(grepl(pelagicspecies, Species, ignore.case = TRUE)) %>%
  mutate(totalvolume = sum(cPounds)) %>%
  mutate(totalvalue = sum(RDollars)) 


#B = total Hawai'i commercial pelagic landings + imports - exports
pelagichimarketsupply_vol <- pelagichilandings$totalvolume[1] + 
  pelagichiimports$totalvolume[1] - pelagichiexports$totalvolume[1]

pelagichimarketsupply_val <- pelagichilandings$totalvalue[1] + 
  pelagichiimports$totalvalue[1] - pelagichiexports$totalvalue[1]


#----------------------------------------------
#3.Calculate Hawai'i pelagic market share: B / A

pelagicmarketshare_vol <- 
  (pelagichimarketsupply_vol / totalhimarketsupply_vol) * 100
  
pelagicmarketshare_val <- 
  (pelagichimarketsupply_val / totalhimarketsupply_val) * 100



#----------------------------------------------
#----------------------------------------------
#Supplemental resource: list of all species landed in Hawai'i commercial fisheries

landedspecies <- 
  unique(Landat$Species) 

write.csv(landedspecies, "Tables/HIlandedspecies.csv")