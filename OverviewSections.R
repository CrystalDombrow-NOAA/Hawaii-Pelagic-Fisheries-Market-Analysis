#This scripts generates the tables and figures for the Hawai`i HMS Fisheries
#Market Analysis report. This .R file produces the tables and figures for the 
#overview sections.

rm(list = ls())
getwd()
source("Rscripts/SourceFiles/Installations.R")
source("Rscripts/SourceFiles/ColorFunctionForFigures.R")


#Load libraries
library(scales)
library(tidyverse)
library(cowplot)
library(nmfspalette)
library(RColorBrewer)
library(wesanderson)


#Create objects to be used in subsequent figures and tables.
Pelagic_Species_Names <- c("tuna|marlin|swordfish|dolphinfish|mahimahi|moonfish|opah|monchong|pomfret|wahoo|spearfish|sailfish|thresher|squid|shortfin mako|escolar|oceanic whitetip|blue shark")

TradeSpecies <- c("tuna|marlin|swordfish|dolphinfish|mahimahi|moonfish|opah|monchong|pomfret|wahoo|spearfish|sailfish|thresher|squid|shortfin mako|escolar|shark") 


#_____________________________________________________________________________
#HAWAI'I PELAGIC LANDINGS

#-------------------- 
#Figure 3: Share of top Hawai'i landings sold, 2019. 
#--------------------
#Create objects for bar chart.
TopLandedLbs <- Landat %>%
  filter(YR == 2019) %>%
  filter(grepl(Pelagic_Species_Names, Species, ignore.case = TRUE)) %>%
  mutate(Species = recode(Species,
                          "Broadbill swordfish" = "Swordfish",
                          "Swordfishes (only one valid species)" = "Swordfish")) %>%
  #Yes, I'm aware the object is still producing two "swordfish" observations,
  #the smaller in volume has no "Sold" volume and thus it does not impact our
  #calculations, so to spare my emotional state I will not keep trying to fix it =D
  group_by(Species) %>%
  mutate(Species = recode(Species,
                          "Moonfish" = "Opah",
                          "Indo-Pacific blue marlin" = "Blue marlin",
                          "Dolphinfish (unspecified)" = "Mahimahi",
                          "Sickle pomfret" = "Pomfret")) %>%
  ungroup(.) %>%
  mutate(TotalLbs = sum(Sold), TotalValue = sum(Value)) %>%
  mutate(perlbs = Sold / TotalLbs * 100, 
         pervalue = Value / TotalValue * 100) %>%
  mutate(rank = rank(desc(perlbs))) %>%
  filter(rank <= 10) %>%
  arrange(rank) %>%
  mutate(Volume = round(perlbs), Value = round(pervalue)) %>%
  select("Species", "Volume", "Value") %>%
  gather(variable, value, -Species)


#Reorder object.
TopLandedLbs$Species <- reorder(TopLandedLbs$Species, -TopLandedLbs$value)

#Expand color palette for bar chart
cols <- 10
extracolors.top10 <- colorRampPalette(brewer.pal(8, "Blues"))(cols)

#Create bar chart.
TopLandedSpecies_HI <- ggplot(TopLandedLbs, aes(x = Species, y = value, 
                                                fill = Species)) +
  geom_bar(stat = "identity", colour = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 75)) +
  facet_grid(~variable) +
  geom_text(aes(label = value, family = "serif"),
            size = 3.5, hjust = 0.5,
            vjust = -.5) +
  ylab("Percent") +
  xlab("Species") +
  #scale_fill_manual(values = wes_palette("Zissou1", 10, type = "continuous")) +
  scale_fill_manual(values = rev(extracolors.top10)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "black")) +
  theme(legend.title = element_blank(),
        text = element_text(size = 12, family = "serif"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#Save figure.
save_plot("Figures/TopLandedSpecies_VolValbar_HI.png", TopLandedSpecies_HI,
          base_aspect_ratio = 2)


#--------------------
# Table 1: Total volume sold (in thousands of lb) of Hawai'i landings.
#--------------------
#Create table.
Annual_Landings_bySpecies <- Landat %>%
  filter(YR > 2007) %>%
  filter(grepl(Pelagic_Species_Names, Species, ignore.case = TRUE)) %>%
  filter(Species != "Frigate mackerel, Frigate tuna") %>%
  mutate(Species = recode(Species, "Broadbill swordfish" = "Swordfish",
                          "Swordfishes (only one valid species)" = "Swordfish",
                          "Dolphinfish (unspecified)" = "Mahimahi", 
                          "Indo-Pacific blue marlin" = "Blue marlin",
                          "Moonfish" = "Opah", 
                          "Pacific/Giant black marlin, Silver marlin" = 
                            "Black marlin, Silver marlin",
                          "Shortbill/Shortnose spearfish" = "Shortbill spearfish",
                          "Sickle pomfret" = "Pomfret", 
                          "Kawakawa, Little tuna" = "Kawakawa",
                          "Indo-Pacific sailfish" = "Sailfish",
                          "Oilfish, Escolar (Spanish)" = "Escolar",
                          "Squids (unspecified)" = "Squid (unspecified)",
                          "Atlantic bluefin tuna" = "Bluefin tuna",
                          "Oceanic whitetip shark (fins)" = 
                            "Oceanic whitetip shark")) %>%
  group_by(YR, Species) %>%
  summarize(Volume = round(sum(Sold) / 1000)) %>%
  select(-0) %>%
  spread(YR, Volume)

#Save table.
write.csv(Annual_Landings_bySpecies, "Tables/Annual_Landings_bySpecies.csv")


#--------------------
# Figure 2: Total volume sold and value of Hawai'i pelagic landings.

#Bigeye, yellowfin, opah, swordfish, wahoo, blue marlin, mahimahi, pomfret, 
#striped marlin, skipjack tuna
#--------------------
#Create objects for ggplot
Annual_Landings_bySpecies_forplot <- Landat %>%
  filter(YR > 2007) %>%
  filter(grepl(Pelagic_Species_Names, Species, ignore.case = TRUE)) %>%
  filter(Species != "Frigate mackerel, Frigate tuna",
         Species != "Atlantic bluefin tuna",
         Species != "Blue shark",
         Species != "Oceanic whitetip shark (fins)") %>%
  #Removing these species because they're caught but not sold from 2008-2019,
  #and their inclusion in this graph takes up unnecessary space in the legend
  #that is creating a formatting unpleasantry.
  group_by(YR, Species) %>%
  mutate(Species = if_else(grepl("swordfish", Species, ignore.case = TRUE),
                           "Swordfish", Species)) %>%
  #Create other category to group species of lowest volume for visual legibility
  mutate(Species = recode(Species,
                          "Albacore tuna" = "Other",
                          "Indo-Pacific sailfish" = "Other",
                          "Kawakawa, Little tuna" = "Other",
                          "Oilfish, Escolar (Spanish)" = "Other",
                          "Shortbill/Shortnose spearfish" = "Other",
                          "Shortfin mako shark" = "Other",
                          "Squids (unspecified)" = "Other",
                          "Thresher shark (unspecified)" = "Other",
                          "Pacific/Giant black marlin, Silver marlin"
                          = "Other")) %>%
  summarize(Volume = sum(Sold) / 1000000, value = sum(Value) / 1000000) %>%
  group_by(YR) %>%
  mutate(TotAnValue = sum(value)) %>%
  mutate(Species = recode(Species,
                          "Dolphinfish (unspecified)" = "Mahimahi",
                          "Indo-Pacific blue marlin" = "Blue marlin",
                          "Moonfish" = "Opah",
                          "Sickle pomfret" = "Pomfret")) %>%
  mutate(totalvolume = sum(Volume))


#Expand color palette for bar chart
cols <- 11
extracolors <- colorRampPalette(brewer.pal(11, "Blues"))(cols)

#Create stacked bar chart.
Annual_Landings_bySpecies_plot_withvalue <- ggplot(Annual_Landings_bySpecies_forplot) +
  geom_bar(aes(y = Volume, x = YR, fill = Species), stat = "identity",
           colour = "black", position = "stack") +
  geom_line(aes(y = TotAnValue / 3, x = YR, 
                color = "TotAnValue"), 
            stat = "identity",
            size = 1, color = "black") +
  labs(x = "Year", y = "Total Pounds Sold (million lb.)") +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "black")) +
  scale_x_continuous(breaks = c(2008:2019)) +
  scale_y_continuous(expand = c(0, 0), limit = c(0, 45), labels = comma,
                     sec.axis = sec_axis(~ . * 3, name = "Total Revenue ($ million)",
                                         label = dollar_format())) +
  scale_fill_manual(values = rev(extracolors)) +
  theme(legend.position = "right", legend.title = element_blank(), 
        legend.justification = "center", 
        text = element_text(size = 12, family = "serif"))

#Save figure.
save_plot("Figures/Annual_Landings_bySpecies_withvalue_plot.png",
          Annual_Landings_bySpecies_plot_withvalue, 
          base_aspect_ratio = 2, base_height = 5)


#_____________________________________________________________________________
#PELAGIC EXPORTS FROM HAWAI'I

#--------------------
#Table 2: Total Hawai'i landings sold and exports of PMUS.  
#--------------------
#Create object for table.
Pelagic_Prod_Annual_E <- Landat %>%
  filter(YR > "2007") %>%
  filter(Species != "Frigate mackerel, Frigate tuna",
         Species != "Sharks (misc.): Spiny dogfish, Green-eyed sharks") %>%
  filter(grepl(TradeSpecies, Species, ignore.case = TRUE)) %>%
  group_by(YR) %>%
  summarize(Volume = sum(Sold), Value = sum(Value), 
            UnitValue = round(Value / Volume, 2))

#Create another object for table.
Pelagic_Exp_Annual <- dat %>%
  filter(YR > 2007 & YR < 2020) %>%
  filter(Trade == "E") %>%
  filter(grepl(TradeSpecies, Species, ignore.case = TRUE)) %>%
  group_by(YR) %>%
  summarize(VolumeExp = round(sum(cPounds, na.rm = TRUE)),
            ValueExp = round(sum(RDollars, na.rm = TRUE)),
            UnitValueExp = round(ValueExp / VolumeExp, 2))
#Yes, I rounded VolumeExp and ValueExp calculations; the UnitValue results
#are the same with or without rounded inputs. Less formatting to copy table to
#the manuscript this way.

#Create table.
Exp_Prod <- cbind(Pelagic_Prod_Annual_E, Pelagic_Exp_Annual[, 2:4]) %>%
  mutate(ExpVolShare = round(VolumeExp / Volume * 100, 1),
         ExpValShare = round(ValueExp / Value * 100, 1))

#Save table.
write.csv(Exp_Prod, "Tables/Exp_Prod.csv")


#--------------------
#Table 3: Average annual Hawai'i landings sold and exports of PMUS, 2008-2019.  
#--------------------
#Create object for table.
Pelagic_Prod_Species <- Landat %>%
  mutate(Species = if_else(grepl("swordfish", Species, ignore.case = TRUE),
                           "swordfish", Species),
         Species = if_else(grepl("squid", Species, ignore.case = TRUE),
                           "squid (unspecified)", Species)) %>%
  filter(YR > "2007") %>%
  filter(grepl(TradeSpecies, Species, ignore.case = TRUE)) %>%
  group_by(Species) %>%
  summarize(Volume = round(mean(Sold, na.rm = TRUE)), 
            Value = round(mean(Value, na.rm = TRUE)),
            UnitValue = round(Value / Volume, 2))
#Yes, I rounded Volume and Value calculations; UnitValue calculations are the 
#same as not-rounded. Less formatting to copy table to manuscript this way.

#Put column to lowercase.
Pelagic_Prod_Species$Species <- tolower(Pelagic_Prod_Species$Species)

#Create another object for table.
Pelagic_Exp_Species <- dat %>%
  filter(YR > 2007) %>%
  mutate(Species = if_else(grepl("squid", Species, ignore.case = TRUE),
                           "squid (unspecified)", Species)) %>%
  filter(Trade == "E") %>%
  filter(grepl(TradeSpecies, Species, ignore.case = TRUE)) %>%
  group_by(YR, Species) %>%
  summarize(TotVol = round(sum(cPounds, na.rm = TRUE)),
            TotVal = round(sum(RDollars, na.rm = TRUE))) %>%
  group_by(Species) %>%
  summarize(VolumeExp = round(mean(TotVol, na.rm = TRUE)),
            ValueExp = round(mean(TotVal, na.rm = TRUE))) %>%
  mutate(UnitValueExp = round(ValueExp / VolumeExp, 2))

#Put characters in $Species column to lowercase.
Pelagic_Exp_Species$Species <- tolower(Pelagic_Exp_Species$Species)

#Create table.
ProdExp_Species <- Pelagic_Prod_Species %>%
  left_join(Pelagic_Exp_Species, by = "Species") %>%
  na.omit() %>%
  mutate(ExpVolShare = round(VolumeExp / Volume * 100, 1),
         ExpValShare = round(ValueExp / Value * 100, 1)) %>%
  arrange(-Volume)

#Save table.
write.csv(ProdExp_Species, "Tables/ProdExp_bySpecies.csv")



#--------------------
#Table 4: Total PMUS export volume from Hawai'i (lb). 
#--------------------
#Create table.
Export_by_SpeciesYear <- dat %>%
  filter(YR > 2007) %>%
  filter(Trade == "E") %>%
  filter(grepl(TradeSpecies, Species, ignore.case = TRUE)) %>%
  mutate(Species = recode(Species, "SQUID" = "Unspecified squid")) %>%
  group_by(YR, Species) %>%
  summarize(TotVol = round(sum(cPounds, na.rm = TRUE)),
            TotVal = round(sum(RDollars, na.rm = TRUE))) %>%
  select(-TotVal) %>%
  spread(YR, TotVol)

#Put characters in $Species column to lowercase.
Export_by_SpeciesYear$Species <- tolower(Export_by_SpeciesYear$Species)

#Replace NAs with 0
Export_by_SpeciesYear[is.na(Export_by_SpeciesYear)] <- 0

#Save table.
write.csv(Export_by_SpeciesYear, "Tables/Export_by_SpeciesYear.csv")



#--------------------
#Figure 4: Total PMUS exports from Hawai'i. 
#--------------------
#Create objects for bar chart.
Exp_Plot <- dat %>%
  filter(YR > 2007 & Trade == "E") %>%
  filter(grepl(TradeSpecies, Species, ignore.case = TRUE)) %>%
  group_by(YR) %>%
  summarize(VolumeExp = round(sum(cPounds, na.rm = TRUE)),
            ValueExp = round(sum(RDollars, na.rm = TRUE))) %>%
  select(YR, VolumeExp, ValueExp)


#Create bar chart.
Exp_ggPlot <- ggplot(Exp_Plot) +
  geom_bar(aes(y = VolumeExp / 1000000, x = YR, fill = "VolumeExp"),
           stat = "identity", color = "black", position = "stack", width = .5) +
  geom_line(aes(y = ValueExp / 3000000, x = YR, color = "Value"), 
            stat = "identity", size = 1) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "black")) +
  labs(x = "Year", y = "Total Pounds Exported (million lb.)") +
  scale_x_continuous(breaks = c(2008:2019)) +
  scale_y_continuous(expand = c(0, 0), label = comma, 
                     sec.axis = sec_axis(~ . * 3, 
                                         name = "Total Revenue ($ million)",
                                         label = dollar_format())) +
  coord_cartesian(ylim = c(0, 1.8)) +
  scale_fill_manual(labels = c("Volume"), values = wescol5[c(3)]) +
  scale_color_manual(labels = c("Value"), values = wescol5[c(4)]) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        legend.justification = "center", 
        text = element_text(size = 12, family = "serif"))

#Save figure.
save_plot("Figures/PelExports.png", Exp_ggPlot, base_aspect_ratio = 1.5)



#--------------------
#Table 5: Average annual exports from Hawai'i of PMUS, 2008-2019.
#--------------------
#Function for subsequent table.
SpcCntyTrd_Tbl <- function(dat, datname) {
  y <- dat %>%
    group_by(YR, Species, Country) %>%
    summarize(Pounds = round(sum(cPounds), 0), 
              RDollars = round(sum(RDollars), 0), 
              Price = RDollars / Pounds) %>%
    group_by(Species, Country) %>%
    summarize(Pounds = round(mean(Pounds, na.rm = TRUE, 0)), 
              RDollars = round(mean(RDollars, na.rm = TRUE, 0)), 
              Price = round(mean(RDollars / Pounds), 2)) %>%
    group_by(Species) %>%
    mutate("Share Volume" = round(Pounds / sum(Pounds) * 100, 1)) %>%
    mutate("Share Value" = round(RDollars / sum(RDollars) * 100, 1)) %>%
    mutate("rank" = max(Pounds))
  y <- y[order(-y$rank, -y$Pounds), ]
  write.csv(y, paste("Tables/", datname, ".csv"), row.names = TRUE)
  y$rank <- NULL
  assign(paste("SCE", datname, sep = ""), as.data.frame(y), envir = .GlobalEnv)
  return(datname)
}

#Create table. This saves as " PelagicTrade_Table .csv"
PelagicTrade <- dat %>%
  filter(YR > 2007) %>%
  filter(Trade == "E") %>%
  filter(grepl(TradeSpecies, Species, ignore.case = TRUE)) %>%
  #Wrote additional code to look at annual trends:
  select(Species, Country, YR, RDollars, cPounds, Price) %>%
  group_by(YR, Species, Country) %>%
  summarize(total = sum(cPounds)) 
  SpcCntyTrd_Tbl("PelagicTrade_Table")

#Apologies, I am far too tired to fuss with species order and making them 
#lowercase in the resulting table.


#______________________________________________________________________________
#PELAGIC IMPORTS TO HAWAI'I

#--------------------
#Table 6: Total Hawai'i landings sold and imports of PMUS. 
#--------------------
#Create object for table.
Pelagic_Prod_Annual_I <- Landat %>%
  filter(YR > "2007") %>%
  filter(Species != "Frigate mackerel, Frigate tuna",
         Species != "Sharks (misc.): Spiny dogfish, Green-eyed sharks") %>%
  filter(grepl(TradeSpecies, Species, ignore.case = TRUE)) %>%
  group_by(YR) %>%
  summarize(Volume = sum(Sold), Value = sum(Value), 
            UnitValue = round(Value / Volume, 2))

#Create another object for table.
Pelagic_Imp_Annual <- dat %>%
  filter(YR > 2007 & YR < 2020) %>%
  filter(Trade == "I") %>%
  filter(grepl(TradeSpecies, Species, ignore.case = TRUE)) %>%
  group_by(YR) %>%
  summarize(VolumeImp = round(sum(cPounds, na.rm = TRUE)),
            ValueImp = round(sum(RDollars, na.rm = TRUE)),
            UnitValueImp = round(ValueImp / VolumeImp, 2))

#Combine objects in table.
Imp_Prod <- cbind(Pelagic_Prod_Annual_I, Pelagic_Imp_Annual[, 2:4]) %>%
  mutate(ImpVolShare = round(VolumeImp / (VolumeImp + Volume) * 100, 1),
         ImpValShare = round(ValueImp / (ValueImp + Value) * 100, 1))

#Save table.
write.csv(Imp_Prod, "Tables/Imp_Prod.csv")


#--------------------
#Table 7. Average annual Hawai'i landings sold and imports of PMUS, 2008-2019. 
#--------------------
#Create object for table.
Pelagic_Prod_Species <- Landat %>%
  filter(YR > "2007") %>%
  mutate(Species = if_else(grepl("swordfish", Species, ignore.case = TRUE),
                           "swordfish", Species),
         Species = if_else(grepl("squid", Species, ignore.case = TRUE),
                           "squid (unspecified)", Species),
         Species = if_else(grepl("dolphinfish", Species, ignore.case = TRUE),
                           "mahimahi", Species)) %>%
  filter(grepl(TradeSpecies, Species, ignore.case = TRUE)) %>%
  group_by(Species) %>%
  summarize(Volume = round(mean(Sold, na.rm = TRUE)), 
            Value = round(mean(Value, na.rm = TRUE)),
            UnitValue = round(Value / Volume, 2))
#Yes, I rounded Volume and Value calculations; UnitValue calculations are the 
#same as not-rounded. Less formatting to copy table to manuscript this way.


#Put $Species column in lowercase
Pelagic_Prod_Species$Species <- tolower(Pelagic_Prod_Species$Species)

#Create another object for table.
Pelagic_Imp_Species <- dat %>%
  filter(YR > 2007) %>%
  mutate(Species = if_else(grepl("swordfish", Species, ignore.case = TRUE),
                           "swordfish", Species),
         Species = if_else(grepl("squid", Species, ignore.case = TRUE),
                           "squid (unspecified)", Species)) %>%
  filter(Trade == "I") %>%
  filter(grepl(TradeSpecies, Species, ignore.case = TRUE)) %>%
  group_by(YR, Species) %>%
  summarize(TotVol = round(sum(cPounds, na.rm = TRUE)), 
            TotVal = round(sum(RDollars, na.rm = TRUE))) %>% 
  group_by(Species) %>%
  summarize(VolumeImp = round(mean(TotVol, na.rm = TRUE)), 
            ValueImp = round(mean(TotVal, na.rm = TRUE)), 
            UnitValueImp = round(ValueImp / VolumeImp, 2))

#Put $Species column in lowercase.
Pelagic_Imp_Species$Species <- tolower(Pelagic_Imp_Species$Species)

#Rename "unspecified tuna".
Pelagic_Imp_Species$Species <- gsub("unspecified tuna", "tunas (unknown)", 
                                    Pelagic_Imp_Species$Species)

#Rename additional species, create calculations for table.
ProdImp_Species <- Pelagic_Prod_Species %>%
  group_by(Species) %>%
  summarize(Volume = round(sum(Volume)), Value = round(sum(Value)), 
            UnitValue = Value / Volume) %>% 
  full_join(Pelagic_Imp_Species, by = "Species") %>%
  drop_na() %>%
  mutate(ImpVolShare = round(VolumeImp / (VolumeImp + Volume) * 100, 1), 
         ImpValShare = round(ValueImp / (ValueImp + Value) * 100, 1))

#Save table.
write.csv(ProdImp_Species, "Tables/ProdImp_bySpecies.csv")



#--------------------
#Table 8: Total PMUS import volume to Hawai'i (in thousands of lb). 
#--------------------
#Create table.
Imports_by_SpeciesYear <- dat %>%
  filter(YR > 2007) %>%
  filter(Trade == "I") %>%
  filter(grepl(TradeSpecies, Species, ignore.case = TRUE)) %>%
  mutate(Species = recode(Species, "SQUID" = "UNSPECIFIED SQUID")) %>%
  group_by(YR, Species) %>%
  summarize(TotVol = round(sum(cPounds / 1000, na.rm = TRUE))) %>%
  spread(YR, TotVol)

#Put characters in $Species column to lowercase.
Imports_by_SpeciesYear$Species <- tolower(Imports_by_SpeciesYear$Species)

#Replace NAs with 0
Imports_by_SpeciesYear[is.na(Imports_by_SpeciesYear)] <- 0

#Save table.
write.csv(Imports_by_SpeciesYear, "Tables/Import_by_SpeciesYear.csv")


#--------------------
#Table 9: Share of PMUS imports to Hawai'i (%). 
#--------------------
#Create object for table.
Pelagic_Imp_Species_byYear <- dat %>%
  filter(YR > 2007) %>%
  filter(Trade == "I") %>%
  filter(grepl(TradeSpecies, Species, ignore.case = TRUE)) %>%
  mutate(Species = recode(Species, "Dolphinfish (unspecified)" = "Mahimahi",
                          "Broadbill swordfish" = "Swordfish",
                          "Swordfishes (only one valid species)" = "Swordfish")) %>%
  group_by(YR, Species) %>%
  summarize(VolumeImp = sum(cPounds, na.rm = TRUE),
            ValueImp = sum(RDollars, na.rm = TRUE)) %>%
  ungroup(.) %>%
  group_by(YR) %>%
  mutate(totalvol = sum(VolumeImp)) %>%
  ungroup(.) %>%
  group_by(YR, Species)

#Put column in lowercase.
Pelagic_Imp_Species_byYear$Species <- tolower(Pelagic_Imp_Species_byYear$Species)

#Adjust object for table.
Imp_byYearSpecies <- Pelagic_Imp_Species_byYear %>%
  unite(ID, YR, Species, sep = " ")

#Put column in lowercase.
Imp_byYearSpecies$ID <- tolower(Imp_byYearSpecies$ID)

#Create table.
ImpShare_Species_byYear <- Imp_byYearSpecies %>%
  mutate(ImpVolShare = round(VolumeImp / totalvol * 100, 1)) %>%
  #If you remove the round(ing) function, the ImpVolShares add up to 100 for 
  #each year 8-)
  separate(ID, c("YR", "Species"),
           extra = "merge",
           fill = "right") %>%
  mutate(Species = recode(Species, "squid" = "unspecified squid")) %>%
  select(YR, Species, ImpVolShare) %>%
  spread(YR, ImpVolShare)

ImpShare_Species_byYear[is.na(ImpShare_Species_byYear)] <- 0

#Save table.
write.csv(ImpShare_Species_byYear, "Tables/ImportShare_byspecies.csv")


#--------------------
#Figure 5: Total PMUS imports to Hawai'i.  
#--------------------
#Create object for figure.
Imp_Plot <- dat %>%
  filter(YR > 2007) %>%
  filter(Trade == "I") %>%
  filter(grepl(TradeSpecies, Species, ignore.case = TRUE)) %>%
  group_by(YR) %>%
  summarize(VolumeImp = round(sum(cPounds, na.rm = TRUE)), 
            ValueImp = round(sum(RDollars, na.rm = TRUE)), 
            UnitValueImp = round(ValueImp / VolumeImp, 2)) %>%
  select(YR, VolumeImp, ValueImp)


#Create bar chart.
Imp_ggPlot <- ggplot(Imp_Plot) +
  geom_bar(aes(y = VolumeImp / 1000000, x = YR, fill = "VolumeImp"), 
           stat = "identity", color = "black", position = "stack", width = .5) +
  geom_line(aes(y = ValueImp / 3000000, x = YR, color = "Value"), 
            stat = "identity", size = 1) +
  labs(x = "Year", y = "Total Pounds Imported (million lb.)") +
  scale_x_continuous(breaks = c(2008:2019)) +
  scale_y_continuous(expand = c(0, 0), limit = c(0, 13), label = comma,
                     sec.axis = sec_axis(~ . * 3, 
                                         name = "Total Value ($ million)", 
                                         label = dollar_format())) +
  scale_fill_manual(labels = c("Volume"), values = wescol5[c(3)]) +
  scale_color_manual(labels = c("Value"), values = wescol5[c(4)]) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(size = .1, color = "black")) +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        text = element_text(size = 12, family = "serif"))

#Save figure.
save_plot("Figures/PelImports.png", Imp_ggPlot, base_aspect_ratio = 1.5)


#--------------------
#Figure 6: Total fresh import volume to Hawai'i.  
#--------------------
#Create object for stacked bar chart.
Fsh_Imp_Pelagic <- dat %>%
  filter(YR %in% c(2008:2019)) %>%
  filter(Trade == "I") %>%
  filter(Form == "Fresh") %>%
  filter(grepl(TradeSpecies, Species, ignore.case = TRUE)) %>%
  mutate(Species = recode(Species, "ALBACORE TUNA" = "Albacore tuna",
                          "BIGEYE TUNA" = "Bigeye tuna",
                          "SWORDFISH" = "Swordfish",
                          "YELLOWFIN TUNA" = "Yellowfin tuna",
                          "SKIPJACK TUNA" = "Skipjack tuna",
                          "UNSPECIFIED TUNA" = "Unspecified tuna",
                          "MAHIMAHI" = "Mahimahi",
                          "BLUEFIN TUNA" = "Bluefin tuna",
                          "SQUID" = "Unspecified squid")) %>%
  group_by(YR, Species) %>%
  summarize(Pounds = sum(cPounds))

#Create stacked bar chart.
Fsh_Imp_Pelagic_gph <- ggplot(Fsh_Imp_Pelagic, aes(x = YR, y = Pounds / 1100000,
                                                   fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "black")) +
  labs(x = "Year", y = "Total Pounds Imported (million lb.)") +
  scale_x_continuous(breaks = seq(2008, 2019, 1)) +
  scale_y_continuous(expand = c(0, 0), labels = comma) +
  coord_cartesian(ylim = c(0, 2.5)) +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "right", legend.title = element_blank(),
        text = element_text(size = 11, family = "serif"))

#Save figure.
save_plot("Figures/Fsh_Imp_Pelagic_gph.png", Fsh_Imp_Pelagic_gph)



#--------------------
#Figure 7: Total frozen import volume to Hawai'i.  
#--------------------
#Create object for stacked bar chart.
Fzn_Imp_Pelagic <- dat %>%
  filter(YR %in% c(2008:2019)) %>%
  filter(Trade == "I") %>%
  filter(grepl("Frozen", Form, ignore.case = TRUE)) %>%
  filter(grepl(TradeSpecies, Species, ignore.case = TRUE)) %>%
  mutate(Species = recode(Species, "MAHIMAHI" = "Mahimahi",
                          "SKIPJACK TUNA" = "Skipjack tuna",
                          "SWORDFISH" = "Swordfish",
                          "UNSPECIFIED TUNA" = "Unspecified tuna",
                          "YELLOWFIN TUNA" = "Yellowfin tuna",
                          "ALBACORE TUNA" = "Albacore tuna",
                          "UNSPECIFIED SHARK" = "Unspecified shark",
                          "SQUID" = "Unspecified squid",
                          "BLUEFIN TUNA" = "Bluefin tuna")) %>%
  group_by(YR, Species) %>%
  summarize(Pounds = sum(cPounds))

#Organize object.
Fzn_Imp_Pelagic$Species <- reorder(Fzn_Imp_Pelagic$Species, 
                                   Fzn_Imp_Pelagic$Pounds)

#Create stacked bar chart.
Fzn_Imp_Pelagic_gph <- ggplot(Fzn_Imp_Pelagic, aes(x = YR, y = Pounds / 1000000,
                                                   fill = Species)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Year", y = "Total Pounds Imported (million lb.)") +
  scale_x_continuous(breaks = seq(2008, 2019, 1)) +
  scale_y_continuous(expand = c(0, 0), limit = c(0, 5.0), labels = comma) +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "right", legend.title = element_blank(),
        text = element_text(size = 12, family = "serif")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "black"))

#Save figure.
save_plot("Figures/Fzn_Imp_Pelagic_gph.png", Fzn_Imp_Pelagic_gph,
          base_aspect_ratio = 1.75)