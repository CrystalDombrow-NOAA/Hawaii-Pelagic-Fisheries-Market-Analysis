#Profile for swordfish. 

rm(list = ls())
source("Rscripts/SourceFiles/Installations.R")
source("Rscripts/SourceFiles/ColorFunctionForFigures.R")


#--------------------
#Table 20. Average annual supply and value of swordfish, 2008-2019.
#--------------------
source("Rscripts/SourceFiles/SummaryTableAnnualAverages.r")
US_Summary_fun("Swordfish", "Swordfish")


#--------------------
#Figure 26. Total domestic consumption of swordfish in Hawai'i and the continental U.S. 
#Figure 27. Annual share of local swordfish consumption in Hawai'i.
#Figure 28. Annual share of domestic swordfish consumption on the continental U.S.
#--------------------
#Not reproduced in manuscript, but calculations used in text & final function
#used for calculations in ConsumptionTrends.R.
source("Rscripts/SourceFiles/SummaryTablebyYear.r")
Annual_US_Summary_fun("Swordfish", "Swordfish")

source("Rscripts/SourceFiles/ConsumptionTrends.r")



#--------------------
#Figure 29. Average volume and value of swordfish landed in Hawai'i.
#--------------------
library(scales)

#Create object for figure.
SwordfishProdVolVal <- Landat %>%
  filter(grepl("swordfish", Species, ignore.case = TRUE)) %>%
  mutate(Species = "Swordfish") %>%
  filter(YR > 2007) %>%
  group_by(YR, Species) %>%
  summarize(PoundsSold = round(mean(Sold)), 
            Dollars = round(mean(Value)), 
            Price = round(Dollars / PoundsSold, 2))

#Create figure.
Swordfish_volval_HIprod <- ggplot(SwordfishProdVolVal) +
  geom_bar(aes(y = PoundsSold, x = YR, fill = "Pounds Sold"), 
           stat = "identity", color = "black", position = "stack") +
  geom_line(aes(y = Dollars / 4, x = YR, color = "Dollar"), 
            stat = "identity", size = 1) +
  labs(x = "Year", y = "Average Pounds Sold (lb.)") +
  scale_x_continuous(breaks = seq(2008, 2019, 1)) +
  scale_y_continuous(expand = c(0, 0), labels = comma, 
                     sec.axis = sec_axis(~ . * 4, name = "Average Revenue ($)", 
                                         label = dollar_format())) +
  coord_cartesian(ylim = c(0, 4000000)) +
  scale_fill_manual(labels = c("Volume"), values = wescol5[c(3)]) +
  scale_color_manual(labels = c("Value"), values = wescol5[c(4)]) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "black")) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        text = element_text(size = 12, family = "serif"))

#Save figure.
save_plot("Figures/Swordfish_volval_HIprod.png", Swordfish_volval_HIprod, 
          base_aspect_ratio = 1.75)



#--------------------
#Figure 30. Monthly average ex-vessel prices (per pound) for Hawai'i 
#swordfish landings. 
#--------------------
scaleFUN <- function(x) sprintf("$%.2f", x)

library(scales)
require(lubridate)

#Create object for graph.
start <- ymd("2008/01/01")

#Create another object for graph.
MonthlyPriceSW <- Monthdat %>%
  mutate(date = start + months(0:143), year = year(date), 
         month = month(date, label = T, abbr = T)) %>%
  select(date, year, month, Swordfish) %>%
  group_by(date, year, month)

#Create graph.
ExVessPrice_Col <- ggplot(MonthlyPriceSW, aes(x = date, y = Swordfish, 
                                              group = 1)) +
  geom_line(stat = "identity") +
  geom_point() +
  scale_y_continuous(labels = scaleFUN) +
  labs(x = "Year", y = "Price") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y", expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        text = element_text(size = 12, family = "serif"), 
        axis.text.x = element_text(angle = 45, hjust = 1))

#Save figure.
save_plot("Figures/ExVessPrice_Col_SW.png", ExVessPrice_Col, 
          base_aspect_ratio = 2)


#--------------------
#Figure 31. Total volume and value of swordfish exports from Hawai'i. 
#--------------------
library(cowplot)

#Create object for graph.
Swordfish_Trade_Total <- dat %>%
  mutate(Species = recode(Species,
                          "Broadbill swordfish" = "Swordfish",
                          "Swordfishes (only one valid species)" = "Swordfish")) %>%
  filter(grepl("swordfish", Species, ignore.case = TRUE)) %>%
  filter(YR > 2007 & YR < 2020) %>%
  filter(Trade != "R") %>%
  group_by(YR, Trade) %>%
  summarize(pounds = sum(cPounds), value = sum(RDollars))

#Filter object for graph.
Swordfish_Trade_Total_E <- Swordfish_Trade_Total %>%
  filter(Trade == "E")

#Create graph.
Export_Plot <- ggplot(Swordfish_Trade_Total_E) +
  geom_bar(aes(y = pounds, x = YR, fill = "pounds"), stat = "identity",
           colour = "black") +
  geom_line(aes(y = value / 10, x = YR, color = "value"), stat = "identity",
            size = 1) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "black")) +
  labs(x = "Year", y = "Total Pounds Exported (lb.)") +
  scale_x_continuous(breaks = seq(2008, 2019, 1), limits = c(2007, 2019)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 23000), labels = comma,
                     sec.axis = sec_axis(~ . * 10, name = "Total Revenue ($)",
                                         labels = dollar))+
  scale_fill_manual(labels = c("Volume"), values = wescol5[c(3)]) +
  scale_color_manual(labels = c("Value"), values = wescol5[c(4)]) +
  ggtitle("Exports") +
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.justification = "center",
        text = element_text(size = 12, family = "serif"),
        axis.text.x = element_text(angle = 45, hjust = 1))

#Save figure.
save_plot("Figures/SwordfishExports.png", Export_Plot, base_aspect_ratio = 1.15)



#--------------------
#Figure 33. Total volume and value of swordfish imports to Hawai'i. 
#--------------------
library(cowplot)

#Create object for graph.
Swordfish_Trade_Total <- dat %>%
  mutate(Species = recode(Species,
                          "Broadbill swordfish" = "Swordfish",
                          "Swordfishes (only one valid species)" = "Swordfish")) %>%
  filter(grepl("swordfish", Species, ignore.case = TRUE)) %>%
  filter(YR > 2007 & YR < 2020) %>%
  filter(Trade != "R") %>%
  group_by(YR, Trade) %>%
  summarize(pounds = sum(cPounds), value = sum(RDollars))

#Filter object for graph.
Swordfish_Trade_Total_I <- Swordfish_Trade_Total %>%
  filter(Trade == "I")

#Create graph.
Import_Plot <- ggplot(Swordfish_Trade_Total_I) +
  geom_bar(aes(y = pounds, x = YR, fill = "pounds"), stat = "identity",
           colour = "black") +
  geom_line(aes(y = value / 5, x = YR, color = "value"), stat = "identity",
            size = 1) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "black")) +
  labs(x = "Year", y = "Total Pounds Imported (lb.)") +
  scale_x_continuous(breaks = seq(2008, 2019, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40000), labels = comma,
                     sec.axis = sec_axis(~ . * 5, name = "Total Value ($)", 
                                         labels = dollar)) +
  scale_fill_manual(labels = c("Volume"), values = wescol5[c(3)]) +
  scale_color_manual(labels = c("Value"), values = wescol5[c(4)]) +
  ggtitle("Imports") +
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.justification = "center",
        text = element_text(size = 12, family = "serif"),
        axis.text.x = element_text(angle = 45, hjust = 1))

#Save figure.
save_plot("Figures/SwordfishImports.png", Import_Plot, base_aspect_ratio = 1.15)



#--------------------
#Table 22. Total fresh swordfish imports to Hawai'i.  
#Table 23. Total frozen swordfish imports to Hawai'i. 
#--------------------
library(scales)
library(wesanderson)

#Create object.
Swordfish_Imp_Form <- dat %>%
  filter(grepl("swordfish", Species, ignore.case = TRUE)) %>%
  filter(YR > 2007 & YR < 2020) %>%
  filter(Trade == "I") %>%
  #Comment out this next line when producing tables and figures, for shares
  #calculations only
  mutate(FormDesc = recode(FormDesc, "Fillet Fresh" = "Fresh")) %>%
  group_by(YR, Country, FormDesc) %>%
  mutate(Country = recode(Country, "AUSTRALIA" = "Australia",
                          "NEW ZEALAND" = "New Zealand",
                          "PHILIPPINES" = "Philippines",
                          "COSTA RICA" = "Costa Rica",
                          "INDONESIA" = "Indonesia",
                          "THAILAND" = "Thailand",
                          "FRENCH POLYNESIA" = "French Polynesia",
                          "ECUADOR" = "Ecuador",
                          "SRI LANKA" = "Sri Lanka")) %>%
  summarize(pounds = sum(cPounds), value = sum(RDollars)) %>%
  ungroup(.) %>%
  group_by(YR, FormDesc) %>%
  mutate(totannvol = sum(pounds)) %>%
  mutate(UnitValue = round(value / pounds, 2), .before = totannvol)%>%
  ungroup(.) %>%
  group_by(Country, YR, FormDesc) %>%
  mutate(countryannvolshare = round(mean(pounds / totannvol) * 100, 1)) %>%
  ungroup(.) %>%
  group_by(FormDesc) %>%
  mutate(totvolbyform = sum(unique(totannvol))) %>%
  ungroup(.) %>%
  group_by(Country, FormDesc) %>%
  mutate(countryformtotvol = sum(pounds)) %>%
  mutate(avgsharebycountryform = (countryformtotvol / totvolbyform) * 100) %>%
  mutate(avgsharebycountryform = round(avgsharebycountryform, 1)) %>%
  ungroup(.) %>%
  group_by(YR, FormDesc, Country)

#Coerce to character vector.
Swordfish_Imp_Form$Country <- as.character(Swordfish_Imp_Form$Country)

#Create "Other" category for figure. Comment out for tables.
Swordfish_Imp_Form$Country[Swordfish_Imp_Form$countryannvolshare < 5] <- "Other"

#Save tables.
write.csv(Swordfish_Imp_Form, "Tables/SwordfishImports.csv")



#--------------------
#Figure 34. Total swordfish import volume to Hawai'i. 
#--------------------
#Uses objects created for Tables 22 and 23.

#Reorder object.
Swordfish_Imp_Form$Country <- reorder(Swordfish_Imp_Form$Country,
                                      Swordfish_Imp_Form$pounds)

#Create graph.
Swordfish_ImpVol_Cnty <- ggplot(Swordfish_Imp_Form) +
  geom_bar(aes(y = pounds, x = YR, fill = Country), stat = "identity",
           color = "black", position = "stack") +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "black")) +
  facet_wrap(~FormDesc) +
  coord_cartesian(ylim = c(0, 40000)) +
  scale_y_continuous(expand = c(0, 0), labels = comma) +
  scale_x_continuous(breaks = seq(2008, 2019, 1)) +
  scale_fill_manual(values = wes_palette("Zissou1", 10, type = "continuous")) +
  labs(x = "Year", y = "Total Pounds Imported (lb.)") +
  theme(legend.position = "right", 
        legend.title = element_blank(), 
        text = element_text(size = 11, family = "serif"),
        axis.text.x = element_text(angle = 45, hjust = 1))

#Save figure.
save_plot("Figures/Swordfish_ImpVol_Cnty.png", Swordfish_ImpVol_Cnty, base_aspect_ratio = 2)



#--------------------
#Table 21. Total swordfish exports from Hawai'i. 
#--------------------
#Create object for table and figure.
Swordfish_Exp_Form <- dat %>%
  filter(grepl("swordfish", Species, ignore.case = TRUE)) %>%
  filter(YR > 2007 & YR < 2020) %>%
  filter(Trade == "E") %>%
  group_by(YR, Country, Product, FormDesc) %>%
  mutate(Country = recode(Country, "CANADA" = "Canada")) %>%
  summarize(pounds = sum(cPounds), value = sum(RDollars)) %>%
  group_by(YR) %>%
  mutate(Tot = sum(pounds)) %>%
  group_by(Country) %>%
  mutate(per = pounds / Tot * 100) %>%
  mutate(UnitValue = round(value / pounds, 2), .before = Tot)

#Coerce object to character vector.
Swordfish_Exp_Form$Country <- as.character(Swordfish_Exp_Form$Country)

#Save table.
write.csv(Swordfish_Exp_Form, "Tables/SwordfishExports.csv")



#--------------------
#Figure 32. Total swordfish export volume from Hawai'i. 
#--------------------
#Uses object created for Table 21.

#Reorder object for figure.
Swordfish_Exp_Form$Country <- reorder(Swordfish_Exp_Form$Country,
                                      Swordfish_Exp_Form$pounds)

#Create graph.
Swordfish_ExpVol_Cnty <- ggplot(Swordfish_Exp_Form) +
  geom_bar(aes(y = pounds, x = YR, fill = Country), stat = "identity",
           color = "black", position = "stack") +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "black")) +
  facet_wrap(~FormDesc) +
  coord_cartesian(ylim = c(0, 20000)) +
  scale_y_continuous(expand = c(0, 0), labels = comma) +
  scale_x_continuous(breaks = seq(2008, 2019, 1), 
                     limits = c(2007, 2019)) +
  scale_fill_manual(values = wes_palette("Zissou1", 1, type = "continuous")) +
  labs(x = "Year", y = "Total Pounds Exported (lb.)") +
  theme(legend.position = "right", legend.title = element_blank(),
        text = element_text(size = 11, family = "serif"),
        axis.text.x = element_text(angle = 45, hjust = 1))

#Save figure.
save_plot("Figures/Swordfish_ExpVol_Cnty.png", Swordfish_ExpVol_Cnty, 
          base_aspect_ratio = 2)