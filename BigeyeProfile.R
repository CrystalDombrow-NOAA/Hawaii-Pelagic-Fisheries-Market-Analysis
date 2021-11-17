#Profile for bigeye tuna. 

rm(list = ls())
source("Rscripts/SourceFiles/Installations.R")
source("Rscripts/SourceFiles/ColorFunctionForFigures.R")


#--------------------
#Table 11. Average annual supply and value of bigeye tuna, 2008-2019.
#--------------------
source("Rscripts/SourceFiles/SummaryTableAnnualAverages.R")
US_Summary_fun("BIGEYE TUNA", "BIGEYE")


#--------------------
#Figure 8. Total domestic consumption of bigeye tuna in Hawai'i and the continental U.S.
#Figure 9. Annual share of domestic bigeye tuna consumption in Hawai'i. 
#Figure 10. Annual share of domestic bigeye tuna consumption on the continental U.S. 
#--------------------
#Not reproduced in manuscript, but calculations used in text & final function
#used for calculations in ConsumptionTrends.R.
source("Rscripts/SourceFiles/SummaryTablebyYear.r")
Annual_US_Summary_fun("BIGEYE TUNA", "BIGEYE")

source("Rscripts/SourceFiles/ConsumptionTrends.r")


#--------------------
#Figure 11: Average volume and value of bigeye tuna landed in Hawai'i. 
#--------------------
library(scales)

#Create object for figure.
BEtunaProdVolVal <- Landat %>%
  filter(Species == "Bigeye tuna") %>%
  filter(YR > 2007) %>%
  group_by(YR, Species) %>%
  summarize(PoundsSold = round(mean(Sold)), 
            Dollars = round(mean(Value)), 
            Price = round(Dollars / PoundsSold, 2))

#Create bar chart.
BEtuna_volval_HIprod <- ggplot(BEtunaProdVolVal) +
  geom_bar(aes(y = PoundsSold / 1000000, x = YR, fill = "PoundsSold"), 
           stat = "identity", color = "black", position = "stack") +
  geom_line(aes(y = Dollars / 4000000, x = YR, color = "Dollars"), 
            stat = "identity", size = 1) +
  labs(x = "Year", y = "Average Pounds Sold (million lb.)") +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "black")) +
  scale_x_continuous(breaks = c(2008:2019)) +
  scale_y_continuous(expand = c(0, 0), limit = c(0, 20),
                     sec.axis = sec_axis(~ . * 4, 
                                         name = "Average Revenue ($ million)", 
                                         label = dollar_format())) +
  coord_cartesian(ylim = c(0, 20)) +
  scale_fill_manual(labels = c("Volume"), values = wescol5[c(3)]) +
  scale_color_manual(labels = c("Value"), values = wescol5[c(4)]) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.justification = "center", 
        text = element_text(size = 12, family = "serif"))

#Save figure.
save_plot("Figures/BEtuna_volval_HIprod.png", 
          BEtuna_volval_HIprod, base_aspect_ratio = 1.5)



#--------------------
#Figure 12: Monthly average ex-vessel prices (per pound) for Hawai'i bigeye tuna 
#landings.  
#--------------------
library(scales)
require(lubridate)

#Function for figure.
scaleFUN <- function(x) sprintf("$%.2f", x)

start <- ymd("2008/01/01")

#Create object for figure.
MonthlyPriceBET <- Monthdat %>%
  mutate(date = start + months(0:143), year = year(date), 
         month = month(date, label = T, abbr = T)) %>%
  select(date, year, month, Bigeye.tuna) %>%
  group_by(date, year, month)

#Create figure.
ExVessPrice_Col <- ggplot(MonthlyPriceBET, 
                          aes(x = date, y = Bigeye.tuna, group = 1)) +
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
save_plot("Figures/ExVessPrice_Col.png", ExVessPrice_Col, base_aspect_ratio = 2)


#--------------------
#Figure 13: Total volume and value of bigeye tuna exports from Hawai'i. 
#--------------------
library(scales)
library(wesanderson)

#Create objects.
BEtuna_Trade_Total <- dat %>%
  filter(grepl("bigeye tuna", Species, ignore.case = TRUE)) %>%
  filter(YR > 2007) %>%
  filter(Trade != "R") %>%
  group_by(YR, Trade) %>%
  summarize(pounds = sum(cPounds), value = sum(RDollars))

#Filter for exports.
BEtuna_Trade_Total_E <- BEtuna_Trade_Total %>%
  filter(Trade == "E") 


#Create bar chart.
Export_Plot <- ggplot(BEtuna_Trade_Total_E) +
  geom_bar(aes(y = pounds, x = YR, fill = "pounds"), 
           stat = "identity", 
           colour = "black") +
  geom_line(aes(y = value / 3, x = YR, color = "value"), stat = "identity",
            size = 1) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "black")) +
  labs(x = "Year", y = "Total Pounds Exported (lb.)") +
  scale_x_continuous(breaks = seq(2008, 2019, 1)) +
  scale_y_continuous(expand = c(0, 0), labels = comma, 
                     sec.axis = sec_axis(~ . * 3, name = "Total Revenue ($)", 
                                         labels = dollar)) +
  coord_cartesian(ylim = c(0, 1200000)) +
  scale_fill_manual(labels = c("Volume"), values = wescol5[c(3)]) +
  scale_color_manual(labels = c("Value"), values = wescol5[c(4)]) +
  ggtitle("Exports") +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.justification = "center", 
        text = element_text(size = 12, family = "serif"), 
        axis.text.x = element_text(angle = 45, hjust = 1))

#Save figure.
save_plot("Figures/BigeyeExports.png", Export_Plot, base_aspect_ratio = 1.15)



#--------------------
#Figure 15: Total volume and value of bigeye tuna imports to Hawai'i. 
#--------------------
library(scales)
library(wesanderson)

#Create objects for graph.
BEtuna_Trade_Total <- dat %>%
  filter(grepl("bigeye tuna", Species, ignore.case = TRUE)) %>%
  filter(YR > 2007) %>%
  filter(Trade != "R") %>%
  group_by(YR, Trade) %>%
  summarize(pounds = sum(cPounds), value = sum(RDollars))

#Filter for imports.
BEtuna_Trade_Total_I <- BEtuna_Trade_Total %>%
  filter(Trade == "I") 

#Create graph.
Import_Plot <- ggplot(BEtuna_Trade_Total_I) +
  geom_bar(aes(y = pounds, x = YR, fill = "pounds"), 
           stat = "identity", 
           colour = "black") +
  geom_line(aes(y = value / 1.5, x = YR, color = "value"), 
            stat = "identity", size = 1) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "black")) +
  labs(x = "Year", y = "Total Pounds Imported (lb.)") +
  scale_x_continuous(breaks = seq(2008, 2019, 1)) +
  scale_y_continuous(expand = c(0, 0), labels = comma, 
                     sec.axis = sec_axis(~ . * 1.5, name = "Total Value ($)", 
                                         labels = dollar)) +
  coord_cartesian(ylim = c(0, 1600000)) +
  scale_fill_manual(labels = c("Volume"), values = wescol5[c(3)]) +
  scale_color_manual(labels = c("Value"), values = wescol5[c(4)]) +
  ggtitle("Imports") +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        legend.justification = "center", 
        text = element_text(size = 12, family = "serif"), 
        axis.text.x = element_text(angle = 45, hjust = 1))

#Save figure.
save_plot("Figures/BigeyeImports.png", Import_Plot, base_aspect_ratio = 1.15)



#--------------------
#Table 12. Total fresh bigeye tuna exports from Hawai'i. 
#Table 13. Total frozen bigeye tuna exports from Hawai'i. 
#--------------------
library(scales)
library(wesanderson)

#Create objects for graph and table.
BEtuna_Exp_Form <- dat %>%
  filter(Species == "BIGEYE TUNA") %>%
  filter(YR > 2007) %>%
  filter(Trade != "R" & Trade != "I") %>%
  mutate(Country = recode(Country,
                          "CANADA" = "Canada", "JAPAN" = "Japan",
                          "CHINA - HONG KONG" = "China - Hong Kong",
                          "SOUTH KOREA" = "South Korea")) %>%
  group_by(YR, Form, Country) %>%
  summarize(pounds = sum(cPounds), value = sum(RDollars)) %>%
  mutate(UnitValue = round(value / pounds, 2)) %>%
  mutate(totannvol = round(sum(pounds))) %>%
  ungroup(.) %>%
  group_by(Country, YR, Form) %>%
  mutate(countryannvolshare = round(mean(pounds / totannvol) * 100, 1)) %>%
  ungroup(.) %>%
  group_by(Form) %>%
  mutate(totvolbyform = sum(unique(totannvol))) %>%
  ungroup(.) %>%
  group_by(Country, Form) %>%
  mutate(countryformtotvol = sum(pounds)) %>%
  mutate(avgsharebycountryform = (countryformtotvol / totvolbyform) * 100) %>%
  mutate(avgsharebycountryform = round(avgsharebycountryform, 1)) %>%
  ungroup(.) %>%
  group_by(YR, Form, Country)

#Reorder.
BEtuna_Exp_Form$Country <- reorder(BEtuna_Exp_Form$Country,
                                   BEtuna_Exp_Form$pounds)

#Save table.
write.csv(BEtuna_Exp_Form, "Tables/BEtuna_Exp_Form.csv")



#--------------------
#Figure 14. Total bigeye tuna export volume from Hawai'i. 
#--------------------
#Create graph.
BEtuna_ExpVol_Cnty <- ggplot(BEtuna_Exp_Form) +
  geom_bar(aes(y = pounds, x = YR, fill = Country),
           stat = "identity",
           position = "stack", colour = "black") +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = .1, color = "black")) +
  facet_wrap(~Form) +
  coord_cartesian(ylim = c(0, 750000)) +
  scale_y_continuous(expand = c(0, 0), labels = comma) +
  scale_x_continuous(breaks = seq(2008, 2019, 1)) +
  labs(x = "Year", y = "Total Pounds Exported (lb.)") +
  scale_fill_manual(values = wes_palette("Zissou1", 4, type = "continuous")) +
  theme(legend.position = "right", 
        legend.title = element_blank(),
        text = element_text(size = 12, family = "serif"),
        axis.text.x = element_text(angle = 45, hjust = 1))

#Save figure.
save_plot("Figures/BEtuna_ExpVol_Cnty.png", BEtuna_ExpVol_Cnty,
          base_aspect_ratio = 2)



#--------------------
#Table 14. Total bigeye tuna imports to Hawai'i. 
#--------------------
library(scales)
library(wesanderson)

#Create object for table and graph.
BEtuna_Imp_Form <- dat %>%
  filter(Species == "BIGEYE TUNA") %>%
  filter(YR > 2007) %>%
  filter(Trade == "I") %>%
  group_by(YR, Country) %>%
  summarize(pounds = sum(cPounds), value = sum(RDollars)) %>%
  mutate(annualtot = sum(pounds)) %>%
  ungroup(.) %>%
  group_by(Country) %>%
  mutate(Country = recode(Country, "MARSHALL IS." = "Marshall Islands",
                          "FED STATES OF MICRON" = "Fed. States of Micronesia",
                          "PHILIPPINES" = "Philippines",
                          "AUSTRALIA" = "Australia", "VIETNAM" = "Vietnam")) %>%
  mutate(annualsharebycountry = pounds / annualtot * 100) %>%
  mutate(countrytotvol = sum(pounds)) %>%
  ungroup(.) %>%
  mutate(totvol = sum(unique(annualtot))) %>%
  mutate(avgsharebycountry = (countrytotvol / totvol) * 100) %>%
  mutate(avgsharebycountry = round(avgsharebycountry, 2)) %>%
  group_by(YR, Country)


#Coerce column to a character vector.
BEtuna_Imp_Form$Country <- as.character(BEtuna_Imp_Form$Country)

#Create "Other" for graph, comments out for tables.
BEtuna_Imp_Form$Country[BEtuna_Imp_Form$annualsharebycountry < 10] <- "Other"

#Create another object for tables.
BEtuna_Imp_Top <- BEtuna_Imp_Form %>%
  group_by(YR, Country) %>%
  summarize(pounds = sum(pounds), value = sum(value), 
            annualsharebycountry = sum(annualsharebycountry)) %>%
  mutate(UnitValue = round(value / pounds, 2), 
         .before = annualsharebycountry) %>%
  ungroup(.) %>%
  drop_na(Country)

#Save table.
write.csv(BEtuna_Imp_Top, "Tables/BEtunaImports.csv")



#--------------------
#Figure 16. Total bigeye tuna import volume to Hawai'i.   
#--------------------
#Create graph.
BEtuna_ImpVol_Cnty <- ggplot(BEtuna_Imp_Top) +
  geom_bar(aes(y = pounds, x = YR, fill = Country),
           stat = "identity",
           color = "black", position = "stack") +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "black")) +
  coord_cartesian(ylim = c(0, 1600000)) +
  scale_y_continuous(expand = c(0, 0), labels = comma) +
  scale_x_continuous(breaks = seq(2008, 2019, 1)) +
  labs(x = "Year", y = "Total Pounds Imported (lb.)") +
  scale_fill_manual(values = wes_palette("Zissou1", 6, type = "continuous")) +
  theme(panel.spacing = unit(-.25, "lines")) +
  theme(legend.position = "right",
        legend.title = element_blank(),
        text = element_text(size = 12, family = "serif"),
        axis.text.x = element_text(angle = 45, hjust = 1))

#Save figure.
save_plot("Figures/BEtuna_ImpVol_Cnty.png", BEtuna_ImpVol_Cnty,
          base_aspect_ratio = 2)