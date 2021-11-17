#Profile for yellowfin tuna. 

rm(list = ls())
source("Rscripts/SourceFiles/Installations.R")
source("Rscripts/SourceFiles/ColorFunctionForFigures.R")


#--------------------
#Table 15. Average annual supply and value of yellowfin tuna, 2008-2019.
#--------------------
source("Rscripts/SourceFiles/SummaryTableAnnualAverages.R")
US_Summary_fun("YELLOWFIN TUNA", "YELLOWFIN")


#--------------------
#Figure 17. Total domestic consumption of yellowfin tuna in Hawai'i and the continental U.S.
#Figure 18. Annual share of local yellowfin tuna consumption in Hawai'i.
#Figure 19. Annual share of domestic yellowfin tuna consumption on the continental U.S.
#--------------------
#Not reproduced in manuscript, but calculations used in text & final function
#used for calculations in ConsumptionTrends.R.
source("Rscripts/SourceFiles/SummaryTablebyYear.r")
Annual_US_Summary_fun("YELLOWFIN TUNA", "YELLOWFIN")

source("Rscripts/SourceFiles/ConsumptionTrends.r")



#--------------------
#Figure 20. Average volume and value of yellowfin tuna landed in Hawai'i. 
#--------------------
library(scales)

#Create object for graph.
YFtunaProdVolVal <- Landat %>%
  filter(Species == "Yellowfin tuna") %>%
  filter(YR > 2007) %>%
  group_by(YR, Species) %>%
  summarize(PoundsSold = round(mean(Sold)), 
            Dollars = round(mean(Value)), 
            Price = round(Dollars / PoundsSold, 2))

#Create graph.
YFtuna_volval_HIprod <- ggplot(YFtunaProdVolVal) +
  geom_bar(aes(y = PoundsSold, x = YR, fill = "PoundsSold"), 
           stat = "identity", color = "black", position = "stack") +
  geom_line(aes(y = Dollars / 4, x = YR, color = "Dollars"), 
            stat = "identity", size = 1) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "black")) +
  labs(x = "Year", y = "Average Pounds Landed (lb.)") +
  scale_x_continuous(breaks = seq(2008, 2019, 1)) +
  scale_y_continuous(expand = c(0, 0), labels = comma, 
                     sec.axis = sec_axis(~ . * 4, name = "Average Revenue ($)", 
                                         labels = dollar)) +
  coord_cartesian(ylim = c(0, 8000000)) +
  scale_fill_manual(labels = c("Volume"), values = wescol5[c(3)]) +
  scale_color_manual(labels = c("Value"), values = wescol5[c(4)]) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.justification = "center", 
        text = element_text(size = 12, family = "serif"))

#Save figure.
save_plot("Figures/YFtuna_volval_HIprod.png", 
          YFtuna_volval_HIprod, base_aspect_ratio = 1.75)


#--------------------
#Figure 21. Monthly average ex-vessel prices (per pound) for Hawai'i yellowfin 
#tuna landings.  
#--------------------
scaleFUN <- function(x) sprintf("$%.2f", x)
library(scales)
require(lubridate)

#Create object for graph.
start <- ymd("2008/01/01")

#Create another object for graph.
MonthlyPriceYF <- Monthdat %>%
  mutate(date = start + months(0:143), year = year(date), 
         month = month(date, label = T, abbr = T)) %>%
  select(date, year, month, Yellowfin.tuna) %>%
  group_by(date, year, month)

#Create graph.
ExVessPrice_Col <- ggplot(MonthlyPriceYF, aes(x = date, y = Yellowfin.tuna, 
                                              group = 1)) +
  geom_line(stat = "identity") +
  geom_point() +
  scale_y_continuous(labels = scaleFUN) +
  labs(x = "Year", y = "Price") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y", expand = c(0, 0)) +
  theme_classic() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        text = element_text(size = 12, 
                            family = "serif"), 
        axis.text.x = element_text(angle = 45, hjust = 1))

#Save figure.
save_plot("Figures/ExVessPrice_Col_YF.png", ExVessPrice_Col, 
          base_aspect_ratio = 2)


#--------------------
#Figure 22. Total volume and value of yellowfin tuna exports from Hawai'i. 
#--------------------
library(cowplot)
WesColChange()

#Create object for graph.
YFtuna_Trade_Total <- dat %>%
  filter(grepl("YELLOWFIN TUNA", Species, ignore.case = TRUE)) %>%
  filter(YR > 2007) %>%
  filter(Trade != "R") %>%
  group_by(YR, Trade) %>%
  summarize(pounds = sum(cPounds), value = sum(RDollars)) 

#Filter object for graph.
YFtuna_Trade_Total_E <- YFtuna_Trade_Total %>% 
  filter(Trade == "E")

#Create graph.
Export_Plot <- ggplot(YFtuna_Trade_Total_E) +
  geom_bar(aes(y = pounds, x = YR, fill = "pounds"), 
           stat = "identity", colour = "black") +
  geom_line(aes(y = value / 10, x = YR, color = "value"), 
            stat = "identity", size = 1) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "black")) +
  labs(x = "Year", y = "Total Pounds Exported (lb.)") +
  scale_x_continuous(breaks = seq(2008, 2019, 1)) +
  scale_y_continuous(expand = c(0, 0), labels = comma, 
                     sec.axis = sec_axis(~ . * 10, name = "Total Revenue ($)", 
                                         labels = dollar)) +
  coord_cartesian(ylim = c(0, 200000)) +
  scale_fill_manual(labels = c("Volume"), values = wescol5[c(3)]) +
  scale_color_manual(labels = c("Value"), values = wescol5[c(4)]) +
  ggtitle("Exports") +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.justification = "center", 
        text = element_text(size = 12, family = "serif"), 
        axis.text.x = element_text(angle = 45, hjust = 1))

#Save figure.
save_plot("Figures/YellowfinExports.png", Export_Plot, base_aspect_ratio = 1.15)



#--------------------
#Figure 24. Total volume and value of yellowfin tuna imports to Hawai'i. 
#--------------------
library(cowplot)
WesColChange()

#Create object for graph.
YFtuna_Trade_Total <- dat %>%
  filter(grepl("YELLOWFIN TUNA", Species, ignore.case = TRUE)) %>%
  filter(YR > 2007) %>%
  filter(Trade != "R") %>%
  group_by(YR, Trade) %>%
  summarize(pounds = sum(cPounds), value = sum(RDollars)) 

#Filter object.
YFtuna_Trade_Total_I <- YFtuna_Trade_Total %>% 
  filter(Trade == "I")

#Create graph.
Import_Plot <- ggplot(YFtuna_Trade_Total_I) +
  geom_bar(aes(y = pounds, x = YR, fill = "pounds"), 
           stat = "identity", colour = "black") +
  geom_line(aes(y = value / 5, x = YR, color = "value"), 
            stat = "identity", size = 1) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "black")) +
  labs(x = "Year", y = "Total Pounds Imported (lb.)") +
  scale_x_continuous(breaks = seq(2008, 2019, 1)) +
  scale_y_continuous(expand = c(0, 0), labels = comma, 
                     sec.axis = sec_axis(~ . * 5, name = "Total Value ($)", 
                                         labels = dollar)) +
  coord_cartesian(ylim = c(0, 1200000)) +
  scale_fill_manual(labels = c("Volume"), values = wescol5[c(3)]) +
  scale_color_manual(labels = c("Value"), values = wescol5[c(4)]) +
  ggtitle("Imports") +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.justification = "center", 
        text = element_text(size = 12, family = "serif"), 
        axis.text.x = element_text(angle = 45, hjust = 1))

#Save figure.
save_plot("Figures/YellowfinImports.png", Import_Plot, base_aspect_ratio = 1.15)


#--------------------
#Figure 23. Total yellowfin tuna export volume from Hawai'i. 
#--------------------
library(scales)
library(wesanderson)

#Create object for graph.
YFtuna_Exp_Form <- dat %>%
  filter(Species == "YELLOWFIN TUNA") %>%
  filter(YR > 2007) %>%
  filter(Trade != "R" & Trade != "I") %>%
  group_by(YR, FormDesc, Country) %>%
  mutate(Country = recode(Country,
                          "CANADA" = "Canada",
                          "JAPAN" = "Japan",
                          "SOUTH KOREA" = "South Korea",
                          "MALAYSIA" = "Malaysia")) %>%
  summarize(pounds = sum(cPounds), value = sum(RDollars)) %>%
  mutate(UnitValue = round(value / pounds, 2)) %>%
  mutate(totannvol = round(sum(pounds))) %>%
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


#Reorder object for graph.
YFtuna_Exp_Form$Country <- reorder(YFtuna_Exp_Form$Country,
                                   YFtuna_Exp_Form$pounds)

#Create graph.
YFtuna_ExpVol_Cnty <- ggplot(YFtuna_Exp_Form) +
  geom_bar(aes(y = pounds, x = YR, fill = Country), stat = "identity",
           position = "stack", colour = "black") +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "black")) +
  facet_wrap(~FormDesc) +
  coord_cartesian(ylim = c(0, 200000)) +
  scale_y_continuous(expand = c(0, 0), labels = comma) +
  scale_x_continuous(breaks = seq(2008, 2019, 1)) +
  labs(x = "Year", y = "Total Pounds Exported (lb.)") +
  scale_fill_manual(values = wes_palette("Zissou1", 6, type = "continuous")) +
  theme(legend.position = "right",
        legend.title = element_blank(),
        text = element_text(size = 12, family = "serif"),
        axis.text.x = element_text(angle = 45, hjust = 1))

#Save figure.
save_plot("Figures/YFtuna_ExpVol_Cnty.png", YFtuna_ExpVol_Cnty, 
          base_aspect_ratio = 2)


#--------------------
#Figure 25. Total yellowfin tuna import volume to Hawai'i.  
#--------------------
library(scales)
library(wesanderson)

#Create object for graph.
YFtuna_Imp_Form <- dat %>%
  filter(Species == "YELLOWFIN TUNA") %>%
  filter(YR > 2007) %>%
  filter(Trade == "I") %>%
  group_by(YR, FormDesc, Country) %>%
  mutate(Country = recode(Country, "FIJI" = "Fiji", "TONGA" = "Tonga",
                          "PHILIPPINES" = "Philippines",
                          "INDONESIA" = "Indonesia",
                          "AUSTRALIA" = "Australia",
                          "VANUATU" = "Vanuatu",
                          "MARSHALL IS." = "Marshall Islands",
                          "CHINA - TAIPEI" = "China - Taipei",
                          "THAILAND" = "Thailand")) %>%
  summarize(pounds = sum(cPounds), value = sum(RDollars)) %>%
  group_by(YR, FormDesc) %>%
  mutate(totannvol = sum(pounds)) %>%
  mutate(UnitValue = round(value / pounds, 2)) %>%
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
YFtuna_Imp_Form$Country <- as.character(YFtuna_Imp_Form$Country)

#Create "Other" category for graph.
YFtuna_Imp_Form$Country[YFtuna_Imp_Form$countryannvolshare < 20] <- "Other"

#Edit object for graph.
YFtuna_Imp_Top_Form <- YFtuna_Imp_Form %>%
  group_by(YR, FormDesc, Country) %>%
  summarize(pounds = sum(pounds), value = sum(value), countryannvolshare = sum(countryannvolshare))

#Create graph.
YFtuna_ImpVol_Form <- ggplot(YFtuna_Imp_Top_Form) +
  geom_bar(aes(y = pounds, x = YR, fill = Country),
           stat = "identity",
           position = "stack", colour = "black") +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "black")) +
  facet_wrap(~FormDesc) +
  coord_cartesian(ylim = c(0, 750000)) +
  scale_y_continuous(expand = c(0, 0), labels = comma) +
  scale_x_continuous(breaks = seq(2008, 2019, 1)) +
  scale_fill_manual(values = wes_palette("Zissou1", 10, type = "continuous")) +
  labs(x = "Year", y = "Total Pounds Imported (lb.)") +
  theme(legend.position = "right", 
        legend.title = element_blank(),
        text = element_text(size = 12, family = "serif"),
        axis.text.x = element_text(angle = 45, hjust = 1))

#Save figure.
save_plot("Figures/YFtuna_ImpVol_Form.png", YFtuna_ImpVol_Form, 
          base_aspect_ratio = 2)



#--------------------
#Table 18. Total fresh imports of yellowfin tuna to Hawai'i.
#--------------------
#Create object for tables.
YFtuna_Imp_Form <- dat %>%
  filter(Species == "YELLOWFIN TUNA") %>%
  filter(YR > 2007) %>%
  filter(Trade == "I") %>%
  group_by(YR, Form, Product, Country) %>%
  summarize(pounds = sum(cPounds), value = sum(RDollars)) %>%
  group_by(YR, Form) %>%
  mutate(Tot = sum(pounds)) %>%
  group_by(Country) %>%
  mutate(per = pounds / Tot * 100)

#Coerce to character vector.
YFtuna_Imp_Form$Country <- as.character(YFtuna_Imp_Form$Country)

#Edit object for tables.
YFtuna_Imp_Top_Form <- YFtuna_Imp_Form %>%
  group_by(YR, Form, Product, Country) %>%
  summarize(pounds = sum(pounds), value = sum(value), per = sum(per)) %>%
  mutate(UnitValue = round(value / pounds, 2), .before = per)

#Filter object.
YF_Fresh_Imp_Table <- YFtuna_Imp_Top_Form %>%
  filter(Form == "Fresh")

#Save table.
write.csv(YF_Fresh_Imp_Table, "tables/YF_Imp_Fresh.csv")



#--------------------
#Table 18. Total frozen yellowfin tuna imports to Hawai'i.
#--------------------
#Uses objects created for Table 17.

#Filter object for table.
YF_Frozen_Imp_Table <- YFtuna_Imp_Top_Form %>%
  filter(Form == "Frozen")

#Save table.
write.csv(YF_Frozen_Imp_Table, "tables/YF_Imp_Frozen.csv")



#--------------------
#Table 16. Total fresh yellowfin tuna exports from Hawai'i. 
#--------------------
#Create object for table.
YFtuna_Exp_Form <- dat %>%
  filter(Species == "YELLOWFIN TUNA") %>%
  filter(YR > 2007) %>%
  filter(Trade == "E") %>%
  group_by(YR, Form, Product, Country) %>%
  summarize(pounds = sum(cPounds), value = sum(RDollars)) %>%
  group_by(YR, Form) %>%
  mutate(Tot = sum(pounds)) %>%
  group_by(Country) %>%
  mutate(per = pounds / Tot * 100)

#Coerce to character vector.
YFtuna_Exp_Form$Country <- as.character(YFtuna_Exp_Form$Country)

#Create "Other" category for graph. Comment out for table.
#YFtuna_Exp_Form$Country[YFtuna_Exp_Form$per < 20] <- "OTHER"

#Create edited object for table.
YFtuna_Exp_Top_Form <- YFtuna_Exp_Form %>%
  group_by(YR, Form, Product, Country) %>%
  summarize(pounds = sum(pounds), value = sum(value), per = sum(per)) %>%
  mutate(UnitValue = round(value / pounds, 2), .before = per)

#Filter object.
YF_Fresh_Exp_Table <- YFtuna_Exp_Top_Form %>%
  filter(Form == "Fresh")

#Save table.
write.csv(YF_Fresh_Exp_Table, "tables/YF_Exp_Fresh.csv")


#--------------------
#Table 17. Total frozen yellowfin tuna exports from Hawai'i. 
#--------------------
#This table uses the code chunks above from Table 16.

#Filter object.
YF_Frozen_Exp_Table <- YFtuna_Exp_Top_Form %>%
  filter(Form == "Frozen")

#Save table.
write.csv(YF_Frozen_Exp_Table, "tables/YF_Exp_Frozen.csv")