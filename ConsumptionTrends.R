#Create consumption trends figures for pelagic species profiles.
#NOTE: The output figures overwrite one another, so rerun as needed for the 
#desired calculation.

#--------------
#Create objects for figures.
#--------------
US_Consumption <- US_Annual %>%
  separate(Species, c("Region", "Trade")) %>%
  group_by(YR, Region, Trade) %>%
  summarize(Volume = sum(Volume), Value = sum(Value)) %>%
  gather(Var, num, -YR, -Region, -Trade) %>%
  spread(Region, num)

if("Exports" %in% unique(US_Consumption$Trade) == T) {
  ML_Consumption_Volume <- US_Consumption %>%
    select(-Hawaii) %>%
    spread(Trade, ML) %>%
    filter(Var == "Volume") %>%
    mutate(ML_Production = Production, ML_Imports = Imports, 
           ML_Exports = Exports) %>%
    select(-Imports, -Production, -Exports)
} else {
  ML_Consumption_Volume <- US_Consumption %>%
    select(-Hawaii) %>%
    spread(Trade, ML) %>%
    filter(Var == "Volume") %>%
    mutate(ML_Production = Production, ML_Imports = Imports) %>%
    select(-Imports, -Production)
}

if (ncol(ML_Consumption_Volume) == 4) {
  ML_Consumption_Volume <- ML_Consumption_Volume %>%
    mutate(ML_Exports = 0)
} else {
  ML_Consumption_Volume <- ML_Consumption_Volume
}


if("Exports" %in% unique(US_Consumption$Trade) == T) {
  Hawaii_Consumption_Trends <- US_Consumption %>%
    select(-ML) %>%
    spread(Trade, Hawaii) %>%
    filter(Var == "Volume") %>%
    mutate(nNAImports = ifelse(is.na(Imports), 0, Imports), 
           nNAExports = ifelse(is.na(Exports), 0, Exports), 
           Cons_HI = Production + nNAImports - nNAExports, 
           Share_Imp_HI = round(nNAImports / Cons_HI * 100, 2), 
           Share_Prod_HI = round(100 - Share_Imp_HI, 2)) %>%
    left_join(ML_Consumption_Volume[, c(1, 3:5)], by = "YR") %>%
    mutate(Tot_Prod = Production + ML_Production,
           Tot_Exp = ML_Exports + nNAExports,
           Tot_Imp = ML_Imports + nNAImports,
           Cons_ML = ML_Production + ML_Imports - ML_Exports,
           Cons_US = Cons_HI + Cons_ML,
           Share_Imp_US = round(Tot_Imp / Cons_US * 100, 2),
           Share_Prod_US = round(100 - Share_Imp_US, 2),
           Share_Prod_ML = round(ML_Production / Cons_ML * 100, 2),
           Share_Imp_ML = round(100 - Share_Prod_ML, 2),
           Share_ProdConsumed_HI = round(Production / Cons_US * 100, 2),
           Share_USCons_HI = Cons_HI / Cons_US) %>%
    select(YR, ML_Production, Production, ML_Imports, ML_Exports, Cons_HI, 
           Cons_ML, Cons_US, Share_USCons_HI, Tot_Imp, Share_Imp_HI, 
           Share_Imp_US, Share_Imp_ML, Share_Prod_ML, Share_Prod_HI, 
           Share_Prod_US, Share_ProdConsumed_HI)
} else {
  Hawaii_Consumption_Trends <- US_Consumption %>%
    select(-ML) %>%
    spread(Trade, Hawaii) %>%
    filter(Var == "Volume") %>%
    mutate(nNAImports = ifelse(is.na(Imports), 0, Imports), 
           Cons_HI = Production + nNAImports,
           Share_Imp_HI = round(nNAImports / Cons_HI * 100, 2), 
           Share_Prod_HI = round(100 - Share_Imp_HI, 2)) %>%
    left_join(ML_Consumption_Volume[, c(1, 3:5)], by = "YR") %>%
    mutate(Tot_Prod = Production + ML_Production,
           Tot_Imp = ML_Imports + nNAImports,
           Cons_ML = ML_Production + ML_Imports,
           Cons_US = Cons_HI + Cons_ML,
           Share_Imp_US = round(Tot_Imp / Cons_US * 100, 2),
           Share_Prod_US = round(100 - Share_Imp_US, 2),
           Share_Prod_ML = round(ML_Production / Cons_ML * 100, 2),
           Share_Imp_ML = round(100 - Share_Prod_ML, 2),
           Share_ProdConsumed_HI = round(Production / Cons_US * 100, 2),
           Share_USCons_HI = Cons_HI / Cons_US) %>%
    select(YR, ML_Production, Production, ML_Imports, Cons_HI, Cons_ML, Cons_US, 
           Share_USCons_HI, Tot_Imp, Share_Imp_HI, Share_Imp_US, Share_Imp_ML, 
           Share_Prod_ML, Share_Prod_HI, Share_Prod_US, Share_ProdConsumed_HI)
}


#Import share Hawai'i.
Scale_HI_Imp <- Hawaii_Consumption_Trends %>%
  select(YR, Share_Imp_HI, Share_Prod_HI) %>%
  gather(Production_Share, share, -YR) %>%
  group_by(YR) %>%
  arrange(YR)

#Import share continental US.
Scale_ML_Imp <- Hawaii_Consumption_Trends %>%
  select(YR, Share_Imp_ML, Share_Prod_ML) %>%
  gather(Production_Share, share, -YR) %>%
  group_by(YR) %>%
  arrange(YR)

#Hawai'i consumption.
HI_Dom_Cons <- Hawaii_Consumption_Trends %>%
  select(YR, Share_ProdConsumed_HI) %>%
  mutate(Share_ProdConsumed_ML = 100 - Share_ProdConsumed_HI) %>%
  gather(Production_Share, share, -YR) %>%
  group_by(YR) %>%
  arrange(YR)


rounder <- function(x, y) {
  if (y >= 0) {
    x + (y - x %% y)
  }
  else {
    x - (x %% abs(y))
  }
}

#Consumption trends, continental US and Hawai'i.
Trend_Cons <- Hawaii_Consumption_Trends %>%
  select(YR, Cons_HI, Cons_ML) %>%
  gather(Region, Cons, -YR)



#---------------
#Figures 8, 17, 26. Total domestic consumption of [spp.] in Hawai'i and the 
#continental U.S. 
#---------------
#Create figure.
Cons_Figure <- ggplot(Trend_Cons, aes(x = YR, y = Cons / 1000000, 
                                      fill = Region)) +
  geom_bar(stat = "identity", colour = "black") +
  labs(x = "Year", y = "Total Pounds Consumed (million lb.)") +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "black"),
        panel.grid.minor.y = element_line(size = .1, color = "black")) +
  coord_cartesian(ylim = c(0, 50)) +
  scale_x_continuous(breaks = seq(2008, 2019, 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(labels = c("Hawai`i", "Continental U.S."), 
                    values = colBrew2) +
  guides(fill = guide_legend(title = NULL)) +
  theme(text = element_text(size = 12, family = "serif"))

#Save figure.
save_plot("Figures/Cons_Share_Stacked.png", Cons_Figure, 
           base_aspect_ratio = 1.75)


#---------------
#Figures 10, 19, 28. Annual share of domestic [spp.] consumption in the 
#continental U.S. 
#---------------
#Create figure.
Mainland_Imp_Share_Figure <- ggplot(Scale_ML_Imp, aes(x = YR, y = share,
                                                      fill = Production_Share)) +
  geom_bar(stat = "identity", colour = "black") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Year", y = "Percent", color = "Legend") +
  scale_x_continuous(breaks = seq(2008, 2019, 1)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(round(min(Scale_ML_Imp$share), 
                                                          -1),
                                  round(max(Scale_ML_Imp$share), -1), 20)) +
  scale_fill_manual(labels = c("Imports", 
                               "Landings"),
                    values = colBrew2) +
  guides(fill = guide_legend(title = NULL)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "black"),
        text = element_text(size = 12, family = "serif"))

#Save figure.
save_plot("Figures/Mainland_Imp_Share.png", Mainland_Imp_Share_Figure, 
          base_aspect_ratio = 2)



#---------------
#Figures 9, 18, 27. Annual share of local [spp.] consumption in Hawai'i.
#---------------
#Create object for figure.
HI_Imp_Share_Figure <- ggplot(Scale_HI_Imp, 
                              aes(x = YR, y = share, fill = Production_Share)) +
  geom_bar(stat = "identity", colour = "black") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Year", y = "Percent", color = "Legend") +
  scale_x_continuous(breaks = seq(2008, 2019, 1)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(round(min(Scale_HI_Imp$share), 
                                                          -1), 
                                  round(max(Scale_HI_Imp$share), -1), 20)) +
  scale_fill_manual(labels = c("Imports", 
                               "Landings"), 
                    values = colBrew2) +
  guides(fill = guide_legend(title = NULL)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .1, color = "black"),
        text = element_text(size = 12, family = "serif"))

#Save figure.
save_plot("figures/HI_Imp_Share.png", HI_Imp_Share_Figure, 
          base_aspect_ratio = 2)