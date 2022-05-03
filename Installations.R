#Load data and create objects to be used in OverviewSections.R and the 3 
#pelagic species profiles.


library(tidyverse)

#----------------
#Load various codes for objects.
#----------------
Product_codes <- read.csv("Data/source/ProductCodes.csv", strip.white = TRUE)
Country_codes <- read.csv("Data/source/CountryCodes.csv", strip.white = TRUE)
District_codes <- read.csv("Data/source/DistrictCodes.csv", strip.white = TRUE)



#----------------
#Load CPI data and save as RDS object.
#----------------
CPI <- read.csv("Data/source/CPI.csv", strip.white = TRUE)

saveRDS(CPI, "Data/intermediate/CPI.RDS")



#----------------
#Load and clean Honolulu trade data.
#----------------

#Create trade data object
dat <- read.csv("Data/source/AllTrade2008-2019.csv", strip.white = TRUE) %>%
  left_join(Product_codes[, (1:5)], by = "Product.ID") %>%
  left_join(Country_codes, by = "Country.ID") %>%
  left_join(y = CPI[, c(1, 3)], by = "YR", all.x = TRUE) %>%
  filter(Product != "HERRING, ANCHOVY, SARDINE, SPRAT, MACKEREL, INDIAN MACKEREL, SEERFISH, JACK AND HORSE MACKEREL, JACKS, CREVALLES, COBIA, SILVER POMFRETS, PACIFIC SAURY, SCAD, CAPELIN, SWORDFISH, KAWAKAWA, BONITO, MARLIN, SAILFISH, SPEARFISH DRIED",
         Product != "SQUID (LOLIGO NSPF) DRIED/SALTED/BRINE",
         Product != "SQUID (LOLIGO NSPF) FROZEN",
         Product != "SQUID (LOLIGO NSPF) LIVE/FRESH",
         Product != "CUTTLEFISH,SQUID PRODUCTS PREPARED DINNERS",
         Product != "SQUID (LOLIGO NSPF) PREPARED/PRESERVED",
         Product != "SQUID (LOLIGO NSPF) FROZEN/DRIED/SALTED/BRINE") %>%
  mutate(RDollars = round(Dollars * CPI.adjustment, 2)) %>% 
  select(-District.ID, -Country.ID) %>%
  mutate(Trade = recode(Trade, `EXP` = "E", `IMP` = "I", `REX` = "R"))

names(dat) <- gsub("Product.Form", "Form", names(dat))

names(dat) <- gsub("Product.Category", "Species", names(dat))

#Create objects for conversion to live weight.
ConvertSpecies <- c("Bigeye tuna|Yellowfin tuna|Albacore tuna|Skipjack tuna|Unspecified tuna|Swordfish|Dolphinfish|Bluefin tuna")
ConvertForms <- c("Fresh|Frozen|Fillet|Meat|Preserved|Steaks|Head on|Head off|Dried")

#Convert to live weight
dat$cPounds <- ifelse(grepl("Bigeye tuna", dat$Species, ignore.case = TRUE) 
                      & dat$FormDesc == "Fresh", dat$Pounds * 1.04,
               ifelse(grepl("Bigeye tuna", dat$Species, ignore.case = TRUE) 
                      & dat$FormDesc == "Frozen", dat$Pounds * 1.205, 
               ifelse(grepl("Yellowfin tuna", dat$Species, ignore.case = TRUE) 
                      & dat$FormDesc == "Fresh", dat$Pounds * 1,
               ifelse(grepl("Yellowfin tuna", dat$Species, ignore.case = TRUE) 
                      & dat$FormDesc == "Gutted Head-Off Frozen", dat$Pounds * 1.25,
               ifelse(grepl("Yellowfin tuna", dat$Species, ignore.case = TRUE) 
                      & dat$FormDesc == "Gutted Head-On Frozen", dat$Pounds * 1.10,
               ifelse(grepl("Yellowfin tuna", dat$Species, ignore.case = TRUE) 
                      & grepl("Whole Frozen", dat$FormDesc, ignore.case = TRUE), 
                      dat$Pounds * 1.00,
               ifelse(grepl("Yellowfin tuna", dat$Species, ignore.case = TRUE) 
                      & dat$FormDesc == "Frozen", dat$Pounds * 1.1,
               ifelse(grepl("Albacore tuna", dat$Species, ignore.case = TRUE) 
                      & dat$FormDesc == "Fresh", dat$Pounds * 1.00,
               ifelse(grepl("Albacore tuna", dat$Species, ignore.case = TRUE) 
                      & dat$FormDesc == "Frozen", dat$Pounds * 1.10,
               ifelse(grepl("Albacore tuna", dat$Species, ignore.case = TRUE) 
                      & dat$FormDesc == "Preserved", dat$Pounds * 1.92,
               ifelse(grepl("Skipjack tuna", dat$Species, ignore.case = TRUE) 
                      & dat$FormDesc == "Fresh", dat$Pounds * 1.00,
               ifelse(grepl("Skipjack tuna", dat$Species, ignore.case = TRUE) 
                      & dat$FormDesc == "Frozen", dat$Pounds * 1.10,
               ifelse(grepl("Unspecified tuna", dat$Species, ignore.case = TRUE) 
                      & dat$FormDesc == "Fresh", dat$Pounds * 1.00,
               ifelse(grepl("Unspecified tuna", dat$Species, ignore.case = TRUE) 
                      & dat$FormDesc == "Frozen", dat$Pounds * 1.125,
               ifelse(grepl("Unspecified tuna", dat$Species, ignore.case = TRUE) 
                      & grepl("Fillet|Meat|Preserved", dat$FormDesc, ignore.case = TRUE), 
                      dat$Pounds * 1.92,
               ifelse(grepl("Swordfish", dat$Species, ignore.case = TRUE) 
                      & dat$FormDesc == "Fresh", dat$Pounds * 1.016,
               ifelse(grepl("Swordfish", dat$Species, ignore.case = TRUE) 
                      & dat$FormDesc == "Frozen", dat$Pounds * 1.35,
               ifelse(grepl("Swordfish", dat$Species, ignore.case = TRUE) 
                      & grepl("Fillet|Meat", dat$FormDesc, ignore.case = TRUE), 
                      dat$Pounds * 2.47,
               ifelse(grepl("Dolphinfish", dat$Species, ignore.case = TRUE) 
                      & dat$FormDesc == "Fresh", dat$Pounds * 1.00,
               ifelse(grepl("Dolphinfish", dat$Species, ignore.case = TRUE) 
                      & dat$FormDesc == "Frozen", dat$Pounds * 1.20,
               ifelse(grepl("Dolphinfish", dat$Species, ignore.case = TRUE) 
                      & grepl("Fillet", dat$FormDesc, ignore.case = TRUE),
                      dat$Pounds * 3.33,
               ifelse(dat$Product == "SQUID NSPF FROZEN/DRIED/SALTED/BRINE", 
                      dat$Pounds * 1.55,
               ifelse(dat$Product == "SQUID NSPF PREPARED/PRESERVED", 
                             dat$Pounds * 1.28,
               ifelse(dat$Product == "SQUID NSPF FILLET FROZEN", 
                             dat$Pounds * 1.45,
               ifelse(dat$Product == "SQUID NSPF FROZEN", 
                             dat$Pounds * 1.00,
               ifelse(dat$Product == "SQUID NSPF DRIED/SALTED/BRINE", 
                             dat$Pounds * 1.55,
               ifelse(dat$Product == "SQUID FILLET FROZEN", 
                             dat$Pounds * 1.45,  
               ifelse(dat$Product == "SQUID NSPF LIVE/FRESH", 
                             dat$Pounds * 1.00,    
               ifelse(dat$Product == "SHARK NSPF FROZEN", 
                             dat$Pounds * 2.00,  
               ifelse(dat$Product == "SHARK FINS DRIED", 
                             dat$Pounds * 1.00, 
               ifelse(dat$Product == "SHARK NSPF FRESH", 
                                    dat$Pounds * 1.24,
              ifelse(dat$Product == "TUNA BLUEFIN ATLANTIC FROZEN", 
                             dat$Pounds * 1.11,
              ifelse(dat$Product == "TUNA BLUEFIN FROZEN", 
                                    dat$Pounds * 1.17,
              ifelse(dat$Product == "TUNA BLUEFIN PACIFIC FROZEN", 
                            dat$Pounds * 1.18,
              ifelse(dat$Product == "TUNA BLUEFIN SOUTHERN FROZEN", 
                            dat$Pounds * 1.176,
               ifelse(!grepl(ConvertForms, dat$FormDesc, ignore.case = TRUE) 
                      & !grepl(ConvertSpecies, dat$Species, ignore.case = TRUE),
                      dat$Pounds, dat$Pounds))))))))))))))))))))))))))))))))))))


#Coerce object to numeric.
dat$cPounds <- as.numeric(dat$cPounds)

#Fix mahimahi issues
dat$Species <- gsub("DOLPHINFISH", "MAHIMAHI", dat$Species)

#Create Unit Price column
dat <- dat %>%
  mutate(dat, Price = ifelse(dat$Trade == "I", round(RDollars / cPounds, 2),
                             round(RDollars / Pounds, 2))) 


#Save converted weight to RDS file.
saveRDS(dat, "Data/intermediate/dat.RDS")


#----------------
#Load and clean continental US + Honolulu trade data for SummaryTable....R scripts
#----------------
USdat <- read.csv("Data/source/USTradeDat.csv", strip.white = TRUE) %>%
  left_join(Product_codes[, (1:5)], by = "Product.ID") %>%
  left_join(Country_codes, by = "Country.ID") %>%
  left_join(District_codes, by = "District.ID") %>%
  left_join(y = CPI[, c(1, 3)], by = "YR", all.x = TRUE) %>%
  mutate(RDollars = round(Dollars * CPI.adjustment, 2),
         Pounds = Volume * 2.20462) %>%
  select(-District.ID, -Country.ID) %>%
  mutate(Trade = ifelse(Trade.Type == 0, "I", "E")) %>%
  select(-"Trade.Type") %>%
  filter(YR %in% c(2008:2019)) %>%
  filter(`District` %in% c("SAN FRANCISCO, CA","BOSTON, MA", "NEW YORK, NY",
                           "NORFOLK, VA", "SAVANNAH, GA", "LOS ANGELES, CA",
                           "CHICAGO, IL", "SAINT LOUIS, MO", "MIAMI, FL",
                           "PORTLAND, ME", "SEATTLE, WA", "PEMBINA, ND",
                           "OGDENSBURG, NY", "BUFFALO, NY", "DETROIT, MI",
                           "NEW ORLEANS, LA", "LAREDO, TX", 
                           "DALLAS-FORT WORTH, TX", "WASHINGTON, DC",
                           "HOUSTON-GALVESTON, TX", "HONOLULU, HI", 
                           "PORTLAND, OR", "BALTIMORE, MD", "ANCHORAGE, AK",
                           "SAN DIEGO, CA", "NOGALES, AZ", "CLEVELAND, OH",
                           "TAMPA, FL", "CHARLOTTE, NC", "SAINT ALBANS, VT",
                           "PHILADELPHIA, PA", "EL PASO, TX", "GREAT FALLS, MT",
                           "CHARLESTON, SC", "MINNEAPOLIS, MN", "MOBILE, AL",
                           "DULUTH, MN", "MILWAUKEE, WI", "PROVIDENCE, RI",
                           "PORT ARTHUR, TX"))


names(USdat) <- gsub("Product.Form", "Form", names(USdat))

names(USdat) <- gsub("Product.Category", "Species", names(USdat))

#Convert to live weight.
USdat$cPounds <- ifelse(grepl("Bigeye tuna", USdat$Species, ignore.case = TRUE) 
                        & USdat$FormDesc == "Fresh", USdat$Pounds * 1.04,
                 ifelse(grepl("Bigeye tuna", USdat$Species, ignore.case = TRUE) 
                        & USdat$FormDesc == "Frozen", USdat$Pounds * 1.20,
                 ifelse(grepl("Yellowfin tuna", USdat$Species, ignore.case = TRUE) 
                        & USdat$FormDesc == "Fresh", USdat$Pounds * 1.00,
                 ifelse(grepl("Yellowfin tuna", USdat$Species, ignore.case = TRUE) 
                        & USdat$FormDesc == "Gutted Head-Off Frozen", 
                        USdat$Pounds * 1.25,
                 ifelse(grepl("Yellowfin tuna", USdat$Species, ignore.case = TRUE) 
                        & USdat$FormDesc == "Gutted Head-On Frozen", 
                        USdat$Pounds * 1.10,
                 ifelse(grepl("Yellowfin tuna", USdat$Species, ignore.case = TRUE) 
                         & USdat$FormDesc == "Whole", USdat$Pounds * 1.00,
                 ifelse(grepl("Yellowfin tuna", USdat$Species, ignore.case = TRUE) 
                        & USdat$FormDesc == "Frozen", USdat$Pounds * 1.10,
                 ifelse(grepl("Albacore tuna", USdat$Species, ignore.case = TRUE) 
                        & USdat$FormDesc == "Fresh", USdat$Pounds * 1.00,
                 ifelse(grepl("Albacore tuna", USdat$Species, ignore.case = TRUE) 
                        & USdat$FormDesc == "Frozen", USdat$Pounds * 1.10,
                 ifelse(grepl("Albacore tuna", USdat$Species, ignore.case = TRUE) 
                        & USdat$FormDesc == "Preserved", USdat$Pounds * 1.92,
                 ifelse(grepl("Skipjack tuna", USdat$Species, ignore.case = TRUE) 
                        & USdat$FormDesc == "Fresh", USdat$Pounds * 1.00,
                 ifelse(grepl("Skipjack tuna", USdat$Species, ignore.case = TRUE) 
                        & USdat$FormDesc == "Frozen", USdat$Pounds * 1.10,
                 ifelse(grepl("Unspecified tuna", USdat$Species, ignore.case = TRUE) 
                        & USdat$FormDesc == "Fresh", USdat$Pounds * 1.00,
                 ifelse(grepl("Unspecified tuna", USdat$Species, ignore.case = TRUE) 
                        & USdat$FormDesc == "Frozen", USdat$Pounds * 1.125,
                 ifelse(grepl("Unspecified tuna", USdat$Species, ignore.case = TRUE) 
                        & grepl("Fillet|Meat|Preserved", 
                                USdat$FormDesc, ignore.case = TRUE),
                        USdat$Pounds * 1.92,
                 ifelse(grepl("Swordfish", USdat$Species, ignore.case = TRUE) 
                        & USdat$FormDesc == "Fresh", USdat$Pounds * 1.016,
                 ifelse(grepl("Swordfish", USdat$Species, ignore.case = TRUE) 
                        & USdat$FormDesc == "Frozen", USdat$Pounds * 1.35,
                 ifelse(grepl("Swordfish", USdat$Species, ignore.case = TRUE) 
                        & grepl("Fillet|Meat|Steaks", 
                                 USdat$FormDesc, ignore.case = TRUE),
                         USdat$Pounds * 2.47,
                 ifelse(grepl("Dolphinfish", USdat$Species, ignore.case = TRUE) 
                        & USdat$FormDesc == "Fresh", USdat$Pounds * 1.00,
                 ifelse(grepl("Dolphinfish", USdat$Species, ignore.case = TRUE) 
                        & USdat$FormDesc == "Frozen", USdat$Pounds * 1.20,
                 ifelse(grepl("Dolphinfish", USdat$Species, ignore.case = TRUE) 
                        & grepl("Fillet", USdat$FormDesc, ignore.case = TRUE),
                          USdat$Pounds * 3.33,
                 ifelse(grepl("Bluefin tuna", USdat$Species, ignore.case = TRUE) 
                        & USdat$FormDesc == "Frozen", USdat$Pounds * 1.176,
                 ifelse(!grepl(ConvertForms, USdat$FormDesc, ignore.case = TRUE) 
                        & !grepl(ConvertSpecies, USdat$Species, 
                                   ignore.case = TRUE),
                          USdat$Pounds, USdat$Pounds)))))))))))))))))))))))

USdat$Species <- gsub("DOLPHINFISH", "MAHIMAHI", USdat$Species)

USdat$cPounds <- as.numeric(USdat$cPounds)

#Make Unit Price column
USdat <- USdat %>%
  mutate(USdat, Price = ifelse(USdat$Trade == "I", round(RDollars / cPounds, 2),
                             round(RDollars / Pounds, 2)))  

#Save as csv file for reference, because object is too large to view in R Studio!
#write.csv(USdat, "Data/intermediate/USdat.csv")

#Save as RDS object.
saveRDS(USdat, "Data/intermediate/USdat.RDS")


#----------------
#Load Hawai'i landings data.
#----------------
Landat <- read.csv("Data/source/Landings.csv", strip.white = TRUE) %>%
  filter(YR %in% c(2008:2019)) %>%
  merge(y = CPI[, c(1, 3)], by = "YR", all.x = TRUE) %>%
  mutate_at(.vars = vars(Price, Value), 
            .funs = funs(round(. * CPI.adjustment)), 2) 
#Multiplies both Value and Price by CPI, doesn't multiply by Value THEN 
#calculate Price.

#Save as RDS object.
saveRDS(Landat, "Data/intermediate/Landat.RDS")



#----------------
#Load US landings data.
#----------------
USLanddat <- read.csv("Data/source/USLandings.csv", strip.white = TRUE) %>%
  merge(y = CPI[, c(1, 3)], by = "YR", all.x = TRUE) %>%
  mutate_at(.vars = vars(Dollars, UnitValue), 
            .funs = funs(round(. * CPI.adjustment)), 2)
#Multiplies both Dollars and UnitValue by CPI, doesn't multiply by Dollars THEN 
#calculate UnitValue.

USLanddat$Species <- gsub("DOLPHINFISH", "MAHIMAHI", USLanddat$Species)

#Save as csv file for reference, because individual data too long to view in 
#RStudio.
write.csv(USLanddat, "Data/intermediate/USLanddat.csv")

#Save as RDS object.
saveRDS(USLanddat, "Data/intermediate/USLanddat.RDS")



#----------------
#Load monthly prices data.
#----------------
Monthdat <- read.csv("Data/source/MonthlyPrices.csv", strip.white = TRUE) %>%
  merge(y = CPI[, c(1, 3)], by = "YR", all.x = TRUE) %>%
  subset(select = c(CPI.adjustment, YR:Yellowfin.tuna)) %>%
  mutate_at(.vars = vars(Albacore.tuna, Bigeye.tuna, Black.marlin, Blue.marlin, 
                         Mahimahi, Moonfish.opah, Pomfret, Skipjack.tuna, 
                         Striped.marlin, Swordfish, Wahoo, Yellowfin.tuna), 
            .funs = funs(round(. * CPI.adjustment)), 2)

#Save as RDS object.
saveRDS(Monthdat, "Data/intermediate/Monthdat.RDS")