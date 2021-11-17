#% of tuna consumed in Hawai'i that is imported
#Data queried: 7/9/2021 from FOSS data portal www.fisheries.noaa.gov/foss/ 

#NOTE: The output figures overwrite one another, so rerun as needed for the 
#desired calculation.

rm(list = ls())
source("Rscripts/SourceFiles/Installations.R")
source("Rscripts/SourceFiles/ColorFunctionForFigures.R")

#Filter out Atlantic bluefin tuna for accurate consumption calculations
USdat <- USdat %>% 
  filter(Product != "TUNA BLUEFIN ATLANTIC FROZEN")
Landat <- Landat %>% 
  filter(Species != "Atlantic bluefin tuna") 


#--------------------
#Average annual supply and value of all tuna, 2008-2019.
#--------------------
source("Rscripts/SourceFiles/SummaryTableAnnualAverages.R")
US_Summary_fun("TUNA", "tuna") 


#--------------------
#Consumption trends 
#--------------------
#Not reproduced in manuscript, but calculations used in text & final function
#used for calculations in ConsumptionTrends.R.
source("Rscripts/SourceFiles/SummaryTablebyYear.r")
Annual_US_Summary_fun("TUNA", "tuna")

source("Rscripts/SourceFiles/ConsumptionTrends.r")