#INFO3300 Kyle Ruhl -- Clean Mapping Dataset Script
#Original Dataset from the NTSB aviation accident database thru 7/2022
#see https://www.kaggle.com/datasets/khsamaha/aviation-accident-database-synopses

#load data and packages
library(dplyr)
library(stringr)
AviationData <- read.csv("~/AviationData.csv")

#filter to only US accidents that have Lat/Long
US_Accidents <- AviationData %>% filter(Country == "United States") %>% filter(Latitude != "") %>% filter(Longitude != "") %>% filter(Investigation.Type == "Accident") %>% select(-1,-3,-9,-10,-13,-14,-15,-16,-17,-18,-19,-20,-21,-22,-23,-28,-29,-30,-31)

#lots of data points, lets filter down into only fatal accidents
FatalUSAccidents <- US_Accidents %>% filter(Injury.Severity != "Non-Fatal") %>% filter(Injury.Severity != "") 


# !! lat/lon after 2008-01-01 is in a different format from previous lat/lon, convert later <2007 dates to same coordinate format for d3 plotting/scale

correct_format <- FatalUSAccidents[grepl("-",FatalUSAccidents$Longitude),] #separate longitudes that are in correct format with negative values
tofix <- FatalUSAccidents[!grepl("-",FatalUSAccidents$Longitude),] #separate longitudes that are in correct format with negative values

tofix$Latitude = str_remove(tofix$Latitude,"^0+") #remove leading zeros
tofix$Latitude = str_remove(tofix$Latitude,"N") #remove N North char

tofix$Longitude = str_remove(tofix$Longitude,"^0+") #remove leading 0s
tofix$Longitude = str_remove(tofix$Longitude,"W") #remove W char
tofix$Longitude = str_remove(tofix$Longitude,"E") #remove stray E char


# Use insert function to add decimal point after correct number and add negative to longitudes

fun_insert <- function(x, pos, insert) {
  gsub(paste0("^(.{", pos, "})(.*)$"), paste0("\\1", insert, "\\2"), x) }

#fix latitudes by inserting decimal after 2 (common USA lat values should be [20,60])
tofix$Latitude <- fun_insert(x = tofix$Latitude, pos = 2, insert = ".")

#longitudes more complicated, some values over 100, some under... start by seperating into 2 df depending on where decimal point will be placed:
lon1=tofix[grepl("^1",tofix$Longitude),] #longitudes that start with 1 (100s)
lon2=tofix[!grepl("^1",tofix$Longitude),] #longitudes that start with nums less than 1 (<100)

lon1$Longitude <- fun_insert(x = lon1$Longitude, pos = 3, insert = ".") #insert .
lon1$Longitude <- fun_insert(x = lon1$Longitude, pos = 0, insert = "-") #insert -

lon2$Longitude <- fun_insert(x = lon2$Longitude, pos = 2, insert = ".")
lon2$Longitude <- fun_insert(x = lon2$Longitude, pos = 0, insert = "-")

full_fixed <- rbind(lon1,lon2) #combine two longitudes into one dataset
merged_format <- rbind(correct_format,full_fixed) %>% write.csv('FatalUSAccidents.csv', row.names = F) #save as csv
