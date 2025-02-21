#Kyle Ruhl INFO 3300 P2 R script creating second dataset
#Create second dataset for menu interactivity, merging census data for each state with sums computed from plane accident dataset. 

#imports
library(dplyr)
library(tidyr)
library(stringr)

AviationData <- read.csv("~/AviationData.csv",stringsAsFactors = TRUE)

#Filter into US fatal accidents only, this time doesnt matter if entry has lat/lon
US_Accidents <- AviationData %>% filter(Country == "United States") %>% select(-1,-3,-9,-10,-13,-14,-15,-16,-17,-18,-19,-20,-21,-22,-23,-28,-29,-30,-31) %>% filter(Investigation.Type == "Accident") %>% filter(Injury.Severity != "Non-Fatal") %>% filter(Injury.Severity != "")


#split location into city / state cols
US_Accidents[,13:14] = str_split_fixed(US_Accidents$Location, ", ",2)
colnames(US_Accidents)[13:14] <- c("City","State") 

US_Accidents$State = factor(US_Accidents$State) #factor by state abbr


#compute averages by state for hover table

StateAvg<- US_Accidents %>% group_by(State) %>% summarise(n()) #sum of total fatal accidents by state
StateAvg$TotalFatalities<- US_Accidents %>% group_by(State) %>% summarise(Freq = sum(Total.Fatal.Injuries, na.rm = TRUE)) #sum of fatalities by state

#import state statistics csv to merge with second dataset
state_stats <- read.csv("~/Desktop/state_stats.csv")

StatesData <- merge(state_stats,StateAvg, by="State") #merge by state abbr, removes islands which other dataset didnt have included

#clean up, change column names
colnames(StatesData) <- c("State","State_Abbr","FIPS_Code","2010_Population","Area(sq_mi)","Number_Of_Fatal_Accidents","Sum_Of_Fatalities")

StatesData %>% write.csv('ByStateData.csv', row.names = F) #save as csv
