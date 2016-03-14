1. The data used in this analysis was provided in email from a representative of the Florida Fish and Wildlife Conservation Commission in xls format. 

2. The dataset has been altered a bit for the analysis. The dataset originally was an xls file that contained different pages for each year. The pages were removed and the file was converted, by hand, to one Comma Separated File, CSV, for use in this analysis. 

Additionally a few columns were added to the data. 

A Column for Season was added. Calculations for season were based on the dates provided in the dataset. 

A column for Collision (True or False) was created to identify all manatees that died from collision. 

Finally: 

Data was removed for the analysis. 
All sexes that were undetermined were removed. Sizes that were unavailable(NA) were removed.

Manipulation script can be found here and at bottom of this file: https://www.dropbox.com/s/gjbk1gksmlw8u7w/manipulationScriptFormal.R?dl=0

3. Metadata

The original Data set provides information regarding the death of manatees in Florida.  

The columns are are listed below with brief descriptions:
County: Provides the county in which the manatee was found. 
Date: Provides the date that the Manatee was found. (MM/DD/YYY)
Field ID: Provides the unique identification for the Manatee found. 
Sex: Provides the sex of the deceased manatee.(Male(M), Female(F), or Unidentified(U))
Size(cm): Provides the size of the deceased manatee in centimeters. 
Waterway: List the name of the waterway that the deceased manatee was found in. 
City: Provides the city that the Manatee was found in. 
Probable Cause: Provides the most likely cause of death for the manatee. (Human Related: Flood Gate/Canal Lock, Human Related: Watercraft Collision , Human Related: Other, Natural: Cold Stress , Natural: Other, Perinatal(<= 150 cm), Undetermined: Other, Undetermined: Too Decomposed, and Verified: Not Recovered) 

Dataset is converted in the statistical analysis through R to the following: 

County: Provides the county in which the manatee was found. 
Sex: Provides the sex of the deceased manatee.(Male(M), Female(F), or Unidentified(U))
Size(cm): Provides the size of the deceased manatee in centimeters. 
Season: Season in which the manatee was found. (Winter, Spring, Summer, or Fall)
Collision: Wether or not a manatee was killed by collision. (T/F)




————Data Manipulation Script————

mortDat <- read.csv("https://www.dropbox.com/s/jsfh8rq2ez0wsj8/ReformattedManateeMortalityData.csv?dl=1") #Original Data Script
cReg <- read.csv("https://www.dropbox.com/s/mpxmf4qn7aelr9v/CountyRegions.csv?dl=1") #Counties segmented into regions. 

b <- mortDat$County[mortDat$County == c(cReg$North.West.Central[1])]

southCentralWest <- vector(length = length(mortDat$County)) 
northWestCentral <- vector(length = length(mortDat$County))
northEastCentral <- vector(length = length(mortDat$County))
southCentralEast <- vector(length = length(mortDat$County))

for(i in 1:length(cReg$South.Central.West)){
  southCentralWest <- (southCentralWest | (mortDat$County == as.character(cReg$South.Central.West[i])))
}
for(i in 1:length(cReg$North.West.Central)){
  northWestCentral <- (northWestCentral | (mortDat$County == as.character(cReg$North.West.Central[i]))) #Places a True if County is a member of the region
}
for(i in 1:length(cReg$North.East.Central)){
  northEastCentral <- (northEastCentral | (mortDat$County == as.character(cReg$North.East.Central[i])))
}
for(i in 1:length(cReg$South.Central.East)){
  southCentralEast <- (southCentralEast | (mortDat$County == as.character(cReg$South.Central.East[i])))
}

checker = southCentralWest | northWestCentral | northEastCentral | southCentralEast #Make sure that all the data gets assigned regions. 

mortDat[, "Region"] = ifelse(southCentralWest == T,F,F)
mortDat[, "Region"][southCentralWest] = "SouthWestCentral"
mortDat[, "Region"][northWestCentral] = "NorthWestCentral"
mortDat[, "Region"][northEastCentral] = "NorthEastCentral"
mortDat[, "Region"][southCentralEast] = "SouthEastCentral"

01/01/2014
winter <- vector(length = length(mortDat$Date)) 
spring <- vector(length = length(mortDat$Date))
summer <- vector(length = length(mortDat$Date))
fall <- vector(length = length(mortDat$Date))

mortDat[, "Season"] = ifelse(1==1, F,F) #Initialize a empty vector for season. 

for(i in 1:length(mortDat$Date)){
  if((as.Date(mortDat$Date[i], "%m/%d") >= "2016-03-20") 
     &&
     (as.Date(mortDat$Date[i], "%m/%d") < "2016-06-21")) {
    mortDat$Season[i] <- "Spring"
  }
  else{
    if((as.Date(mortDat$Date[i], "%m/%d") >= "2016-06-21") 
       &&
       (as.Date(mortDat$Date[i], "%m/%d") < "2016-09-22")) {
      mortDat$Season[i] <- "Summer" 
    }
    else{
      if((as.Date(mortDat$Date[i], "%m/%d") >= "2016-09-22") 
         &&
         (as.Date(mortDat$Date[i], "%m/%d") < "2016-12-21")){
        mortDat$Season[i] <- "Fall"
      }else{
        mortDat$Season[i] <- "Winter"
      }
    }
  }
}
naLOGI <- !is.na(mortDat$Size..cm.) #Locations that are not NA. 
mortDat <- mortDat[naLOGI, ] #Removes the NAs from the data. 
mortDat <- mortDat[mortDat$Size..cm. > 0, ]
names(mortDat)[names(mortDat)=="Size..cm."] <- "Sizecm" 

####Create Collision Boolean#####

mortDat["Collision"] <- ifelse(mortDat$Probable.Cause == "Human Related: Watercraft Collision", T, F)

mortD <- mortDat[!(mortDat["Sex"] == "U"),]
