mortDat <- read.csv("https://www.dropbox.com/s/jsfh8rq2ez0wsj8/ReformattedManateeMortalityData.csv?dl=1")
cReg <- read.csv("https://www.dropbox.com/s/mpxmf4qn7aelr9v/CountyRegions.csv?dl=1")

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
