#INIT DATA FRAMES
mortDat <- read.csv("https://www.dropbox.com/s/jsfh8rq2ez0wsj8/ReformattedManateeMortalityData.csv?dl=1") #Loads in mortality data 
cReg <- read.csv("https://www.dropbox.com/s/mpxmf4qn7aelr9v/CountyRegions.csv?dl=1") #Loads Classifier for Country Region

b <- mortDat$County[mortDat$County == c(cReg$North.West.Central[1])] 

southCentralWest <- vector(length = length(mortDat$County)) #Holds booleans for region
northWestCentral <- vector(length = length(mortDat$County)) #Holds booleans for region
northEastCentral <- vector(length = length(mortDat$County)) #Holds booleans for region
southCentralEast <- vector(length = length(mortDat$County)) #Holds booleans for region

for(i in 1:length(cReg$South.Central.West)){
  southCentralWest <- (southCentralWest | (mortDat$County == as.character(cReg$South.Central.West[i]))) #Places a True if County is a member of the region
}
for(i in 1:length(cReg$North.West.Central)){
  northWestCentral <- (northWestCentral | (mortDat$County == as.character(cReg$North.West.Central[i]))) #Places a True if County is a member of the region
}
for(i in 1:length(cReg$North.East.Central)){
  northEastCentral <- (northEastCentral | (mortDat$County == as.character(cReg$North.East.Central[i]))) #Places a True if County is a member of the region
}
for(i in 1:length(cReg$South.Central.East)){
  southCentralEast <- (southCentralEast | (mortDat$County == as.character(cReg$South.Central.East[i]))) #Places a True if County is a member of the region
}

checker = southCentralWest | northWestCentral | northEastCentral | southCentralEast #Make sure that all the data gets assigned regions. 

mortDat[, "Region"] = ifelse(southCentralWest == T,F,F)
mortDat[, "Region"][southCentralWest] = "SouthCentralWest"
mortDat[, "Region"][northWestCentral] = "NorthWestCentral"
mortDat[, "Region"][northEastCentral] = "NorthEastCentral"
mortDat[, "Region"][southCentralEast] = "SouthCentralEast"

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
mortDat <- mortDat[naLOGI, ] #Removes the NAs from the data. Removes NAs from dataframe. 
mortDat <- mortDat[mortDat$Size..cm. > 0, ] #Removes non positive values.
names(mortDat)[names(mortDat)=="Size..cm."] <- "Sizecm" # Changes the name of the column to Sizecm. 



####Create Collision Boolean#####

mortDat["Collision"] <- ifelse(mortDat$Probable.Cause == "Human Related: Watercraft Collision", T, F) #Adds a value to the datafram for the occurence of death by collision

#DATA FRAMES INITIALIZED

#AGGREGATION ON DATA
aggregate(Collision ~ Sex, mortDat, length) #Counting Sex Information
aggregate(Collision ~ Region, mortDat, length) #Counting the region frequency 
max(aggregate(Collision ~ Waterway, mortDat, length)["Collision"]) # Single water way with 1162. Wow. 
aggregate(Collision ~ County, mortDat, length) # Much more meaningful

plot(Collision ~ Sizecm, mortDat, pch = "|",  xlab = "Manatee size(cm)",
     ylab = "Collision", main="Collision Vs Manatee Size")  
mdm <- mortDat[mortDat$Sex == "M",]
mdf <- mortDat[mortDat$Sex == "F",]
mdc <- mortDat[(mortDat$Sex == "M") | (mortDat$Sex == "F")  ,]
points(mdf$Sizecm, mdf$Collision, pch = "|", col = "blue") # Mort Dat Female
points(mdm$Sizecm, mdm$Collision, pch = "|", col = "green")  # Mort Dat Male

aggregate(Collision ~ Sex, mortDat, sum) # Males have a higher incidence of collisions but marginally
aggregate(Sizecm ~ Collision, mortDat, mean) 
# Size is considerably higher in average with Collision. 
# 271.0433 average in collision vs 218.0519 without. 


####Hypothesis####
#Null: Collision rate of manatee cause of death does not
# vary with the size, sex, region, or season. 

####Choose Model ####

#GLM: 3 Categorical predictor and 1 Continuous predictor with binomial categorical response. 

####Visualization?####
wc <- mortDat[mortDat['Collision'] == TRUE, ]  #Population with collision
nc <- mortDat[mortDat['Collision'] == FALSE, ] #Population without collision

aggregate(Collision ~ Season, wc, length) #Some Evidence of a difference by Season. 
# Season Collision
# 1   Fall       371
# 2 Spring       660
# 3 Summer       578
# 4 Winter       538

####Season Visulization ####

season = wc["Season"]
season.freq = table(season)
barplot(season.freq)
colors = c("brown", "green", "yellow", "blue") 
barplot(season.freq, col=colors, 	legend = rownames(season.freq), main="Number of Collisions ")

####Random sampling based Nonmetric Multidimensional Scaling Code ####

library(vegan)

alt <- mortDat[!(mortDat["Sex"] == "U"),]
alt <- alt[,c("Region", "Sex", "Sizecm", "Season", "Collision")]
altP <- alt[alt$Collision == TRUE, c("Region", "Sex", "Sizecm", "Season", "Collision")] #Collision population
altNP <- alt[alt$Collision == FALSE, c("Region", "Sex", "Sizecm", "Season", "Collision")] #Non-Collision population
altR <- rbind(altP[sample(1:length(altP$Region), 500),], altNP[sample(1:length(altNP$Region), 500),]) #New sample set composed of 500 datapoints each from collision and non-collision. 

#Ordinates categorical variables to be numerical factors
altR$Region <- as.numeric(factor(altR$Region , levels=unique(alt$Region))) 
altR$Sex <- as.numeric(factor(altR$Sex , levels=unique(alt$Sex)))
altR$Season <- as.numeric(factor(altR$Season , levels=unique(alt$Season)))
altR$Collision <- as.numeric(factor(altR$Collision , levels=unique(alt$Collision)))

c.mds <- metaMDS(altR[,1:4], zerodist="add") 
#str(c.mds)
par(mfcol = c(1,1))
fig <- ordiplot(c.mds, type = "none", main = "NMDS for Collision and Non-Collision Communities")
points(fig, "sites", pch=21, col=c("dodgerblue", "red")[altR$Collision], bg="white", cex=1.1)
ordihull(c.mds, altR$Collision == "2", display = "sites", draw = "polygon")


altR$nmds1 <- c.mds$points[,1]
altR$nmds2 <- c.mds$points[,2]

pairs(altR[,6:7], col= c("dodgerblue", "red")[altR$Collision], pch = 21, main = "NMDS for Collision and Non-Collision Communities")


####CEMENTING: Model Selection Process####

library(MuMIn)
library(xtable)
#Model Selection

mortD <- mortDat[!(mortDat["Sex"] == "U"),]

options(na.action = "na.fail") #Specifies NA treatment
subset <- mortD[c("Sizecm","Region", "Sex", "Season","Collision")] #Subsets data to the explanatory and reposn variables only. 
fm1 <- glm(Collision ~ Sizecm*Region*Sex*Season, data = subset, family = binomial, na.action = na.fail) #Maximal Model
dd <- dredge(fm1) #Produces model selection table based on AIC. 
final <- glm(Collision ~ Region + Season + Sex + Sizecm + Region:Season + Region:Sex + Season:Sex + Sex:Sizecm, data = mortD, family = binomial, na.action = na.fail)
maximal <- glm(Collision ~ Sizecm*Region*Sex*Season, data = subset, family = binomial, na.action = na.fail)

#STEP AIC Method#

model <- glm(Collision ~ Sizecm*Region*Sex*Season, data = mortD, family = binomial, na.action = na.fail) # Maximal Model
summary(model)

model2 <- step(model) #Step AIC model reduction 
summary(model2)

#resulting model
stepAICModel <- glm(formula = Collision ~ Sizecm + Region + Sex + Season + Sizecm:Sex + 
      Region:Sex + Sizecm:Season + Region:Season + Sex:Season + 
      Sizecm:Sex:Season, family = binomial, data = mortD, na.action = na.fail) 

oneStep  <- glm(formula = Collision ~ Sizecm + Region + Sex + Season + Sizecm:Sex + 
                  Region:Sex + Region:Season + Sex:Season + 
                  Sizecm:Sex:Season, family = binomial, data = mortD, na.action = na.fail) #Removal of Sex:Season in comparison to StepAIC model

oneStepAlt <- glm(formula = Collision ~ Sizecm + Region + Sex + Season + Sizecm:Sex +  
                  Region:Sex + Sizecm:Season + Region:Season + Sex:Season ,
                  family = binomial, data = mortD, na.action = na.fail) #Removal of Sizecm:Sex:Season in comparison to StepAIC model

dredgeModel <- glm(Collision ~ Region + Season + Sex + Sizecm + Region:Season + Region:Sex + Season:Sex + Sex:Sizecm, data = mortD, family = binomial, na.action = na.fail)
maximal <- glm(Collision ~ Sizecm*Region*Sex*Season, data = subset, family = binomial, na.action = na.fail)


#Anova Table Generation Code. 

xtable(anova(maximal, stepAICModel, test="Chi")) #Anova Table for maximal model vs Dredge model. 
xtable(anova(maximal, dredgeModel, test="Chi")) #Anova Table for maximal model vs Dredge model. 
xtable(anova(maximal, oneStep, oneStepAlt, dredgeModel, test="Chi")) #Anova Table for building down from maximal to AICmodel to dredge model.  
