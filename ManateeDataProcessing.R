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
mortDat <- mortDat[naLOGI, ] #Removes the NAs from the data. 
mortDat <- mortDat[mortDat$Size..cm. > 0, ]
names(mortDat)[names(mortDat)=="Size..cm."] <- "Sizecm" 


# SouthCentralEastSet <- mortDat[mortDat$Region == "SouthCentralEast", ]
# SouthCentralWestSet <- mortDat[mortDat$Region == "SouthCentralWest", ]
# NorthWestCentralSet <- mortDat[mortDat$Region == "NorthWestCentral", ]
# NorthEastCentralSet <- mortDat[mortDat$Region == "NorthEastCentral", ]
# 
# write.csv(summary(SouthCentralEastSet, c("summaryDefault", "table")), "SouthCentralEastTable.csv")
# write.csv(summary(SouthCentralWestSet, c("summaryDefault", "table")), "SouthCentralWestTable.csv")
# write.csv(summary(NorthWestCentralSet, c("summaryDefault", "table")), "NorthWestCentralTable.csv")
# write.csv(summary(NorthEastCentralSet, c("summaryDefault", "table")), "NorthEastCentralTable.csv")


####Create Collision Boolean#####

mortDat["Collision"] <- ifelse(mortDat$Probable.Cause == "Human Related: Watercraft Collision", T, F)


aggregate(Collision ~ Sex, mortDat, length)
aggregate(Collision ~ Region, mortDat, length)
max(aggregate(Collision ~ Waterway, mortDat, length)["Collision"]) # Single water way with 1162. Wow. 
aggregate(Collision ~ County, mortDat, length) # Much more meaningful

#min(aggregate(Collision ~ Waterway, mortDat, length))
plot(Collision ~ Sizecm, mortDat)      # interesting
plot(Collision ~ Sizecm, mortDat, pch = "|",  xlab = "Manatee size(cm)",
     ylab = "Collision", main="Collision Vs Manatee Size")      # interesting


plot(mortDat$Sizecm, mortDat$Collision, pch = "|")
mdm <- mortDat[mortDat$Sex == "M",]
mdf <- mortDat[mortDat$Sex == "F",]
points(mdf$Sizecm, mdf$Collision, pch = "|", col = "blue") # Mort Dat Female
points(mdm$Sizecm, mdm$Collision, pch = "|", col = "green")  # Mort Dat Male

#points(mdf$Region, mdf$Collision, pch = "|", col = "red") # Mort Dat Region
#points(mdm$height, mdm$infect, pch = "|", col = "green")  # Mort Dat Male


aggregate(Collision ~ Sex, mortDat, sum) # Males have a higher incidence of collisions but marginally
aggregate(Sizecm ~ Collision, mortDat, mean) 
# Size is considerably higher in average with Collision. 
# 271.0433 average in collision vs 218.0519 without. 


####Hypothesis####
#Null: Collision rate of manatee cause of death does not
# vary with the size or sex of the manatee. 
#Hypothesis: Size and sex of a mantee impact their cause of death. 


####Choose Model ####

#GLM: 1 Categorical predictor and 1 Continuous predictor with binomial categorical response. 



#### 4 select best model via backward elimination

m1 <- glm(Collision ~ Sizecm*Sex, data = mortDat, family = binomial)
m2 <- glm(Collision ~ Sizecm + Sex, data=mortDat, family = binomial)
m3 <- glm(Collision ~ Sizecm, data=mortDat, family = binomial)
m4 <- glm(Collision ~ Sex, data=mortDat, family = binomial)

anova(m1, m2, test= "Chi") # m4 better
anova(m2, m4, test= "Chi") # m4 better
anova(m2, m3, test= "Chi") # m3 better
anova(m3, m4, test= "Chi") # No probability??? shreggy

summary(m3)
summary(m4)

####Check The Models ####
par(mfcol=c(2,1))
plot(m3) # model looks appropriate.
par(mfcol=c(1,1))

par(mfcol=c(2,2))
plot(m4) # model looks appropriate.
par(mfcol=c(1,1))


#### Report Parameters #### 

plot(mortDat$Sizecm, mortDat$Collision, pch = "|", xlab = "Manatee size, cm",
     ylab = "Collision")
points(mdf$Sizecm, mdf$Collision, pch = "|", col = "blue")
points(mdm$Sizecm, mdm$Collision, pch = "|", col = "green")
range(mortDat$Sizecm)
xv <- mdm$Sizecm
xv
xv1 <- mdf$Sizecm
xv1
xv2 <- c(rep("M", length(xv)), rep("F", length(xv1)))
xv2
look <- data.frame(Sizecm= c(xv, xv1), Sex= xv2)
look
str(look)
str(mortDat)
look$fit <- predict(m3, newdata = look, type = "response")
str(look)

points(look$Sizecm[look$Sex == "M"],
      look$fit[look$Sex == "M"],
      col = "green")
points(look$Sizecm[look$Sex == "F"], 
      look$fit[look$Sex == "F"],
      col = "blue")
text(110,.2, "M", col = "green")
text(110,.75, "F", col = "blue")


#### VGM Style Analysis ####

#edit fail

plot(mortDat$Sizecm, mortDat$Collision, pch = "|", xlab = "Manatee size, cm",
     ylab = "Collision")
points(mdf$Sizecm, mdf$Collision, pch = "|", col = "blue")
points(mdm$Sizecm, mdm$Collision, pch = "|", col = "green")
range(mortDat$Sizecm)
xv <- range(mortDat$Sizecm)[1]:range(mortDat$Sizecm)[2]
xv
xv2 <- c(rep("M", length(mdm$Sex)), rep("F", length(mdf$Sex)))
xv2
look <- data.frame(Sizecm= c(mdm$Sizecm, mdf$Sizecm), Sex= xv2)
look
str(look)
str(mortDat)
look$fit <- predict(m3, newdata = look, type = "response")
str(look)
lines(look$Sizecm[look$Sex == "M"],
      look$fit[look$Sex == "M"],
      col = "green")
lines(look$Sizecm[look$Sex == "F"], 
      look$fit[look$Sex == "F"],
      col = "blue")
text(110,.2, "M", col = "green")
text(110,.75, "F", col = "blue")



library(vegan)
library(MASS) 

library("VGAM")
ps.options(pointsize = 12)
options(width = 72, digits = 4)
options(SweaveHooks = list(fig = function() par(las = 1)))
options(prompt = "R> ", continue = "+")

####CEMENTING####

library(MuMIn)
#Model Selection

mortD <- mortDat[!(mortDat["Sex"] == "U"),]

options(na.action = "na.fail")
subset <- mortD[c("Season","Sizecm","County", "Region", "Sex", "Waterway", "Collision")]
subset <- mortD[c("Sizecm","Region", "Sex", "Season","Collision")]
fm1 <- glm(Collision ~ Sizecm*Region*Sex*Season, data = subset, family = binomial, na.action = na.fail)
dd <- dredge(fm1)

globmod <- glm(Collision ~ Sizecm*Region*Sex*Season, data = subset, family = binomial, na.action = na.fail)

varying.link <- list(family = alist(
  logit = binomial("logit"),
  probit = binomial("probit"),
  cloglog = binomial("cloglog")
))

(ms12 <- dredge(globmod, varying = varying.link,
                rank = AIC))

#STEP AIC Method#

model <- glm(Collision ~ Sizecm*Region*Sex*Season, data = mortD, family = binomial, na.action = na.fail)
summary(model)
# AIC: 8906.3
model2 <- step(model)
summary(model2)

#resulting model
orig <- glm(formula = Collision ~ Sizecm + Region + Sex + Season + Sizecm:Sex + 
      Region:Sex + Sizecm:Season + Region:Season + Sex:Season + 
      Sizecm:Sex:Season, family = binomial, data = mortD, na.action = na.fail)

orig2 <- glm(formula = Collision ~ Sizecm + Region + Sex + Season + Sizecm:Sex + 
      Region:Sex + Sizecm:Season + Region:Season + 
      Sizecm:Sex:Season, family = binomial, data = mortD, na.action = na.fail)
anova(orig,orig2,test="Chi") #Put this in the powerpointP = 0.1689
#There is no persuasvie evidence of a Sex:Season Term

orig3 <- glm(formula = Collision ~ Sizecm + Region + Sex + Season + Sizecm:Sex + 
               Region:Sex + Sizecm:Season + Region:Season, family = binomial, data = mortD, na.action = na.fail)

anova(orig2,orig3,test="Chi") # Show in slides. Some evidence of an importance with this term. #However we do get a reduction in AIC 8887.3 - 8877.8 versus 

#Decided that the best model was the one with those reductions to the stepAIC technique. Matching the model that came from the 
#Dredging technique. 
model3 <- update(model2, formula=drop.terms(model2$terms, c("Region:Sex:Season"), keep.response=TRUE)  )
