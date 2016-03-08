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

####Create Collision Boolean#####

mortDat["Collision"] <- ifelse(mortDat$Probable.Cause == "Human Related: Watercraft Collision", T, F)


library(MuMIn)
#Model Selection

mortD <- mortDat[!(mortDat["Sex"] == "U"),]

options(na.action = "na.fail")
subset <- mortD[c("Season","Sizecm","County", "Region", "Sex", "Waterway", "Collision")]
subset <- mortD[c("Sizecm","Region", "Sex", "Season","Collision")]
fm1 <- glm(Collision ~ Sizecm*Region*Sex*Season, data = subset, family = binomial, na.action = na.fail)
dd <- dredge(fm1)

#Selected Model
library(xtable)
final <- glm(Collision ~ Region + Season + Sex + Sizecm + Region:Season + Region:Sex + Season:Sex + Sex:Sizecm, data = mortD, family = binomial, na.action = na.fail)
xtable(final)

globmod <- glm(Collision ~ Sizecm*Region*Sex*Season, data = subset, family = binomial, na.action = na.fail)

varying.link <- list(family = alist(
  logit = binomial("logit"),
  probit = binomial("probit"),
  cloglog = binomial("cloglog")
))

(ms12 <- dredge(globmod, varying = varying.link,
                rank = AIC))
##Pretty Mumin Tables###

R2 <- function(x) summary(x)$r.squared
ms <- dd
ms[,1] <- round(ms[,1],2)
ms[,5] <- round(ms[,5],4)
ms[,18] <- round(ms[,18],2)
ms[,19] <- round(ms[,19],2)
ms[,20] <- round(ms[,20],2)
ms[,21]<- ifelse(c(1:length(ms[,21])) ==1, "<.0001", "<.0001")
i <- 1:10 # indices of columns with model terms
response <- "a"

res <- as.data.frame(ms)
v <- names(ms)[i]
v[v == "(Intercept)"] <- 1

# create formula-like model names:
mnames <- apply(res[, i], 1, function(x) 
  deparse(simplify.formula(reformulate(v[!is.na(x)], response = response))))
## OR
#   mnames <- apply(res[, i], 1, function(x)
#          sapply(attr(ms, "modelList"), function(x) deparse(formula(x)))

res <- cbind(model = mnames, res[, -i])
Hmisc::latex(res, file = "")





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
#AIC: 8878.7

orig2 <- glm(formula = Collision ~ Sizecm + Region + Sex + Season + Sizecm:Sex + 
               Region:Sex + Sizecm:Season + Region:Season + #Removed Sex:Season
               Sizecm:Sex:Season, family = binomial, data = mortD, na.action = na.fail)
#AIC: 8877.8

anova1 <- anova(orig,orig2,test="Chi") #Put this in the powerpointP = 0.1689
#There is no persuasvie evidence of a Sex:Season Term
colnames(anova1)[5] <- "P-value"
#anova2$Model <- as.character(2:1)
anova1 <- anova1[, c(6, 1:5)]
xtable(anova1)

orig3 <- glm(formula = Collision ~ Sizecm + Region + Sex + Season + Sizecm:Sex + 
               Region:Sex + Region:Season + #Removed Sizecm:Season
               Sizecm:Sex:Season, family = binomial, data = mortD, na.action = na.fail)
#AIC: 8877.8
anova2 <- anova(orig2,orig3,test="Chi") #Put this in the powerpointP = 0.1689
#There is no persuasvie evidence of a Sex:Season Term
colnames(anova2)[5] <- "P-value"
#anova2$Model <- as.character(2:1)
anova2 <- anova2[, c(6, 1:5)]
xtable(anova2)





orig4 <- glm(formula = Collision ~ Sizecm + Region + Sex + Season + Sizecm:Sex + 
               Region:Sex + Sizecm:Season + Region:Season, family = binomial, data = mortD, na.action = na.fail)
#AIC: 8887.3

anova2 <-anova(orig2,orig4,test="Chi") # Show in slides. Some evidence of an importance with this term. #However we do get a reduction in AIC 8887.3 - 8877.8 versus 
colnames(anova2)[5] <- "P-value"
#anova2$Model <- as.character(2:1)
anova2 <- anova2[, c(6, 1:5)]
xtable(anova2)
#Decided that the best model was the one with those reductions to the stepAIC technique. Matching the model that came from the 
#Dredging technique. 
model3 <- update(model2, formula=drop.terms(model2$terms, c("Region:Sex:Season"), keep.response=TRUE)  )


