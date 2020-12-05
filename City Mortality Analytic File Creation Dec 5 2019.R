
 make sure St FIPS working with OoERagg,, city.names


use only those cities that compose 75% or more of county population

maybe city is not good because tax is regressive

look at overdispersion (but identical to individual)

allow state effect?



remove(list=ls())

panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 1.5
  text(0.5, 0.5, txt, cex = cex.cor)
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19)
}



setwd("C:\\Users\\Todd MacKenzie\\Dropbox\\Mortality vs City Taxes and Expenditures")

######################################################
# Read in Mortality Data
######################################################

DeathCertif <- read.csv("Data\\US_Mortality_by_County.csv")
# Restrict to 200-2015
DeathCertif <- DeathCertif[DeathCertif$Year >= 2006 & DeathCertif$Year <= 2015, ]

State <- DeathCertif$Postal_Abbreviation
FIPS <- as.character(DeathCertif$FIPS_County)
DeathCertif$St.FIPS <- paste(State, c("00", "0", "")[nchar(FIPS)], FIPS, sep="")
 
# New York City consists of 5 counties
DeathCertif$St.FIPS[is.element(DeathCertif$St.FIPS,c("NY005","NY047","NY081","NY085"))] <- "NY061"

# Correct if number of deaths exceeds population
DeathCertif$Population <- pmax(DeathCertif$Number_Deaths, DeathCertif$Population)

# Add up all the demographic bins to get total county population (10 years worth)
County.Population <- tapply(DeathCertif$Population, DeathCertif$St.FIPS, sum, na.rm=TRUE)
#County.Population <- rowMeans(tapply(DeathCertif$Population, list(DeathCertif$St.FIPS, DeathCertif$Year), sum, na.rm=TRUE))

######################################################
# Contextual variables
######################################################
N.by.Race <- tapply(DeathCertif$Population, list(DeathCertif$St.FIPS, DeathCertif$Adjusted_Race), sum, na.rm=TRUE)
Race.2.County <- N.by.Race[,2] / County.Population
Race.3.County <- N.by.Race[,3] /County.Population
Race.4.County <- N.by.Race[,4] /County.Population

N.by.Hispanic <- tapply(DeathCertif$Population, list(DeathCertif$St.FIPS, DeathCertif$Binary_Hispanic), sum, na.rm=TRUE)
Hispanic.County <- N.by.Hispanic[,2] / County.Population

Age.County <-  (1/ County.Population) * as.vector(tapply(DeathCertif$Population, list(DeathCertif$St.FIPS, DeathCertif$Age), sum, na.rm=TRUE) %*% c(0.5,3, 2.5+(1:17)*5) )

AddVars.DF <- data.frame(St.FIPS=names(County.Population), County.Population, Race.2.County, Race.3.County, Race.4.County, Hispanic.County, Age.County)
DeathCertif <- merge(DeathCertif, AddVars.DF, by = "St.FIPS")

######################################################
# Read in Lincoln's FISC Data
######################################################

FISC <- read.delim("Data\\CityTaxExpendLincoln.txt")

# Reduce variable list from the almost 1000
# social_services is public welfare, health and hospitals
rev.expend.subset  <- c("rev_general", "own_source_rev", "tax_income_indiv", "tax_income_corp", "tax_property", "spending_general", "spending_direct", "education", "social_services", "public_welfare", "health", "hospitals", "transportation", "public_safety", "envir_housing")
FISC <- FISC[, c("year", "city_name", "city_population", rev.expend.subset)]

FISC.FIPS.Crosswalk <- read.delim("Data\\FISC Crosswalk City to FIPS.txt")
# Some cities and counties are synonomous
FISC.FIPS.Crosswalk$county_name <- ifelse(FISC.FIPS.Crosswalk$county_name=="", FISC.FIPS.Crosswalk$city_name, as.character(FISC.FIPS.Crosswalk$county_name))

FISC <- merge(FISC, FISC.FIPS.Crosswalk, by="city_name", all.x=TRUE)

State <- substr(FISC$city_name, 1, 2)
FIPS <- FISC$fips_county
FISC$St.FIPS <- paste(State, c("00", "0", "")[nchar(FIPS)], FIPS, sep="")

# See other file for analysis of variation between cities and over time

# Drop Washingon, DC because no state government 
FISC <- FISC[FISC$city_name != "DC: Washington", ]

# Sticking to one year for now
FISC <- FISC[FISC$year == 2007, ]

#########################################################
# Reduce to county as level of analysis
# i.e. aggregate city info to county level
# Use population weighted means of FISC measures
#########################################################
St.FIPS <- unique(FISC$St.FIPS)
City.Names <- unlist(tapply(FISC$city_name, FISC$St.FIPS, paste, collapse=","))
County.Name <- unique(paste(substr(FISC$St.FIPS,1,2), FISC$county_name))
Cities.Population <- tapply(FISC$city_population, FISC$St.FIPS, sum)
NewFISC <- data.frame(St.FIPS, County.Name, City.Names, Cities.Population)
Nms <- c("rev_general", "own_source_rev", "tax_income_indiv", "tax_income_corp", "tax_property", "spending_general", "spending_direct",
  "education", "social_services", "public_welfare", "health", "hospitals", "transportation", "public_safety", "envir_housing")
for (nm in Nms) {
  Var <- tapply(FISC[, nm]*FISC$city_population, FISC$St.FIPS, sum) / Cities.Population
  NewFISC <- cbind(NewFISC, Var)
}
names(NewFISC)[-(1:4)] <- Nms

FISC <- NewFISC

# Are the FISC cities represented in the death files?
# 141 Cities ?
table(is.element(unique(FISC$St.FIPS), unique(DeathCertif$St.FIPS)))
table(is.element(unique(DeathCertif$St.FIPS), unique(FISC$St.FIPS)))


# Descriptive statistics on City Expenditures
summary(FISC[, rev.expend.subset])
pairs(FISC[, c("rev_general", "own_source_rev", "education","public_welfare", "health", "hospitals", "transportation", "public_safety", "envir_housing")], lower.panel = panel.cor, upper.panel = upper.panel)


######################################################
# Creation of the Analytic File
######################################################
CityMortality <- merge(FISC, DeathCertif, by="St.FIPS", all.x=TRUE)
names(CityMortality)[names(CityMortality)=="Postal_Abbreviation"] <- "State.Abbreviation"

######################################################
# Read in County Income
######################################################
State.Codes <- read.delim("Data\\State Codes.txt")
State.Codes$Name <- as.character(State.Codes$Name)

# https://www.bea.gov/data/income-saving/personal-income-county-metro-and-other-areas
PCI <- read.delim("Data\\PCI_2000_2017.txt")
PCI$State.Abbreviation<- substr(toupper(PCI$GeoName), (x <- regexpr(", ", PCI$GeoName))+2,x+3)
#PCI$St.FIPS <- paste(PCI$StateAbbrev, substr(PCI$GeoFIPS,3,5), sep=".")
PCI$St.FIPS <- paste(PCI$State.Abbreviation, substr(PCI$GeoFIPS,5,7), sep="") # what no5 3rd to 5th???

pci.columns <- regexpr("X20", names(PCI))>0
names(PCI)[pci.columns] <- paste("PCI", substr(names(PCI),2,5), sep="")[pci.columns]

PCI <- PCI[, c("St.FIPS", "PCI2005")]

# Merge with Analytic File
CityMortality <- merge(CityMortality, PCI, by="St.FIPS", all.x=TRUE)

######################################################
# Read in County Education
######################################################
# https://www.ers.usda.gov/data-products/county-level-data-sets/
Education <- read.delim("Data\\Education by County Ready.txt")
nc <- nchar(Education$FIPS.Code)
FIPS.last3 <-substr(Education$FIPS.Code, nc-2, nc)
Education$St.FIPS <- paste(Education$State.Abbreviation, FIPS.last3, sep="")
Educ2000 <- Education[, c("St.FIPS", "Sub.HS.2000", "HS.2000", "Some.College.2000", "College.2000")]

# Merge with Analytic File
CityMortality <- merge(CityMortality, Educ2000, by="St.FIPS", all.x=TRUE)


######################################################
# Read in Expendituresy by State in 2005
# https://www.taxpolicycenter.org/statistics/state-and-local-general-expenditures-capita
######################################################
library(readxl)
StateExpenditures2005 <- read_excel("c:\\users\\todd mackenzie\\dropbox\\mortality versus taxes\\data\\State and Local Expenditures Per Capita 2005.xlsx")
StateExpenditures2005 <- merge(StateExpenditures2005, State.Codes[], by.x="State", by.y="Name", all.x=TRUE)
names(StateExpenditures2005)[-1] <- paste("State.", names(StateExpenditures2005)[-1], sep="")

# Merge with Analytic File
CityMortality <- merge(CityMortality, StateExpenditures2005, by="State.Abbreviation", all.x=TRUE)

######################################################
# Variable Editing
######################################################

fAge <- factor(CityMortality$Age)
fRace <- factor(CityMortality$Adjusted_Race)

Deaths.by.Bin <- tapply(CityMortality$Number_Deaths, list(CityMortality$Sex, fAge, fRace, CityMortality$Binary_Hispanic), sum)
data.frame(Deaths.by.Bin)
summary(Deaths.by.Bin)

################################################
# Adjusted Mortality
################################################

# As a simplicity we shall use total population (by county in person-years) as the weight
# AS opposed to different weights based on differen

Y.Binomial <- cbind(CityMortality$Number_Deaths, CityMortality$Population - CityMortality$Number_Deaths)

NullModel <- glm(Y.Binomial ~ (Sex+fAge+fRace+Binary_Hispanic)^3, data=CityMortality, family=binomial)
OoERagg <- tapply(CityMortality$Number_Deaths, CityMortality$St.FIPS, sum) / tapply(NullModel$fit * CityMortality$Population, CityMortality$St.FIPS, sum)
OoERagg.Sex <- tapply(CityMortality$Number_Deaths, list(CityMortality$St.FIPS, CityMortality$Sex), sum) / tapply(NullModel$fit * CityMortality$Population, list(CityMortality$St.FIPS, CityMortality$Sex), sum)
OoERagg.Race <- tapply(CityMortality$Number_Deaths, list(CityMortality$St.FIPS, CityMortality$Adjusted_Race), sum) / tapply(NullModel$fit * CityMortality$Population, list(CityMortality$St.FIPS, CityMortality$Adjusted_Race), sum)
Age.20.40.65 <- cut(CityMortality$Age, c(-1, 4.5, 8.5, 13.5, 99), labels=c("Age<20","Age20_39","Age40_64","Age65+"))
OoERagg.Age <- tapply(CityMortality$Number_Deaths, list(CityMortality$St.FIPS, Age.20.40.65 ), sum) / tapply(NullModel$fit * CityMortality$Population, list(CityMortality$St.FIPS, Age.20.40.65 ), sum)

Endpoints <- data.frame(OoERagg, OoERagg.Sex, OoERagg.Age, OoERagg.Race)


OoERagg.SE <- sqrt(tapply(CityMortality$Number_Deaths, CityMortality$St.FIPS, sum)) / tapply(NullModel$fit * CityMortality$Population, CityMortality$St.FIPS, sum)
#log.scale.SE <- OoERagg.SE / OoERagg 

Total.Population <- tapply(CityMortality$Population, CityMortality$St.FIPS, sum)
plot(Total.Population, 1/OoERagg.SE^2, log="xy")
cor.test(Total.Population, 1/OoERagg.SE^2)

summary(OoERagg)
quantile(OoERagg, c(0.01,0.99))
max(OoERagg)/min(OoERagg)


CountyLevel <- data.frame(St.FIPS=row.names(Endpoints), Endpoints, OoERagg.SE)
CountyLevel <- merge(CountyLevel, AddVars.DF, by="St.FIPS", all.x=TRUE)
CountyLevel <- merge(CountyLevel, FISC, by="St.FIPS")
CountyLevel <- merge(CountyLevel, PCI, by="St.FIPS", all.x=TRUE)
CountyLevel <- merge(CountyLevel, Educ2000, by="St.FIPS", all.x=TRUE)
CountyLevel$State.Abbreviation <- substr(CountyLevel$St.FIPS, 1, 2)
CountyLevel <- merge(CountyLevel, StateExpenditures2005, by="State.Abbreviation", all.x=TRUE)

write.table(CountyLevel[,-13], file="Data\\County Level Mortality vs Expenditures Data.txt", sep="\t", row.names=FALSE)

# Cities comprise about 60% of county population
# For 79 o fthe 140 counties, cities comprise over 50% of population
summary(CountyLevel$Pop.Fraction)
CountyLevel[order(CountyLevel$Pop.Fraction), c("City.Names","County.Population","Pop.Fraction")][1:20,]

plot(CountyLevel$Cities.Population, CountyLevel$Pop.Fraction, log="x")
abline(h=0.5,v=250000)

# Descriptives 
table(CountyLevel$State.Abbreviation)

fig <- function(x.name, log="xy") {
  plot(CountyLevel[, x.name], CountyLevel$OoERagg, cex=1/(300*CountyLevel$OoERagg.SE), xlab="$", main=x.name, log=log, axes=FALSE, ylab="Excess Adjusted Mortality (%)")
  axis(1)
  axis(2, c(0.8,0.9,1.0,1.1,1.2,1.3), labels=c(-20,-10,0,10,20,30))
  rug(CountyLevel[, x.name])
  kp <- CountyLevel$County.Population >= 0.5*10^6
  text(CountyLevel[kp, x.name], CountyLevel$OoERagg[kp], substr(CountyLevel$City.Names[kp], 1,2), cex=0.60)
}

fig("rev_general")
fig("own_source_rev")
fig("tax_property", log="y")
fig("spending_general")
fig("spending_direct")
fig("education", log="y")
fig("public_welfare")
fig("social_services")
fig("health")
fig("hospitals", log="y")
fig("transportation")
fig("public_safety")

fig("tax_income_indiv", log="y")
fig("tax_income_corp")


fig("PCI2005")
fig("College.2000")

# Elizabeth Bradley ratio
CountyLevel$ss.to.htl <- CountyLevel$social_services / (CountyLevel$health + CountyLevel$hospitals)
fig("ss.to.htl")

# Figure
par(mfcol=c(2,3))
fig("own_source_rev")
fig("tax_property")
fig("education", log="y")
fig("public_welfare", log="y")
fig("public_safety")
fig("envir_housing")

################################################
# Weighted Linear Models: Univariable and Multivariable
################################################
Wt <- 1/CountyLevel$OoERagg.SE^2
log.rate <- log(CountyLevel$OoERagg)

for (x.name in rev.expend.subset) {
  cat("\n", x.name,"\n")
  log.x <- log(ifelse(CountyLevel[, x.name]==0,1,CountyLevel[,x.name]))/log(1.01)
  print(round(summary(lm(log.rate ~ log.x, weight=Wt, data=CountyLevel))$coef[2,],3))
}

Wt <- 1/CountyLevel$OoERagg.SE^2
log.rate <- log(CountyLevel$OoERagg)


summary(lm(log.rate ~ log(PCI2005), weight=Wt, data=CountyLevel))$coef
summary(lm(log.rate ~ Sub.HS.2000 + Some.College.2000 + College.2000, weight=Wt, data=CountyLevel))$coef
summary(lm(log.rate ~ Race.2.County + Race.3.County + Race.4.County + Hispanic.County, weight=Wt, data=CountyLevel))$coef

# Model with county level demographics including per capita income and education
for (x.name in rev.expend.subset) {
  cat("\n", x.name,"\n")
  log.x <- log(ifelse(CountyLevel[, x.name] == 0, 1, CountyLevel[, x.name])) / log(1.1)
  print(round(summary(lm(log.rate ~ log.x + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000 + Race.2.County + Race.3.County + Race.4.County + Hispanic.County + Age.County, weight=Wt, data=CountyLevel))$coef[2,],4))
}

# Model with state total expenditures per capita
for (x.name in rev.expend.subset) {
  cat("\n", x.name,"\n")
  log.x <- log(ifelse(CountyLevel[, x.name] == 0, 1, CountyLevel[, x.name])) / log(1.1)
  print(round(summary(lm(log.rate ~ log.x + log(State.Total) + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000  + Race.2.County + Race.3.County + Race.4.County + Hispanic.County + Age.County, weight=Wt, data=CountyLevel))$coef[2:3,],4))
}

# Bradley test ? check if the
CountyLevel$social_minus_health <- pmax(CountyLevel$social_services - CountyLevel$health, 1)
summary(lm(log.rate ~ log(CountyLevel$social_minus_health) + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000  + Race.2.County + Race.3.County + Hispanic.County + Age.County, weight=Wt, data=CountyLevel))

# Multiple city factors in
summary(lm(log.rate ~ health + hospitals + public_welfare + education + public_safety + log(State.Total) + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000  + Race.2.County + Race.3.County + Race.4.County + Hispanic.County + Age.County, weight=Wt, data=CountyLevel))$coef[,]
# Should we control for state ?
summary(lm(log.rate ~ health + hospitals + public_welfare + education + public_safety + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000  + Race.2.County + Race.3.County + Race.4.County + Hispanic.County + Age.County, weight=Wt, data=CountyLevel))$coef[,]
library(lme4)
summary(lmer(log.rate ~ (1 | State.Abbreviation) + health + hospitals + public_welfare + education + public_safety + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000  + Race.2.County + Race.3.County + Race.4.County + Hispanic.County + Age.County, weight=Wt, data=CountyLevel))


# Note: it would be identical if we were testing log ratio of expenditure to PCI since LS would be the same

###########################################
# Pairing up City and State vars
###########################################
CountyLevel$HealthHospitals <- CountyLevel$health + CountyLevel$hospitals
City.Expend.Nms <- c("rev_general", "education", "public_welfare", "HealthHospitals", "transportation", "public_safety")
CountyLevel$State.Education <- CountyLevel$State.EducationPrimSec + CountyLevel$State.EducationPostSec
State.Expend.Nms <- c("State.Total", "State.Education", "State.Welfare", "State.HealthHospitals", "State.Highways", "State.Police")

for (i in 1:length(City.Expend.Nms)) {
  City.Exp <- pmax(1, CountyLevel[, City.Expend.Nms[i]])
  State.Exp <- pmax(1, CountyLevel[, State.Expend.Nms[i]])
  os <- summary(lm(log.rate ~ log(City.Exp) + log(State.Exp) + log(State.Total) + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000  + Race.2.County + Race.3.County + Hispanic.County + Age.County, weight=Wt, data=CountyLevel))
  cat(City.Expend.Nms[i],  State.Expend.Nms[i], "\n")
  print(os$coef[2:3,])
}


##########################################
# Variations
# Considered subsets by 
##########################################
include uncorrelated expenditures? subtract education ?

################################################

Y.Binomial <- cbind(CityMortality$Number_Deaths, CityMortality$Population - CityMortality$Number_Deaths)
summary(glm(Y.Binomial ~ rev_general + Sex*fAge*fRace*Binary_Hispanic, family=binomial, data = CityMortality))$coef[2,]

summary(o.glm <- glm(Y.Binomial ~ log(own_source_rev) + log(State.Total) + log(PCI2005) + log(Sub.HS.2000) + log(College.2000) + (Sex+fAge+fRace+Binary_Hispanic)^2, family=binomial, data = CityMortality))$coef[2:6,]

