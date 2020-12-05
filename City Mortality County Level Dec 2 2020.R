

remove(list=ls())

#setwd("C:\\Users\\Todd MacKenzie\\Dropbox\\Mortality vs City Taxes and Expenditures")
#setwd("C:\\Users\\Todd A. MacKenzie\\Dropbox (Dartmouth College)\\Mortality vs City Taxes and Expenditures")

###############################################################################
# Read in City/County Level Mortality Analytic Data Set Prepared in Other Code
##############################################################################

#CountyLevel <- read.delim("Data\\County Level Mortality vs Expenditures Data.txt")
CountyLevel <- read.delim("..\\Data\\County Level Mortality vs Expenditures Data.txt")
attach(CountyLevel)

CountyLevel$State.Education <- CountyLevel$State.EducationPrimSec + CountyLevel$State.EducationPostSec
CountyLevel$State.SocialServices <- CountyLevel$State.Welfare + CountyLevel$State.HealthHospitals

names(CountyLevel)[names(CountyLevel) == "spending_direct" ] <- "Total.City.Spending"

N <- dim(CountyLevel)[1]

# Single City County
CountyLevel$SingleCity <- 1 == nchar(xx <- as.character(CountyLevel$City.Names)) - nchar(gsub(":", "", xx))

##############################################
# Prepare list of City and County Names
##############################################
City.Names.2 <- rep(NA, N)
for (i in 1:dim(CountyLevel)[1]) {
  pattern <- paste(CountyLevel$State.Abbreviation[i], ":", sep="")
  City.Names.2[i] <- gsub(pattern=pattern, replacement="", x=CountyLevel$City.Names[i], ignore.case=FALSE)
}  

Names <- data.frame(State=CountyLevel$State.Abbreviation, City.Names=City.Names.2, County.Name)
write.table(Names, file="..\\Results\\Table of City Names.txt", sep="\t")

########################################
# Visualization of City-County Mortality
########################################
par(mfrow=c(3,1), mar=c(3.0, 3.0, 1.5, 0.1))
Y <- CountyLevel$OoERMort
ord <- order(-CountyLevel$OoERMort)
plot(1:dim(CountyLevel)[1], Y[ord], pch=16, ylab="", cex.main=0.8, main="Mortality Ratios by City\n Adjusted for Age, Sex, Race and Hispanic Status", axes=FALSE)
abline(h=1, lty=3)
axis(2)
axis(1, at=c(1, dim(CountyLevel)[1]), labels=c("",""))
for (i in 1:dim(CountyLevel)[1])
  lines(rep(i,2), Y[ord][i] + OoERMort.SE[ord][i]*c(-2,+2))


##########################
# Visualization of Exposures
##########################
#png("..//Results//City Spending.png", res=300, width=600,height=400)
#par(mfrow=c(1,1), mar=c(4, 4.0, 1.8, 0.1))
# Visualize City Expenditures
Major.Spending <- CountyLevel[, c("education", "public_safety", "envir_housing", "social_services", "transportation")]
Other.Spending <- CountyLevel$Total.City.Spending - rowSums(Major.Spending)
Major.Spending <- cbind(Major.Spending, Other.Spending)
ord <- order(-CountyLevel$Total.City.Spending)
row.names(Major.Spending) <- St.FIPS
n.city <- dim(CountyLevel)[1] # 100
cols <- rev(c(1,8,4,3,2,5))
barplot(as.matrix(t(Major.Spending[ord,][1:n.city,]))/1000, col=cols, space=0, las=2, cex.main=0.8, main="Spending by Municipal Governments (1000s of $)", cex.main=3, cex.names=0.1, cex.main=1, xlab="City-County")
legend(0.80*n.city, 14, text.col=rev(cols), bty="n", cex=0.8, y.intersp=0.5, legend=rev(c("Education", "Public Safety", "Env & Housing", "Social Services", "Transport", "Other")))
#points(1:n.city - 0.5, State.Total[ord], pch=16, cex=1 , col="orange")
#dev.off()

# Visualize State Expenditures
State.Major.Spending <- CountyLevel[, c("State.Education", "State.SocialServices", "State.Highways", "State.Police")]
State.Other.Spending <- CountyLevel$State.Total - rowSums(State.Major.Spending)
State.Major.Spending <- cbind(State.Major.Spending, State.Other.Spending)
Keep <- tapply(1:nrow(CountyLevel), CountyLevel$State.Abbreviation, min)
k.State.Major.Spending <- State.Major.Spending[Keep, ]
row.names(k.State.Major.Spending) <- as.character(CountyLevel$State.Abbreviation[Keep])
ord <- order(- CountyLevel$State.Total[Keep])
cols <- rev(c(1,2,3,4,5))
#png("..//Results//State Spending.png", res=10000, width=600,height=400)
#par(mfrow=c(1,1), mar=c(1, 1.0, 1.8, 0.1))
barplot(as.matrix(t(k.State.Major.Spending[ord,]))/1000, col=cols, space=0, las=2, ylab="$", cex.main=0.8, main="Spending by State (1000s of $)", cex.names=0.2, xlab="State")
legend(0.80*nrow(k.State.Major.Spending), 14, text.col=rev(cols), bty="n", cex=0.8, y.intersp=0.5, legend=rev(substr(names(k.State.Major.Spending), start=7, stop=100)))
#dev.off()

# Cities comprise about 60% of county population
# For 79 of the 140 counties, cities comprise over 50% of population
CountyLevel$Pop.Fraction <- CountyLevel$Cities.Population / (CountyLevel$County.Population / 1000)
summary(CountyLevel$Pop.Fraction)

# Table 1
Cov1000 <- data.frame(Total.Municipal.Spending = CountyLevel$Total.City.Spending, Major.Spending, State.Total, State.Major.Spending, PCI2005, CountyLevel$County.Population)/1000 
Cov100 <- 100*CountyLevel[, c("Race.2.County", "Race.3.County", "Race.4.County", "Hispanic.County")]
names(Cov100) <- c("Black", "Native Amer", "Asian", "Hispanic")
CovPerc <- data.frame(CountyLevel[, c("Sub.HS.2000", "Some.College.2000", "College.2000")], Cov100 ) 
Cov <- data.frame(Cov1000, CovPerc)

Tbl1 <- round(t(apply(Cov, 2, quantile, na.rm=TRUE)), 1)
write.table(Tbl1, "../Results/Table.txt")

# Descriptives 
sort(table(CountyLevel$State.Abbreviation))

fig <- function(x.name, title=x.name, log="xy") {
  x <- pmax(CountyLevel[, x.name], 1)
#  shade <- gray(2:0 / 3)[as.numeric(cut(CountyLevel$Pop.Fraction, c(0,50,75,101)))]
  shade <- 1
  plot(x, CountyLevel$OoERMort, cex=1/(300*CountyLevel$OoERMort.SE), xlab="Spending $", main=title, log=log, axes=FALSE, ylab="Excess Adjusted Mortality (%)", pch=16, col=shade)
  axis(1)
  axis(2, c(0.8,0.9,1.0,1.1,1.2,1.3), labels=c(-20,-10,0,10,20,30))
  rug(CountyLevel[, x.name])
  kp <- CountyLevel$County.Population >= 0.5*10^6
  #text(CountyLevel[kp, x.name], CountyLevel$OoERMort[kp], substr(CountyLevel$City.Names[kp], 1,2), cex=0.60)
}

CountyLevel[is.element(CountyLevel$St.FIPS, names(CountyLevel$OoERMort)[CountyLevel$OoERMort<0.85]), c("County.Name", "City.Names")]
CountyLevel[is.element(CountyLevel$St.FIPS, names(CountyLevel$OoERMort)[CountyLevel$OoERMort>1.25]), c("County.Name", "City.Names")]


# Figure
png("../Results/Scatter.png")
par(mfcol=c(2,3))
par(mar=c(1.5,4,4,1))
fig("Total.City.Spending", "City Total")
par(mar=c(4.1,4,4,1))
fig("State.Total", "State Total")
par(mar=c(1.5,2,4,1))
fig("envir_housing", "City Envir. & Housing")
par(mar=c(4.1,2,4,1))
fig("social_services", "City Social Services")
par(mar=c(1.5,2,4,1))
fig("education", "City Education")
par(mar=c(4.1,2,4,1))
fig("public_safety", "City Public Safety")
dev.off()

################################################
# Weighted Linear Models: Univariable and Multivariable
################################################

log.rate <- log(CountyLevel$OoERMort)
Wt <- CountyLevel$County.Population

# Effect of covariates
summary(lm(log.rate ~ log(PCI2005), weight=Wt, data=CountyLevel))$coef
summary(lm(log.rate ~ Sub.HS.2000 + Some.College.2000 + College.2000, weight=Wt, data=CountyLevel))$coef
summary(lm(log.rate ~ Race.2.County + Race.3.County + Race.4.County + Hispanic.County, weight=Wt, data=CountyLevel))$coef

# 62% of variation explained !
summary(lm(log.rate ~ log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000 + Race.2.County + Race.3.County + Race.4.County + Hispanic.County, weight=Wt, data=CountyLevel))

# 72% of variation explained - but too many df
summary(lm(log.rate ~ (log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000 + Race.2.County + Race.3.County + Race.4.County + Hispanic.County)^2, weight=Wt, data=CountyLevel))

# Role of State Expenditures
summary(lm(log.rate ~ log(State.Total) + (log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000 + Race.2.County + Race.3.County + Race.4.County + Hispanic.County + Age.County)^2, weight=Wt, data=CountyLevel))$coef[2,]

# Total City and Total State
summary(lm(log.rate ~ log(Total.City.Spending) + log(State.Total) + (log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000 + Race.2.County + Race.3.County + Race.4.County + Hispanic.County + Age.County)^2, weight=Wt, data=CountyLevel))$coef[2:3,]

library(lme4)
(os <- summary(lmer(log.rate ~ (1 | State.Abbreviation) + log(State.Total) + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000  + Race.2.County + Race.3.County + Race.4.County + Hispanic.County + Age.County, weight=Wt, data=CountyLevel)))

(os <- summary(lmer(log.rate ~ (1 | State.Abbreviation) + log(Total.City.Spending) + log(State.Total) + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000  + Race.2.County + Race.3.County + Race.4.County + Hispanic.County + Age.County, weight=Wt, data=CountyLevel)))


# Exposures of Primary Interest
rev.expend.subset  <- c("Total.City.Spending", "education", "social_services", "transportation", "public_safety", "envir_housing", "State.Total", "State.Education", "State.SocialServices", "State.Highways", "State.Police")

Estimate.Associations <- function(log.rate, Wt) {
  # Unadjusted effecs of city spending categories
  Unadj <- NULL
  for (x.name in rev.expend.subset) {
    log.x <- log(ifelse(CountyLevel[, x.name]==0,1,CountyLevel[,x.name]))/log(1.1)
    os <- summary(lm(log.rate ~ log.x, weight=Wt, data=CountyLevel))$coef[2,]
    # CI <- os[1:2] %*% matrix(nrow=2, ncol=3, c(1,0,1,-2,1,+2))
    # cat(round(CI ,3), round(o[4],4), "\n")
    NewRow <- c(os[1:2], ps=os[4])
    Unadj <- rbind(Unadj, NewRow)
  }
  row.names(Unadj) <- rev.expend.subset

  # Adjusted for County level demographics including per capita income and education
  Adj <- NULL
  for (x.name in rev.expend.subset) {
    log.x <- log(ifelse(CountyLevel[, x.name] == 0, 1, CountyLevel[, x.name])) / log(1.1)
    os <- summary(lm(log.rate ~ log.x + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000 + Race.2.County + Race.3.County + Race.4.County + Hispanic.County + Age.County, weight=Wt, data=CountyLevel))
    # CI <- os$coef[2,1:2] %*% matrix(nrow=2, ncol=3, c(1,0,1,-2,1,+2))
    # cat(round(CI ,3), round(os[4],4), "\n")
    NewRow <- c(os$coef[2,1:2], ps=os$coef[2,4])
    Adj <- rbind(Adj, NewRow)  
  }
  row.names(Adj) <- rev.expend.subset

  # Adjusted for State Expenditures (per capita) as well as county level demographics
  StAdj <- NULL
  for (x.name in rev.expend.subset) {
    log.x <- log(ifelse(CountyLevel[, x.name] == 0, 1, CountyLevel[, x.name])) / log(1.1)
    os <- summary(lm(log.rate ~ log.x + log(State.Total) + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000  + Race.2.County + Race.3.County + Race.4.County + Hispanic.County + Age.County, weight=Wt, data=CountyLevel))
    #CI <- os$coef[2,1:2] %*% matrix(nrow=2, ncol=3, c(1,0,1,-2,1,+2))
    NewRow <- c(os$coef[2,1:2], ps=os$coef[2,4])
    StAdj <- rbind(StAdj, NewRow)  
  }
  row.names(StAdj) <- rev.expend.subset

  # State RE added to all of above
  StREAdj <- NULL
  for (x.name in rev.expend.subset) {
    log.x <- log(ifelse(CountyLevel[, x.name] == 0, 1, CountyLevel[, x.name])) / log(1.1)
    os <- summary(lmer(log.rate ~ (1 | State.Abbreviation) + log.x + log(State.Total) + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000  + Race.2.County + Race.3.County + Race.4.County + Hispanic.County + Age.County, weight=Wt, data=CountyLevel))
    #CI <- os$coef[2,1:2] %*% matrix(nrow=2, ncol=3, c(1,0,1,-2,1,+2))
    p <- 1 - pchisq(os$coef[2,3]^2, df=1)
    NewRow <- c(os$coef[2,1:2], ps=p)
    StREAdj <- rbind(StREAdj, NewRow)  
  }
  row.names(StREAdj) <- rev.expend.subset
  StREAdj
  
  (Combined <- cbind(Unadj, Adj, StAdj, StREAdj))
}

# the dependent variable - in subgroup analyses will also consider  OoERFemale, OoERMale, OoERAge.20
log.rate <- log(CountyLevel$OoERMort)
Wt <- CountyLevel$County.Population

(MainTable <- Estimate.Associations(log.rate, Wt))
write.table(MainTable, "../Results/MainTable.txt")

####################################################
# Excluding Smaller Cities
####################################################

Keep <- CountyLevel$County.Population > 100 * 1000 * 10 #  100,000
(os <- summary(lm(log.rate ~ log(Total.City.Spending) + log(State.Total) + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000  + Race.2.County + Race.3.County + Race.4.County + Hispanic.County + Age.County, weight=Wt, subset=Keep, data=CountyLevel)))$coef[2:3,]
(os <- summary(lmer(log.rate ~ (1 | State.Abbreviation) + log(Total.City.Spending) + log(State.Total) + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000  + Race.2.County + Race.3.County + Race.4.County + Hispanic.County + Age.County, weight=Wt, subset=Keep, data=CountyLevel)))$coef[2:3,]

Keep <- CountyLevel$County.Population > 200 * 1000 * 10 #  200,000
(os <- summary(lm(log.rate ~ log(Total.City.Spending) + log(State.Total) + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000  + Race.2.County + Race.3.County + Race.4.County + Hispanic.County + Age.County, weight=Wt, subset=Keep, data=CountyLevel)))$coef[2:3,]
(os <- summary(lmer(log.rate ~ (1 | State.Abbreviation) + log(Total.City.Spending) + log(State.Total) + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000  + Race.2.County + Race.3.County + Race.4.County + Hispanic.County + Age.County, weight=Wt, subset=Keep, data=CountyLevel)))$coef[2:3,]

####################################################
# Excluding City-Counties with > 1 City
####################################################

Keep <- CountyLevel$SingleCity
(os <- summary(lm(log.rate ~ log(Total.City.Spending) + log(State.Total) + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000  + Race.2.County + Race.3.County + Race.4.County + Hispanic.County + Age.County, weight=Wt, subset=Keep, data=CountyLevel)))$coef[2:3,]
(os <- summary(lmer(log.rate ~ (1 | State.Abbreviation) + log(Total.City.Spending) + log(State.Total) + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000  + Race.2.County + Race.3.County + Race.4.County + Hispanic.County + Age.County, weight=Wt, subset=Keep, data=CountyLevel)))$coef[2:3,]


############################################
# Subgroup Analyses
############################################

var.nm <- c("Total.City.Spending", "State.Total")
full.adj <- 10:12
Subgroup.Tbl <- NULL
OoER.Names <- c("OoERMort", "OoERFemale", "OoERMale", "OoERAge.20", "OoERAge20_39", "OoERAge40_64", "OoERAge65plus", "OoERWhite", "OoERBlack", "OoERAsian", "OoERNativeAmer", "OoERHispanic", "OoERNotHispanic")
Wt.Names <- c("County.Population", "N.Female", "N.Male", "N.Age.20", "N.Age20_39", "N.Age40_64", "N.Age65plus", "N.White", "N.Black", "N.Asian", "N.NativeAmer", "N.Hispanic", "N.NotHispanic")
for (i in 1:length(OoER.Names)) {
  y <- log(CountyLevel[, OoER.Names[i]])
  wt <- CountyLevel[, Wt.Names[i]]
  o <- Estimate.Associations(y, wt)[var.nm, full.adj]
  Subgroup.Tbl <- rbind(Subgroup.Tbl, c(o[1,], o[2,]))
}
colnames(Subgroup.Tbl)[c(1,4)] <- c("City Est", "State Est")
rownames(Subgroup.Tbl) <- substr(OoER.Names, 5,20)
write.table(Subgroup.Tbl, "../Results/SubgroupTable.txt")

library(forestplot)
png("..\\Results\\Subgroup Municipal.png")
arg <- exp(Subgroup.Tbl[,1:2] %*% matrix(nrow=2, ncol=3, c(1,0,1,-2,1,+2)))
arg <- 100 * (arg - 1)
names <- c("All", "Female", "Male", "Age < 20", "Age 20-39", "Age 40-64", "Age 65+", "White", "Black", "Asian", "Native Am / Pac Is", "Hispanic", " Not Hispanic")
labeltext = list(names, paste(sprintf("%.1f", arg[,1]), " (", sprintf("%.1f", arg[,2]), ", ", sprintf("%.1f", arg[,3]), ")", sep=""))
txt_gp = fpTxtGp(label=list(gpar(cex=0.65)))
forestplot(labeltext, arg, txt_gp=txt_gp, cex=1, boxsize=0.1, xlab="Mortality Change %")
dev.off()

png("..\\Results\\Subgroup State.png")
arg <- exp(Subgroup.Tbl[,4:5] %*% matrix(nrow=2, ncol=3, c(1,0,1,-2,1,+2)))
arg <- 100 * (arg - 1)
labeltext = list(paste(sprintf("%.1f", arg[,1]), " (", sprintf("%.1f", arg[,2]), ", ", sprintf("%.1f", arg[,3]), ")", sep=""))
txt_gp = fpTxtGp(label=list(gpar(cex=0.65)))
forestplot(labeltext, arg, txt_gp=txt_gp, cex=1, boxsize=0.1, xlab="Mortality Change %")
dev.off()

# Police spending by Race
summary(lmer(log(OoERBlack) ~ (1 | State.Abbreviation) + log(public_safety) + log(State.Total) + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000  + Race.2.County + Race.3.County + Race.4.County + Hispanic.County + Age.County, weight=Wt, data=CountyLevel))$coef
summary(lm(log(OoERBlack) ~ log(public_safety) + log(State.Total) + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000  + Race.2.County + Race.3.County + Race.4.County + Hispanic.County + Age.County, weight=Wt, data=CountyLevel))$coef
summary(lm(log(OoERBlack) ~ log(public_safety) + log(State.Total) + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000, weight=Wt, data=CountyLevel))$coef

summary(lmer(log(OoERWhite) ~ (1 | State.Abbreviation) + log(public_safety) + log(State.Total) + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000  + Race.2.County + Race.3.County + Race.4.County + Hispanic.County + Age.County, weight=Wt, data=CountyLevel))$coef
summary(lm(log(OoERWhite) ~ log(public_safety) + log(State.Total) + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000  + Race.2.County + Race.3.County + Race.4.County + Hispanic.County + Age.County, weight=Wt, data=CountyLevel))$coef
summary(lm(log(OoERWhite) ~ log(public_safety) + log(State.Total) + log(PCI2005) + Sub.HS.2000 + Some.College.2000 + College.2000, weight=Wt, data=CountyLevel))$coef






##########################################
# Basement
##########################################

# Visualize Revenue
Major.Revenue <- CountyLevel[, c("tax_property", "tax_income_indiv",  "tax_income_corp")]
External.Revenue <- CountyLevel$rev_general - CountyLevel$own_source_rev
Major.Revenue <- cbind(Major.Revenue, External.Revenue)
Other.Revenue <- CountyLevel$rev_general - rowSums(Major.Revenue)
Major.Revenue <- cbind(Major.Revenue, Other.Revenue)
ord <- order(-CountyLevel$rev_general)
row.names(Major.Revenue) <- St.FIPS
n.city <- dim(CountyLevel)[1] # 50
cols <- c(1,2,4,3,5)
barplot(as.matrix(t(Major.Revenue[ord,][1:n.city,])), col=cols, space=0.0, las=2, ylab="$", main="Revenue", cex.main=1, cex.names=0.5)
legend(0.85*n.city, 10500, text.col=rev(cols), bty="n", cex=0.8, y.intersp=0.7, legend=rev(c("Income Tax", "Corporate Tax", "Property Tax", "Transfers", "Other Revenue")))

Estimate.Associations(log(OoERFemale), Wt)[var.nm, full.adj]
Estimate.Associations(log(OoERMale), Wt)
Estimate.Associations(log(OoERAge.20), Wt)
Estimate.Associations(log(OoERAge20_39), Wt)
Estimate.Associations(log(OoERAge40_64), Wt)          
Estimate.Associations(log(OoERAge65plus), Wt)
Estimate.Associations(log(OoERAsian), Wt)             
Estimate.Associations(log(OoERBlack), Wt)
Estimate.Associations(log(OoERNativeAmer), Wt)     
