#Saina Shibili
#RS3 Final Project 

#LOAD IN clean_mergeData.Rda
load("data/mergeData.Rda")

#------------------------------------------------------
#LOAD IN DATA SETS - only if clean_mergeData not saved
#required libraries
library(readr)
#last updated GDP data--> 05/10/2023
GDPdata <- read_csv("data/GDPdata.csv")
AverageIncomedata <- read_csv("data/AverageIncomedata.csv")
COVID19data <- read_csv("data/COVID19data.csv")

#CLEAN UP GDP DATA SET
clean_GDPdata <- GDPdata[-c(1:4),c(1,2,5)]
#view colnames + change them to more relevant colnames
colnames(GDPdata)
colnames(clean_GDPdata) <- c("CountryName", "CountryCode", "2021GDP")
#alphabetize
clean_GDPdata <- clean_GDPdata[order(clean_GDPdata$CountryName),]

#CLEAN UP AVGINCOME DATA SET
clean_avgIncdata <- AverageIncomedata[-c(1:4),c(1:3)]
colnames(AverageIncomedata)
colnames(clean_avgIncdata) <- c("CountryName","CountryCode","2021AvgIncome")

#JOIN GDP AND AVG INCOME
clean_financedata <- merge(clean_avgIncdata, clean_GDPdata)

#CLEAN UP COVID DATA SET
clean_COVID19data <- COVID19data[-1,-12]
#view colnames + change them to more relevant colnames
colnames(COVID19data)
colnames(clean_COVID19data) <- c("CountryName", "Region", "CasesCum", "CasesCumRate", "NewCases", "NewCasesRate", "DeathCum", "DeathCumRate", "NewDeaths", "NewDeathsRate", "NewDeaths24H")
#alphabetize
clean_COVID19data <- clean_COVID19data[order(clean_COVID19data$CountryName),]

#UNIVERSALIZING COUNTRY NAMES - i.e. removing "the", etc
clean_financedata[17,1] <- "Bahamas"
clean_COVID19data[25,1] <- "Bolivia"
clean_financedata[50,1] <- "Democratic Republic of the Congo"
clean_financedata[51,1] <- "Congo"
clean_financedata[126,1] <- "Democratic People's Republic of Korea"
clean_financedata[68,1] <- "Egypt"
clean_financedata[87,1] <- "Gambia"
clean_financedata[114,1] <- "Iran (Islamic Republic of)"
clean_financedata[127,1] <- "Republic of Korea"
clean_COVID19data[115,1] <- "Lao PDR"
clean_financedata[159,1] <- "Micronesia (Federated States of)"
clean_COVID19data[155,1] <- "Northern Mariana Islands"
clean_financedata[164,1] <- "Republic of Moldova"
clean_financedata[214,1] <- "Sint Maarten"
clean_financedata[215,1] <- "Slovakia"
clean_financedata[246,1] <- "Türkiye"
clean_COVID19data[212,1] <- "United Kingdom"
clean_COVID19data[226,1] <- "Tanzania"
clean_COVID19data[227,1] <- "United States"
clean_financedata[259,1] <- "Venezuela (Bolivarian Republic of)"
clean_COVID19data[233,1] <- "Vietnam"
clean_financedata[261,1] <- "United States Virgin Islands"
clean_financedata[264,1] <- "Yemen"

#JOIN DATAFRAMES
library(dplyr)
mergeData <- merge(clean_financedata, clean_COVID19data, by="CountryName")
mergeData <- relocate(.data=mergeData, "Region", .after="CountryCode")
colnames(mergeData)[c(4,5)] <- c("AvgIncome", "GDP")

#SAVE CLEAN MERGED DATA
save(mergeData, file='data/mergeData.Rda')
#-----------------------------------------------------
library(car)
library(ggplot2)
library(pastecs)
#SUMMARY STATISTICS + PLOTS
#GDP
boxplot(mergeData$GDP, xlab="GDP", horizontal=TRUE)
stat.desc(mergeData$GDP)
#AvgIncome
boxplot(mergeData$AvgIncome, xlab="Average Income", horizontal=TRUE)
stat.desc(mergeData$AvgIncome)
#Cases Rate
boxplot(mergeData$CasesCumRate, xlab="Case Rates", horizontal=TRUE)
stat.desc(mergeData$CasesCumRate)

#INITIAL PLOTS
initiallm <- lm(CasesCumRate~AvgIncome+GDP, data=mergeData)
summary(initiallm)
qqnorm(initiallm$residuals)
ggplot(data=mergeData, aes(x=GDP, y=CasesCumRate, color=AvgIncome)) +
  geom_point() + 
  theme_bw() +
  scale_color_gradient(low="lightpink",high="mediumpurple1")
avPlots(initiallm)
plot(initiallm$residuals, ylab="Residuals") +
  abline(0,0)

#PLOT GDP DATA TO IDENTIFY OUTLIERS
GDPboxplot <- boxplot(mergeData$GDP, plot=F)
GDPout_ind <- which(mergeData$GDP %in% c(GDPboxplot$out))
GDPout_mergeData <- mergeData[-GDPout_ind,]

#PLOT AVGINCOME DATA TO IDENTIFY OUTLIERS
AvgIncomeboxplot <- boxplot(mergeData$AvgIncome, plot=F)
AvgIncome_out <- which(mergeData$AvgIncome %in% c(AvgIncomeboxplot$out))
AvgIncomeout_mergeData <- mergeData[-AvgIncome_out,]

#MERGE OUTLIER DATA
out_mergeData <- merge(GDPout_mergeData, AvgIncomeout_mergeData)

#SUMMARY STATISTICS + PLOTS WITHOUT OUTLIERS
#GDP
boxplot(out_mergeData$GDP, xlab="GDP", horizontal=TRUE)
stat.desc(out_mergeData$GDP)
#AvgIncome
boxplot(out_mergeData$AvgIncome, xlab="Average Income", horizontal=TRUE)
stat.desc(out_mergeData$AvgIncome)

#PLOT WITHOUT OUTLIERS
library(devtools)
install_github("dgrtwo/broom")
library(broom)
outlm <- lm(CasesCumRate~AvgIncome+GDP, data=out_mergeData)
summary(outlm)
#export regression
write.csv(tidy(outlm), "outliers_lm.csv")

qqnorm(outlm$residuals)
ggplot(data=out_mergeData, aes(x=GDP, y=CasesCumRate, color=AvgIncome)) +
  geom_point() + 
  theme_bw() +
  scale_color_gradient(low="lightpink",high="mediumpurple1")
avPlots(outlm)
plot(outlm$residuals, ylab="Residuals") +
  abline(0,0)

#TRANSFORM DATA USING LOG
out_mergeData$logGDP = log(out_mergeData$GDP)
out_mergeData$logAvgIncome = log(out_mergeData$AvgIncome)

#summary statistics + plot for logGDP
boxplot(out_mergeData$logGDP, xlab="logGDP", horizontal=TRUE)
stat.desc(out_mergeData$logGDP)

out_mergeData$sqGDP = out_mergeData$GDP ** 2
out_mergeData$sqAvgIncome = out_mergeData$AvgIncome ** 2

log_outlm <- lm(CasesCumRate~logAvgIncome+logGDP, data=out_mergeData)
summary(log_outlm)
qqnorm(log_outlm$residuals)
ggplot(data=out_mergeData, aes(x=logGDP, y=CasesCumRate, color=logAvgIncome)) +
  geom_point() + 
  theme_bw() +
  scale_color_gradient(low="lightpink",high="mediumpurple1")
avPlots(log_outlm)
plot(log_outlm$residuals, ylab="Residuals") +
  abline(0,0)

logGDP_outlm <- lm(CasesCumRate~AvgIncome+logGDP, data=out_mergeData)
summary(logGDP_outlm)

#export regression
write.csv(tidy(logGDP_outlm), "logGDP_lm.csv")

qqnorm(logGDP_outlm$residuals)
ggplot(data=out_mergeData, aes(x=logGDP, y=CasesCumRate, color=AvgIncome)) +
  geom_point() + 
  theme_bw() +
  scale_color_gradient(low="lightpink",high="mediumpurple1")
avPlots(logGDP_outlm)
plot(logGDP_outlm$residuals, ylab="Residuals") +
  abline(0,0)

#RANDOMIZATION FOR LOGGDP
library(mosaic)
JustlogGDPlm = lm(CasesCumRate~logGDP, data=out_mergeData)
summary(JustlogGDPlm)
initialp_logGDP = 0.3798

set.seed(12345)
N=1000
logGDPcor=do(N)*cor(shuffle(logGDP)~CasesCumRate, data=out_mergeData, use="complete.obs")
hist(logGDPcor$cor, xlab="Correlation", main="Correlations of Case Rate vs. LogGDP")

highlogGDP = sum(logGDPcor$cor>=initialp_logGDP)
lowlogGDP = sum(logGDPcor$cor<=-initialp_logGDP)
finalp_logGDP = (highlogGDP+lowlogGDP)/N #shld be 0?

#RANDOMIZATION FOR AVGINCOME
JustAvgIncomelm = lm(CasesCumRate~AvgIncome, data=out_mergeData)
summary(JustAvgIncomelm)
#export regression
write.csv(tidy(JustAvgIncomelm), "AVGINCOME.csv")

initialp_AvgIncome = 2.2e-16

set.seed(54321)
avgIncomecor=do(N)*cor(shuffle(AvgIncome)~CasesCumRate, data=out_mergeData, use="complete.obs")
hist(avgIncomecor$cor, xlab="Correlation", main="Correlations of Case Rate vs. Average Income")

highAvgIncome = sum(avgIncomecor$cor>=initialp_AvgIncome)
lowAvgIncome = sum(avgIncomecor$cor<=-initialp_AvgIncome)
finalp_AvgIncome = (highAvgIncome+lowAvgIncome)/N

#AVERAGE INCOME ON CASE RATE
plot(out_mergeData$CasesCumRate~out_mergeData$AvgIncome, xlab="AvgIncome", ylab="Cumulative Case Rate")
abline(JustAvgIncomelm)
