## Evan Wasner
## Econ 753
## Assignment 5

## Set working directory
setwd("I:/Evan/Documents/Umass/Econ 753/EvanWasner_Econ753_ProblemSets/Assignment 4-5")

## Libraries
library(alfred)
library(tidyverse)
library(aTSA)
library(reshape2)
library(lmtest)  ## For Granger causality

## Clear workplace
rm(list = ls())
dev.off()

## Get data from FRED
BAA  <- get_fred_series("BAA", series_name="BAA")   
FEDFUNDS  <- get_fred_series("FEDFUNDS", series_name="FEDFUNDS") 

## Join data and arrange by date
rates <- arrange(full_join(x=FEDFUNDS, y=BAA), date)

## Omit na rows
rates <- na.omit(rates)

## Filter for observations after March 1963
rates <- filter(rates, date>="1963-03-01")

## Melt 
rates.melt <- melt(rates, 
                   id="date", 
                   measure.vars = c("FEDFUNDS", "BAA"), 
                   variable.name = "Measurement",
                   value.name = "Rate")
save(rates.melt, file="rates.melt.Rdata")

#################################
##                             ##
## Augmented Dicky-Fuller Test ##
##                             ##
#################################

## Run Augmented Dicky-Fuller Test on both BAA and FEDFUNDS
adfTest.BAA <- as.data.frame(adf.test(rates$BAA, output=FALSE))
adfTest.FEDFUNDS <- as.data.frame(adf.test(rates$FEDFUNDS, output=FALSE))

## Construct a table with results
adfTest.df <- cbind(adfTest.BAA[,1:3],
                    adfTest.BAA[,5:6],
                    adfTest.BAA[,8:9],
                    adfTest.FEDFUNDS[,2:3],
                    adfTest.FEDFUNDS[,5:6],
                    adfTest.FEDFUNDS[,8:9])
colnames(adfTest.df) <- c("Lag", "ADF", "p Value", "ADF", "p Value", "ADF", "p Value",
                          "ADF", "p Value", "ADF", "p Value", "ADF", "p Value")
save(adfTest.df, file="adfTest.df.Rdata")

## Difference Variables
drates <- data.frame(date=rates[2:692,1],
                     dBAA=diff(rates$BAA),
                     dFEDFUNDS=diff(rates$FEDFUNDS))
drates.melt <- melt(drates, 
                    id="date", 
                    measure.vars = c("dFEDFUNDS", "dBAA"), 
                    variable.name = "Measurement",
                    value.name = "Rate")
save(drates.melt, file="drates.melt.Rdata")

## Re-run ADF tests and construct table with results
adfTest.dBAA <- as.data.frame(adf.test(drates$dBAA, output=FALSE))
adfTest.dFEDFUNDS <- as.data.frame(adf.test(drates$dFEDFUNDS, output=FALSE))
adfTest.diff.df <- cbind(adfTest.dBAA[,1:3],
                         adfTest.dBAA[,5:6],
                         adfTest.dBAA[,8:9],
                         adfTest.dFEDFUNDS[,2:3],
                         adfTest.dFEDFUNDS[,5:6],
                         adfTest.dFEDFUNDS[,8:9])
colnames(adfTest.diff.df) <- c("Lag", "ADF", "p Value", "ADF", "p Value", "ADF", "p Value",
                          "ADF", "p Value", "ADF", "p Value", "ADF", "p Value")
save(adfTest.diff.df, file="adfTest.diff.df.Rdata")

############################
##                        ##
## Granger Causality Test ##
##                        ##
############################

## Should NOT Granger test these because they are not both stationary
grangertest(FEDFUNDS ~ BAA, order=4, na.action=na.omit, data=rates)
grangertest(BAA ~ FEDFUNDS, order=4, na.action=na.omit, data=rates)

## Granger causality test on first-differenced, stationary series
grangertest(dFEDFUNDS ~ dBAA, order=4, na.action=na.omit, data=drates)
grangertest(dBAA ~ dFEDFUNDS, order=4, na.action=na.omit, data=drates)
grangertest(dFEDFUNDS ~ dBAA, order=8, na.action=na.omit, data=drates)
grangertest(dBAA ~ dFEDFUNDS, order=8, na.action=na.omit, data=drates)
grangertest(dFEDFUNDS ~ dBAA, order=12, na.action=na.omit, data=drates)
grangertest(dBAA ~ dFEDFUNDS, order=12, na.action=na.omit, data=drates)


drates.cycle1 <- filter(drates, date>="1973-11-01" & date<="1981-06-01")
drates.cycle2 <- filter(drates, date>="2001-03-01" & date<="2008-02-01")
drates.recent <- filter(drates, date>"2008-02-01")

lagVector <- c(1,4,8,12)

## Make table for Granger Causality Test for all Dates
grangerTable.all <- data.frame()
for (lag in lagVector) {
  test <- grangertest(dFEDFUNDS ~ dBAA, order=lag, na.action=na.omit, data=drates)
  grangerTable.all <- rbind(grangerTable.all, c(lag, test[2,3], test[2,4]))
}
for (lag in lagVector) {
  test <- grangertest(dBAA ~ dFEDFUNDS, order=lag, na.action=na.omit, data=drates)
  grangerTable.all <- rbind(grangerTable.all, c(lag, test[2,3], test[2,4]))
}
colnames(grangerTable.all) <- c("Lag", "F-Stat", "p-Value")

## Make table for Granger Causality Test for cycle1
grangerTable.cycle1 <- data.frame()
for (lag in lagVector) {
  test <- grangertest(dFEDFUNDS ~ dBAA, order=lag, na.action=na.omit, data=drates.cycle1)
  grangerTable.cycle1 <- rbind(grangerTable.cycle1, c(test[2,3], test[2,4]))
}
for (lag in lagVector) {
  test <- grangertest(dBAA ~ dFEDFUNDS, order=lag, na.action=na.omit, data=drates.cycle1)
  grangerTable.cycle1 <- rbind(grangerTable.cycle1, c(test[2,3], test[2,4]))
}
colnames(grangerTable.cycle1) <- c("F-Stat", "p-Value")

## Make table for Granger Causality Test for cycle2
grangerTable.cycle2 <- data.frame()
for (lag in lagVector) {
  test <- grangertest(dFEDFUNDS ~ dBAA, order=lag, na.action=na.omit, data=drates.cycle2)
  grangerTable.cycle2 <- rbind(grangerTable.cycle2, c(test[2,3], test[2,4]))
}
for (lag in lagVector) {
  test <- grangertest(dBAA ~ dFEDFUNDS, order=lag, na.action=na.omit, data=drates.cycle2)
  grangerTable.cycle2 <- rbind(grangerTable.cycle2, c(test[2,3], test[2,4]))
}
colnames(grangerTable.cycle2) <- c("F-Stat", "p-Value")

## Make table for Granger Causality Test for recent dates
grangerTable.recent <- data.frame()
for (lag in lagVector) {
  test <- grangertest(dFEDFUNDS ~ dBAA, order=lag, na.action=na.omit, data=drates.recent)
  grangerTable.recent <- rbind(grangerTable.recent, c(test[2,3], test[2,4]))
}
for (lag in lagVector) {
  test <- grangertest(dBAA ~ dFEDFUNDS, order=lag, na.action=na.omit, data=drates.recent)
  grangerTable.recent <- rbind(grangerTable.recent, c(test[2,3], test[2,4]))
}
colnames(grangerTable.recent) <- c("F-Stat", "p-Value")

## Combine tables
grangerTable <- cbind(grangerTable.all,
                      grangerTable.cycle1,
                      grangerTable.cycle2,
                      grangerTable.recent)
save(grangerTable, file="grangerTable.Rdata")