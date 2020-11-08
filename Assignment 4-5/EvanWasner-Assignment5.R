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

