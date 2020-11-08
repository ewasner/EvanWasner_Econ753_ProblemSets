## Evan Wasner
## Econ 753
## Assignments 4

## Set working directory
setwd("I:/Evan/Documents/Umass/Econ 753/EvanWasner_Econ753_ProblemSets/Assignment 4-5")

## Libraries
library(XLConnect)
library(tidyverse)
library(reshape2)

## Clear workplace
rm(list = ls())
dev.off()


################
## Question 2 ##
################

flowsData <- readWorksheetFromFile("PS4_Data.xlsx",
                                   sheet="Sheet1",
                                   startRow=1,
                                   startCol=1)

flowsData.melt <- melt(flowsData, 
                       id="Year", 
                       measure.vars=c("Internal_Funds_FI","Net_Increase_Liabilities_FI"),
                       variable.name = "Flow_Type",
                       value.name = "Flow")
save(flowsData.melt, file="flowsData.melt.Rdata")