## Evan Wasner
## Econ 753
## Assignment 3, Question 1

## Set working directory
setwd("I:/Evan/Documents/Umass/Econ 753/EvanWasner_Econ753_ProblemSets/Assignment 3")

## Libraries
library(foreign)
library(tidyverse)

## Clear workplace
rm(list = ls())
dev.off()


################
## Question 1 ##
##   PART a   ##
################

## Extract data
cps <- read.dta("http://courses.umass.edu/econ753/berndt/stata/chap5-cps.dta")

## Filter for years 1978 and 1985
cps78 <- filter(cps, year==1978)
cps85 <- filter(cps, year==1985)

## Exponentiate wage for geometric mean
cps78 <- mutate(cps78, wage=exp(lnwage))

## Get geometric and arithmetic mean
geometricMeanlnwage <- exp(mean(cps78$lnwage))
arithmeticMeanlnwage <- mean(cps78$wage)

## Annual means
annualGeometricMeanlnwage <- 2000 * geometricMeanlnwage
annualArithmeticMeanlnwage <- 2000 * arithmeticMeanlnwage

## Mean and standard deviation of ED and EX
meanEd <- mean(cps78$ed)
sdEd <- sd(cps78$ed)
meanEx <- mean(cps78$ex)
sdEx <- sd(cps78$ex)

## Make tables with info
lnwageMeanTable <- data.frame("Geometric Mean" = c(geometricMeanlnwage, annualGeometricMeanlnwage),
                              "Arithmetic Mean" = c(arithmeticMeanlnwage, annualArithmeticMeanlnwage))
rownames(lnwageMeanTable) <- c("Hourly", "Annual")
save(lnwageMeanTable, file="lnwageMeanTable.Rdata")

edExTable <- data.frame(Mean = c(meanEd, meanEx),
                        "Standard Deviation" = c(sdEd, sdEx))
rownames(edExTable) <- c("Education", "Experience")
save(edExTable, file="edExTable.Rdata")
