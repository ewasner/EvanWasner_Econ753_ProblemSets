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
cps78.geometricMeanlnwage <- exp(mean(cps78$lnwage))
cps78.arithmeticMeanlnwage <- mean(cps78$wage)

## Annual means
cps78.annualGeometricMeanlnwage <- 2000 * cps78.geometricMeanlnwage
cps78.annualArithmeticMeanlnwage <- 2000 * cps78.arithmeticMeanlnwage

## Mean and standard deviation of ED and EX
cps78.meanEd <- mean(cps78$ed)
cps78.sdEd <- sd(cps78$ed)
cps78.meanEx <- mean(cps78$ex)
cps78.sdEx <- sd(cps78$ex)

## Make tables with info
cps78.lnwageMeanTable <- data.frame("Geometric Mean" = c(cps78.geometricMeanlnwage, cps78.annualGeometricMeanlnwage),
                              "Arithmetic Mean" = c(cps78.arithmeticMeanlnwage, cps78.annualArithmeticMeanlnwage))
rownames(cps78.lnwageMeanTable) <- c("Hourly", "Annual")
colnames(cps78.lnwageMeanTable) <- c("Geometric Mean", "Arithmetic Mean")
save(cps78.lnwageMeanTable, file="cps78.lnwageMeanTable.Rdata")

cps78.edExTable <- data.frame(Mean = c(cps78.meanEd, cps78.meanEx),
                        "Standard Deviation" = c(cps78.sdEd, cps78.sdEx))
rownames(cps78.edExTable) <- c("Education", "Experience")
colnames(cps78.edExTable) <- c("Mean", "Standard Deviation")
save(cps78.edExTable, file="cps78.edExTable.Rdata")

################
## Question 1 ##
##   PART b   ##
################

## Sample size
cps78.sampleSize <- 550

## Means for demographic dummy variables
cps78.nonwh.mean <- mean(cps78$nonwh)
cps78.hisp.mean <- mean(cps78$hisp)
cps78.fe.mean <- mean(cps78$fe)

## Counts for demographic dummy variables
cps78.nonwh.count <- cps78.sampleSize * cps78.nonwh.mean
cps78.hisp.count <- cps78.sampleSize * cps78.hisp.mean
cps78.fe.count <- cps78.sampleSize * cps78.fe.mean

## Make table with info
cps78.demographicsTable <- data.frame(Mean = c(cps78.nonwh.mean, cps78.hisp.mean, cps78.fe.mean),
                                      Count = c(cps78.nonwh.count, cps78.hisp.count, cps78.fe.count))
rownames(cps78.demographicsTable) <- c("nonwh", "hisp", "fe")
save(cps78.demographicsTable, file="cps78.demographicsTable.Rdata")

################
## Question 1 ##
##   PART c   ##
################

## Filter to subgroups
cps78.male <- filter(cps78, fe==0)
cps78.female <- filter(cps78, fe==1)
cps78.wh <- filter(cps78, nonwh==0 & hisp==0)
cps78.nonwh <- filter(cps78, nonwh==1)
cps78.hisp <- filter(cps78, hisp==1)

## Table for Gender 
cps78.genderTable <- data.frame("Mean ed" = c(mean(cps78.male$ed), mean(cps78.female$ed)),
                                "sd ed" = c(sd(cps78.male$ed), sd(cps78.female$ed)),
                                "Mean Hourly Wage" = c(exp(mean(cps78.male$lnwage)), exp(mean(cps78.female$lnwage))),
                                "sd Wage" = c(sd(cps78.male$wage), sd(cps78.female$wage)),
                                "Mean Annual Wage" = c(2000*exp(mean(cps78.male$lnwage)), 
                                                       2000*exp(mean(cps78.female$lnwage))))
rownames(cps78.genderTable) <- c("Male", "Female")
colnames(cps78.genderTable) <- c("Mean", "SD", "Mean Hourly", "SD", "Mean Annual")
save(cps78.genderTable, file="cps78.genderTable.Rdata")

## Table for Race
cps78.raceTable <- data.frame("Mean ed" = c(mean(cps78.wh$ed), mean(cps78.nonwh$ed), mean(cps78.hisp$ed)),
                              "sd ed" = c(sd(cps78.wh$ed), sd(cps78.nonwh$ed), sd(cps78.hisp$ed)),
                              "Mean Hourly Wage" = c(exp(mean(cps78.wh$lnwage)), exp(mean(cps78.nonwh$lnwage)),
                                                       exp(mean(cps78.hisp$lnwage))),
                              "sd Wage" = c(sd(cps78.wh$wage), sd(cps78.nonwh$wage), sd(cps78.hisp$wage)),
                              "Mean Annual Wage" = c(2000*exp(mean(cps78.wh$lnwage)), 
                                                       2000*exp(mean(cps78.nonwh$lnwage)),
                                                       2000*exp(mean(cps78.hisp$lnwage))))
rownames(cps78.raceTable) <- c("Whites", "NonWhites", "Hispanic")
colnames(cps78.raceTable) <- c("Mean", "SD", "Mean Hourly", "SD", "Mean Annual")
save(cps78.raceTable, file="cps78.raceTable.Rdata")
