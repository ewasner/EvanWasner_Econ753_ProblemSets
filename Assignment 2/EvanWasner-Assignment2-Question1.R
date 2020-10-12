## Evan Wasner
## Econ 753
## Assignment 2, Question 1

## Set working directory
setwd("I:/Evan/Documents/Umass/Econ 753/EvanWasner_Econ753_ProblemSets/Assignment 2")

## Libraries
library(foreign)
library(tidyverse)
library(nlme)

## Clear workplace
rm(list = ls())
dev.off()


################
## Question 3 ##
##   PART a   ##
################

## Import data
chow <- read.dta("http://courses.umass.edu/econ753/berndt/stata/chow.dta")

## Add columns
chow <- mutate(chow, lnrent=log(rent), lnmult=log(mult), lnaccess=log(access), 
               lnadd=log(add), mem=words*binary*digits, lnmem=log(words*binary*digits),
               d61=ifelse(year==61,1,0), d62=ifelse(year==62,1,0), d63=ifelse(year==63,1,0),
               d64=ifelse(year==64,1,0), d65=ifelse(year==65,1,0))

## Filter data into different years
chow59 <- filter(chow, year>=54 & year<=59)
chow65 <- filter(chow, year>=60 & year<=65)

## Construct correlation matrices and save data
cor59 <- cor(select(chow59, lnrent, lnmult, lnaccess, lnadd, lnmem))
cor65 <- cor(select(chow65, lnrent, lnmult, lnaccess, lnadd, lnmem))
save(cor59,file="cor59.Rdata")
save(cor65,file="cor65.Rdata")

################
## Question 3 ##
##   PART b   ##
################

## Run regression of lnrent and save data
chow65.lm <- lm(lnrent ~ d61 + d62 + d63 + d64 + d65 + lnmult + lnmem + lnaccess, 
               data=chow65)
save(chow65.lm,file="chow65.lm.Rdata")

## Create table for price indices
priceIndexTable <- data.frame(Year=c("1960", "1961", "1962", "1963", "1964", "1965"),
                              Estimated_Coefficient=chow65.lm$coefficients[1:6])

## Set price index for year 1960 to 1 and take antilogarithms of coefficients to construct price indices 
priceIndexTable <- mutate(priceIndexTable, Price_Index=ifelse(Year==1960,1,exp(Estimated_Coefficient)))

## Save table
save(priceIndexTable,file="priceIndexTable.Rdata")

################
## Question 3 ##
##   PART e   ##
################

## Perform GLS regression and save output
chow65.gls <- lm(lnrent ~ d61 + d62 + d63 + d64 + d65 + lnmult + lnmem + lnaccess, 
                data=chow65, weights=sqrt(volume))
save(chow65.gls,file="chow65.gls.Rdata")


##########################################################################################
## This is just personal stuff testing out how to do regressions with matrices,         ##
## ignore everything in between these comment sections...                               ##
xMatrix <- cbind(numeric(82)+1, 
                 as.matrix(select(chow65, lnmult, lnaccess, lnmem,
                            d61, d62, d63, d64, d65)))
yMatrix <- as.matrix(chow65$lnrent)

betaMatrix <- solve(t(xMatrix) %*% xMatrix) %*% t(xMatrix) %*% yMatrix
summary(chow65.lm)

uhat <- yMatrix - xMatrix %*% betaMatrix
varCovarMatrix <- t(uhat) %*% uhat
sigma <- t(uhat) %*% uhat / (82 - 9)
diag(sigma[1] * solve(t(xMatrix) %*% xMatrix))

## GLS with Matrix Algebra
xMatrixgls <- as.matrix(select(chow65, generalizedvolume, generalizedlnmult, generalizedlnaccess, generalizedlnmem,
                            generalizedd61, generalizedd62, generalizedd63, generalizedd64, generalizedd65))

yMatrixgls <- as.matrix(chow65$generalizedlnrent)

betaMatrixgls <- solve(t(xMatrixgls) %*% xMatrixgls) %*% t(xMatrixgls) %*% yMatrixgls
betaMatrixgls
summary(chow.gls65)
##########################################################################################
##                            Okay, finish ignoring                                     ##
##########################################################################################