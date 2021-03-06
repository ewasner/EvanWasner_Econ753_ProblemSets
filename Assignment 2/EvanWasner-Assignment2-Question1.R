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
cor59 <- format(cor59, digits=3)
cor65 <- cor(select(chow65, lnrent, lnmult, lnaccess, lnadd, lnmem))
cor65 <- format(cor65, digits=3)
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

## Remove row names
rownames(priceIndexTable) <- c()

## Save table
priceIndexTable <- format(priceIndexTable, digits=3)
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

################
## Question 6 ##
##   PART a   ##
################

## Add dummy variables for years 1955-1960
chow <- mutate(chow, d60=ifelse(year==60,1,0), d55=ifelse(year==55,1,0), d56=ifelse(year==56,1,0),
               d57=ifelse(year==57,1,0), d58=ifelse(year==58,1,0), d59=ifelse(year==59,1,0))

## Pooled regression
chow.lm <- lm(lnrent ~ d55 + d56 + d57 + d58 + d59 + d60 + 
                d61 + d62 + d63 + d64 + d65 + 
                lnmult + lnmem + lnaccess, 
                data=chow)

## Chained price indices - create vector
chainedIndices <- vector()
chainedIndices[1] = 0

## Run for loop which creates lm variables for each year of regression
## and extracts each dummy variable coefficient into the chainedIndices vector
for(yr in 54:64) { 
  assign(paste("chow",yr,yr+1,".lm",sep=""), 
         lm(paste("lnrent ~ lnmult + lnaccess + lnmem + d", yr+1,sep =""), 
            data=filter(chow, year==yr | year==yr+1)))
  chainedIndices[yr-52] <- coef(get(paste("chow",yr,yr+1,".lm",sep="")))[5]
} 

## Create table with coefficients and price indices and save
priceIndexTable2 <- data.frame(year=1954:1965, 
                               pooled_Coef=c(0, coef(chow.lm)[2:12]))
priceIndexTable2 <- mutate(priceIndexTable2, 
                           pooled_Coef_Dif=pooled_Coef-lag(pooled_Coef),
                           chained_Coef=chainedIndices)

## The following is incorrect code but I want to keep it for the future:
# , blank=vector(mode="character", length=12)
# priceIndexTable2 <- mutate(priceIndexTable2, pooled_Indices=exp(pooled_Coef),
#                            chained_Indices=exp(chained_Coef))
# colnames(priceIndexTable2) <- c("Year", "Pooled coefficients", "Chained Coefficients", "          ", 
#                                 "Pooled CPI Indices", "Chained CPI Indices")
## End incorrect code

colnames(priceIndexTable2) <- c("Year", "Pooled Cooefficients", "Changes in Pooled Coefficients",
                                "Adjacent Year Coefficients")
rownames(priceIndexTable2) <- c()
priceIndexTable2 <- format(priceIndexTable2, digits=3)
save(priceIndexTable2, file="priceIndexTable2.Rdata")

################
## Question 6 ##
##   PART b   ##
################

## Create a table that contains the pooled regression coefficients, the summed adjacent-year coefficients,
## and the exponentiated values of each
sixBtable <-format(mutate(data.frame(pooled_Coef=coef(chow.lm)[2:12],coef_Summed=cumsum(priceIndexTable2[,4][2:12])),
                          pooled_Indices=exp(pooled_Coef), summed_Indices=exp(coef_Summed)),digits=2, scientific=8)
##sixBtable <- format(mutate(data.frame(pooled_Coef=coef(chow.lm)[2:12],coef_Summed=cumsum(coef(chow.lm)[2:12])),
##       pooled_Indices=exp(pooled_Coef), summed_Indices=exp(coef_Summed)),digits=2, scientific=8)
sixBtable <- rbind(c(0,0,1,1),sixBtable)
rownames(sixBtable) <- c()
priceIndexTable3 <- data.frame(year=1954:1965, pooled_Indices=sixBtable$pooled_Indices,
                               summed_Indices=sixBtable$summed_Indices)
colnames(priceIndexTable3) <- c("Year", "Pooled CPI Indices", "Chained CPI Indices")
save(priceIndexTable3,file="priceIndexTable3.Rdata")

