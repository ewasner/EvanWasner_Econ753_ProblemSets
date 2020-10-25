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

## Exponentiate wage 
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

################
## Question 1 ##
##   PART d   ##
################

## Exponentiate wage 
cps85 <- mutate(cps85, wage=exp(lnwage))

## Deflate wages by deflator
cps85.deflator <- 1.649
cps85 <- mutate(cps85, wage=wage/cps85.deflator)
cps85 <- mutate(cps85, lnwage=log(wage))

## Get geometric mean and standard deviation
cps85.geometricMeanlnwage <- exp(mean(cps85$lnwage))
cps85.sdwage <- sd(cps85$wage)

## Annual mean
cps85.annualGeometricMeanlnwage <- 2000 * cps85.geometricMeanlnwage

## Mean and standard deviation of ED
cps85.meanEd <- mean(cps85$ed)
cps85.sdEd <- sd(cps85$ed)

## Sample size
cps85.sampleSize <- 534

## Means for demographic dummy variables
cps85.nonwh.mean <- mean(cps85$nonwh)
cps85.hisp.mean <- mean(cps85$hisp)
cps85.fe.mean <- mean(cps85$fe)

## Counts for demographic dummy variables
cps85.nonwh.count <- cps85.sampleSize * cps85.nonwh.mean
cps85.hisp.count <- cps85.sampleSize * cps85.hisp.mean
cps85.fe.count <- cps85.sampleSize * cps85.fe.mean

## Make table with info
cps85.demographicsTable <- data.frame(Mean = c(cps85.nonwh.mean, cps85.hisp.mean, cps85.fe.mean),
                                      Count = c(cps85.nonwh.count, cps85.hisp.count, cps85.fe.count))
rownames(cps85.demographicsTable) <- c("nonwh", "hisp", "fe")
save(cps85.demographicsTable, file="cps85.demographicsTable.Rdata")

## Filter to subgroups
cps85.male <- filter(cps85, fe==0)
cps85.female <- filter(cps85, fe==1)
cps85.wh <- filter(cps85, nonwh==0 & hisp==0)
cps85.nonwh <- filter(cps85, nonwh==1)
cps85.hisp <- filter(cps85, hisp==1)

## Table for Gender 
cps85.genderTable <- data.frame("Mean ed" = c(mean(cps85.male$ed), mean(cps85.female$ed)),
                                "sd ed" = c(sd(cps85.male$ed), sd(cps85.female$ed)),
                                "Mean Hourly Wage" = c(exp(mean(cps85.male$lnwage)), exp(mean(cps85.female$lnwage))),
                                "sd Wage" = c(sd(cps85.male$wage), sd(cps85.female$wage)),
                                "Mean Annual Wage" = c(2000*exp(mean(cps85.male$lnwage)), 
                                                       2000*exp(mean(cps85.female$lnwage))))
rownames(cps85.genderTable) <- c("Male", "Female")
colnames(cps85.genderTable) <- c("Mean", "SD", "Mean Hourly", "SD", "Mean Annual")
save(cps85.genderTable, file="cps85.genderTable.Rdata")

## Table for Race
cps85.raceTable <- data.frame("Mean ed" = c(mean(cps85.wh$ed), mean(cps85.nonwh$ed), mean(cps85.hisp$ed)),
                              "sd ed" = c(sd(cps85.wh$ed), sd(cps85.nonwh$ed), sd(cps85.hisp$ed)),
                              "Mean Hourly Wage" = c(exp(mean(cps85.wh$lnwage)), exp(mean(cps85.nonwh$lnwage)),
                                                     exp(mean(cps85.hisp$lnwage))),
                              "sd Wage" = c(sd(cps85.wh$wage), sd(cps85.nonwh$wage), sd(cps85.hisp$wage)),
                              "Mean Annual Wage" = c(2000*exp(mean(cps85.wh$lnwage)), 
                                                     2000*exp(mean(cps85.nonwh$lnwage)),
                                                     2000*exp(mean(cps85.hisp$lnwage))))
rownames(cps85.raceTable) <- c("Whites", "NonWhites", "Hispanic")
colnames(cps85.raceTable) <- c("Mean", "SD", "Mean Hourly", "SD", "Mean Annual")
save(cps85.raceTable, file="cps85.raceTable.Rdata")

## Table with totals for both years
cps78.totalTable <- data.frame("Mean ed" = cps78.meanEd,
                               "sd ed" = cps78.sdEd,
                               "Mean Hourly Wage" = cps78.geometricMeanlnwage,
                               "sd Wage" = sd(cps78$wage),
                               "Mean Annual Wage" = cps78.annualGeometricMeanlnwage)
rownames(cps78.totalTable) <- "Whole Sample"
colnames(cps78.totalTable) <- c("Mean", "SD", "Mean Hourly", "SD", "Mean Annual")
save(cps78.totalTable, file="cps78.totalTable.Rdata")
cps85.totalTable <- data.frame("Mean ed" = cps85.meanEd,
                               "sd ed" = cps85.sdEd,
                               "Mean Hourly Wage" = cps85.geometricMeanlnwage,
                               "sd Wage" = cps85.sdwage,
                               "Mean Annual Wage" = cps85.annualGeometricMeanlnwage)
rownames(cps85.totalTable) <- "Whole Sample"
colnames(cps85.totalTable) <- c("Mean", "SD", "Mean Hourly", "SD", "Mean Annual")
save(cps85.totalTable, file="cps85.totalTable.Rdata")

################
## Question 1 ##
##   PART e   ##
################

##################################################
##            First for lnwage                  ##

## Calculate mean and standard deviation of lnwage
cps78.w <- mean(cps78$lnwage)
cps78.s <- sd(cps78$lnwage)

## Separate wage data into categories
cps78$lnwagecat.subgroup <- cut(cps78$lnwage,
                                breaks = c(-Inf, 
                                           cps78.w - 2 * cps78.s,
                                           cps78.w - cps78.s,
                                           cps78.w,
                                           cps78.w + cps78.s,
                                           cps78.w + 2 * cps78.s,
                                           Inf))
cps78$lnwagecat <- factor(cps78$lnwagecat.subgroup,
                          labels = c("wi<=w-2s",
                                     "w-2s<wi<=w-s",
                                     "w-s<wi<=w",
                                     "w<wi<=w+s",
                                     "w+s<wi<=w+2s",
                                     "w+2s<wi"))

## Create table with counts and proportion of sample size
cps78.logNormalTable <- t(rbind(with(cps78, table(lnwagecat)),
                                with(cps78, table(lnwagecat))/cps78.sampleSize))
colnames(cps78.logNormalTable) <- c("Counts", "Proportion")
save(cps78.logNormalTable, file="cps78.logNormalTable.Rdata")

##################################################
##            Next for wage                     ##

## Calculate mean and standard deviation of wage
cps78.w <- mean(cps78$wage)
cps78.s <- sd(cps78$wage)

## Separate wage data into categories
cps78$wagecat.subgroup <- cut(cps78$wage,
                              breaks = c(-Inf, 
                                         cps78.w - 2 * cps78.s,
                                         cps78.w - cps78.s,
                                         cps78.w,
                                         cps78.w + cps78.s,
                                         cps78.w + 2 * cps78.s,
                                         Inf))
cps78$wagecat <- factor(cps78$wagecat.subgroup,
                        levels = c("(-Inf,2.8]",
                                   "(-0.453,2.8]",
                                   "(2.8,6.06]",
                                   "(6.06,9.32]",
                                   "(9.32,12.6]",
                                   "(12.6, Inf]"),
                        labels = c("wi<=w-2s",
                                   "w-2s<wi<=w-s",
                                   "w-s<wi<=w",
                                   "w<wi<=w+s",
                                   "w+s<wi<=w+2s",
                                   "w+2s<wi"))

## Create table with counts and proportion of sample size
cps78.normalTable <- t(rbind(with(cps78, table(wagecat)),
                             with(cps78, table(wagecat))/cps78.sampleSize))
colnames(cps78.normalTable) <- c("Counts", "Proportion")
save(cps78.normalTable, file="cps78.normalTable.Rdata")

## Chi-squared tests
testlnwage <- chisq.test(cps78.logNormalTable[,1],
                          p=c(pnorm(-2),pnorm(-1)-pnorm(-2),pnorm(0)-pnorm(-1),
                              pnorm(1)-pnorm(0),pnorm(2)-pnorm(1),1-pnorm(2)))

testwage <- chisq.test(cps78.normalTable[,1],
                       p=c(pnorm(-2),pnorm(-1)-pnorm(-2),pnorm(0)-pnorm(-1),
                           pnorm(1)-pnorm(0),pnorm(2)-pnorm(1),1-pnorm(2)))

chiTestTable <- data.frame(Statistic=c(testlnwage$statistic,testwage$statistic),
                           pValue=c(testlnwage$p.value,testwage$p.value))
rownames(chiTestTable) <- c("lnwage", "wage")
save(chiTestTable, file="chiTestTable.Rdata")

## Below is just stuff I was practicing with that I want to keep

# dnorm(0)
# 
# pnorm(0)-pnorm(-1)
# 
# seq(cps78.w-3*cps78.s,cps78.w+3*cps78.s, by=0.01),
# dnorm(seq(cps78.w-3*cps78.s,cps78.w+3*cps78.s, by=0.01),
#       mean=cps78.w,sd=cps78.s)
# 
# 
# 
# plot(seq(cps78.w-3*cps78.s,cps78.w+3*cps78.s, by=0.01),
#      dnorm(seq(cps78.w-3*cps78.s,cps78.w+3*cps78.s, by=0.01),
#            mean=cps78.w,sd=cps78.s))
# 
# ggplot(cps78, aes(x=lnwage)) + geom_density()
# 
# 
# , y=densityPlot(lnwage)

################
## Question 6 ##
##   PART a   ##
################

cps78.lnwage.lm1 <- lm(lnwage ~ fe + union + nonwh + hisp + ed + ex + exsq, data=cps78)
save(cps78.lnwage.lm1, file="cps78.lnwage.lm1.Rdata")

linearHypothesis(cps78.lnwage.lm1, c("nonwh = hisp"))

################
## Question 6 ##
##   PART b   ##
################

## Counts table
cps78.demographicsTable6b <- data.frame(Mean = c(1-cps78.nonwh.mean-cps78.hisp.mean, 
                                                 cps78.nonwh.mean, 
                                                 cps78.hisp.mean),
                                        Count = c((1-cps78.nonwh.mean-cps78.hisp.mean)*cps78.sampleSize,
                                                  cps78.nonwh.count, 
                                                  cps78.hisp.count))
rownames(cps78.demographicsTable6b) <- c("other", "nonwh", "hisp")
save(cps78.demographicsTable6b, file="cps78.demographicsTable6b.Rdata")


cps78.raceTable6b <- rbind(with(cps78.wh, data.frame(ed=mean(ed),
                                                     diff=mean(ed)-mean(cps78.wh$ed),
                                                     ex=mean(ex),
                                                     diff=mean(ex)-mean(cps78.wh$ex),
                                                     fe=mean(fe),
                                                     diff=mean(fe)-mean(cps78.wh$fe),
                                                     union=mean(union),
                                                     diff=mean(union)-mean(cps78.wh$union),
                                                     lnwage=mean(lnwage),
                                                     diff=mean(lnwage)-mean(cps78.wh$lnwage))),
                           with(cps78.nonwh, data.frame(ed=mean(ed),
                                                        diff=mean(ed)-mean(cps78.wh$ed),
                                                        ex=mean(ex),
                                                        diff=mean(ex)-mean(cps78.wh$ex),
                                                        fe=mean(fe),
                                                        diff=mean(fe)-mean(cps78.wh$fe),
                                                        union=mean(union),
                                                        diff=mean(union)-mean(cps78.wh$union),
                                                        lnwage=mean(lnwage),
                                                        diff=mean(lnwage)-mean(cps78.wh$lnwage))),
                           with(cps78.hisp, data.frame(ed=mean(ed),
                                                       diff=mean(ed)-mean(cps78.wh$ed),
                                                       ex=mean(ex),
                                                       diff=mean(ex)-mean(cps78.wh$ex),
                                                       fe=mean(fe),
                                                       diff=mean(fe)-mean(cps78.wh$fe),
                                                       union=mean(union),
                                                       diff=mean(union)-mean(cps78.wh$union),
                                                       lnwage=mean(lnwage),
                                                       diff=mean(lnwage)-mean(cps78.wh$lnwage))))
rownames(cps78.raceTable6b) <- c("Others", "NonWhites", "Hispanic")
save(cps78.raceTable6b, file="cps78.raceTable6b.Rdata")

################
## Question 6 ##
##   PART c   ##
################

cps78.wh.lnwage.lm1 <- lm(lnwage ~ fe + union + ed + ex + exsq, data=cps78.wh)
cps78.nonwh.lnwage.lm1 <- lm(lnwage ~ fe + union + ed + ex + exsq, data=cps78.nonwh)
cps78.hisp.lnwage.lm1 <- lm(lnwage ~ fe + union + ed + ex + exsq, data=cps78.hisp)
save(cps78.wh.lnwage.lm1, file="cps78.wh.lnwage.lm1.Rdata")
save(cps78.nonwh.lnwage.lm1, file="cps78.nonwh.lnwage.lm1.Rdata")
save(cps78.hisp.lnwage.lm1, file="cps78.hisp.lnwage.lm1.Rdata")