## Evan Wasner
## Econ 753
## Assignment 5

## Set working directory
setwd("I:/Evan/Documents/Umass/Econ 753/EvanWasner_Econ753_ProblemSets/Assignment 6")

## Libraries
library(alfred) # Read from FRED
library(tidyverse)
library(reshape2)

## Clear workplace
rm(list = ls())
dev.off()


####################
## Traditional PC ##
####################

## Get data from FRED
U3  <- get_fred_series("UNRATE", series_name="U3")  
CPIU  <- get_fred_series("CPIAUCSL", series_name="CPI")  

## Classic PC
PC.Classic <- merge(CPIU, U3, by="date", all=TRUE)

## Separate into year and month
CPIU <- mutate(CPIU, year=str_split_fixed(date, "-", 3)[,1], month=str_split_fixed(date, "-", 3)[,2])
PC.Classic <- mutate(PC.Classic, 
                     year=str_split_fixed(date, "-", 3)[,1], 
                     month=str_split_fixed(date, "-", 3)[,2])

## Get annual averages of values
PC.Classic <- aggregate(PC.Classic[,c("CPI","U3")], 
                        by=list(PC.Classic$year),
                        FUN=mean)

## Calculate inflation
PC.Classic <- mutate(PC.Classic, CPIU_Inflation=100*(CPI-lag(CPI))/lag(CPI))

ggplot(U3, aes(x=date, y=UNRATE)) + geom_line()
ggplot(PC.Classic, aes(x=U3, y=CPIU_Inflation)) + geom_point()