## Evan Wasner
## Econ 753
## Assignment 2, Question 1

## Set working directory
setwd("I:/Evan/Documents/Umass/Econ 753/EvanWasner_Econ753_ProblemSets/Assignment 2")

## Libraries
library(tidyverse)
library(XLConnect)

## Clear workplace
rm(list = ls())
dev.off()

## Read CPI-U data from spreadsheet
cpiU <- readWorksheetFromFile("CPI_U.xls",
                              sheet="Monthly",
                              startRow=1,
                              startCol=1)
cpiU <- cbind(cpiU, "CPI-U")
colnames(cpiU) <- c("date", "cpi", "type")

CcpiU <- readWorksheetFromFile("C_CPI_U.xls",
                               sheet="FRED Graph",
                               startRow=11,
                               startCol=1)
CcpiU <- cbind(CcpiU, "C-CPI-U")
colnames(CcpiU) <- c("date", "cpi", "type")

RcpiURS <- readWorksheetFromFile("R_CPI_U_RS.xlsx",
                                 sheet="Table 1",
                                 startRow=6,
                                 startCol=1)

## Separate year and month
cpiU <- mutate(cpiU, year=str_split_fixed(date, "-", 3)[,1], month=str_split_fixed(date, "-", 3)[,2])
CcpiU <- mutate(CcpiU, year=str_split_fixed(date, "-", 3)[,1], 
                month=str_split_fixed(date, "-", 3)[,2])



## Get annual CPI averages and convert to base 2018
cpiUSummarizeAll <- summarize_all(group_by(cpiU,year),mean)
cpiUAnnual <- data.frame(year=cpiUSummarizeAll$year, cpi=cpiUSummarizeAll$cpi)
cpiUAnnual <- cbind(cpiUAnnual, type="CPI-U")
cpiUBase <- as.double(filter(cpiUAnnual, year==2018)[2][1])
cpiUAnnual <- mutate(cpiUAnnual, cpi = cpi/(0.01*cpiUBase))

CcpiUSummarizeAll <- summarize_all(group_by(CcpiU,year),mean)
CcpiUAnnual <- data.frame(year=CcpiUSummarizeAll$year, cpi=CcpiUSummarizeAll$cpi)
CcpiUAnnual <- cbind(CcpiUAnnual, type="C-CPI-U")
CcpiUBase <- as.double(filter(CcpiUAnnual, year==2018)[2][1])
CcpiUAnnual <- mutate(CcpiUAnnual, cpi = cpi/(0.01*CcpiUBase))

RcpiURSAnnual <- data.frame(year=as.character(RcpiURS$YEAR), cpi=RcpiURS$AVG)
RcpiURSAnnual <- cbind(RcpiURSAnnual, type="R-CPI-U-RS")
RcpiURSBase <- as.double(filter(RcpiURSAnnual, year==2018)[2][1])
RcpiURSAnnual <- mutate(RcpiURSAnnual, cpi = cpi/(0.01*RcpiURSBase))

## Calculate inflation rate
cpiUAnnual <- mutate(cpiUAnnual, previous=lag(cpi), inflation=100*(cpi-previous)/previous)
CcpiUAnnual <- mutate(CcpiUAnnual, previous=lag(cpi), inflation=100*(cpi-previous)/previous)
RcpiURSAnnual <- mutate(RcpiURSAnnual, previous=lag(cpi), inflation=100*(cpi-previous)/previous)


ggplot(filter(bind_rows(cpiUAnnual,CcpiUAnnual,RcpiURSAnnual), year >= 1970), aes(x=year)) + 
  geom_line(aes(y=inflation, color=type, group = 1), size=2) +
  scale_x_discrete(breaks=c(1970,1980,1990,2000,2010,2020))

ggplot(filter(bind_rows(cpiUAnnual,CcpiUAnnual,RcpiURSAnnual), year >= 1970), aes(x=year)) + 
  geom_point(aes(y=inflation, color=type), size=2) +
  scale_x_discrete(breaks=c(1970,1980,1990,2000,2010,2020))
