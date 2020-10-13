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

#####################################################
##            Inflation calculations               ##
#####################################################

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

cpiUX1 <- readWorksheetFromFile("ERP-2012-table62.xls",
                                sheet="B62",
                                startRow=5,
                                startCol=1,
                                endRow=15,
                                endCol=10,
                                header=FALSE)
cpiUX1 <- data.frame(year=cpiUX1[,1],cpi=cpiUX1[,10])

## CPI-U-X1 rate of change
cpiUX1 <- mutate(cpiUX1, change=(cpi-lead(cpi))/lead(cpi))

## Separate year and month
cpiU <- mutate(cpiU, year=str_split_fixed(date, "-", 3)[,1], month=str_split_fixed(date, "-", 3)[,2])
CcpiU <- mutate(CcpiU, year=str_split_fixed(date, "-", 3)[,1], 
                month=str_split_fixed(date, "-", 3)[,2])
cpiUX1 <- mutate(cpiUX1, year=substr(cpiUX1$year,1,4))



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

## Calculate CPI-U-RS values for 1968-1977 based on change in CPI-U-X1
cpiUX1 <- cbind(cpiUX1, cpiURS=vector(mode="double", length=11))
cpiUX1[11,4] <- as.double(filter(RcpiURSAnnual,year==1978)$cpi)
for (val in 10:1){
  cpiUX1[val,4] <- cpiUX1[val+1,4]*(1+cpiUX1[val,3])
}

## Input extrapolated years in R-CPI-U-RS
RcpiURSAnnual <- filter(RcpiURSAnnual, year>1978)
RcpiURSAnnual <- rbind(data.frame(year=cpiUX1$year,cpi=cpiUX1$cpiURS, type="R-CPI-U-RS"), RcpiURSAnnual)

## Convert R-CPI-U-RS to 2018 base
RcpiURSBase <- as.double(filter(RcpiURSAnnual, year==2018)[2][1])
RcpiURSAnnual <- mutate(RcpiURSAnnual, cpi = cpi/(0.01*RcpiURSBase))

## Calculate inflation rate
cpiUAnnual <- mutate(cpiUAnnual, previous=lag(cpi), inflation=100*(cpi-previous)/previous)
CcpiUAnnual <- mutate(CcpiUAnnual, previous=lag(cpi), inflation=100*(cpi-previous)/previous)
RcpiURSAnnual <- mutate(RcpiURSAnnual, previous=lag(cpi), inflation=100*(cpi-previous)/previous)

## Save data
save(cpiUAnnual,file="cpiUAnnual.Rdata")
save(CcpiUAnnual,file="CcpiUAnnual.Rdata")
save(RcpiURSAnnual,file="RcpiURSAnnual.Rdata")

# ggplot(filter(bind_rows(cpiUAnnual,CcpiUAnnual,RcpiURSAnnual), year >= 1970), aes(x=year)) + 
#   geom_line(aes(y=inflation, color=type, group = 1), size=2) +
#   scale_x_discrete(breaks=c(1970,1980,1990,2000,2010,2020))

inflationPlot <- ggplot(filter(bind_rows(cpiUAnnual,CcpiUAnnual,RcpiURSAnnual), 
                               year >= 1970), aes(x=year)) + 
  geom_point(aes(y=inflation, color=type), size=2) +
  scale_x_discrete(breaks=c(1970,1980,1990,2000,2010,2020))
save(inflationPlot,file="inflationPlot.Rdata")


#####################################################
##                Minimum Wage                     ##
#####################################################

## Extract data
wage <- readWorksheetFromFile("Min_wage.xlsx",
                              sheet="Min wageHistory of Federal Mini",
                              startRow=6,
                              startCol=1,
                              endCol=2)
colnames(wage) <- c("year", "Nominal_Wage")

## Calculate real wage in CPI-U-RS values (already based to 100 in year 2018)
wage <- cbind(wage, Real_Wage=wage$Nominal_Wage / (RcpiURSAnnual$cpi/100))

wagePlot <- ggplot(wage, aes(x=year, y=Real_Wage)) + geom_line() + 
  xlab("Year") + ylab("Real Wage") + theme_bw()

## Calculating hypothetical wage - gathering inflation rate
wage <- cbind(wage, inflation=RcpiURSAnnual$inflation, hype_Wage=vector(mode="double", length=52))
wage$hype_Wage[1] <- wage$Nominal_Wage[1]

## Calculate hypothetical wage based on inflation rate
for (val in 2:52){
  wage$hype_Wage[val] <- wage$hype_Wage[val-1] * (1 + 0.01 * wage$inflation[val])
}

wage2 <- gather(data=wage, "Wage_Type", "Wage", -year, -inflation, -Real_Wage)

wage2Plot <- ggplot(wage2, aes(x=year, y=Wage, color=Wage_Type)) + geom_line(size=1) + 
  scale_y_continuous(name="Real Wage", limits=c(0,15)) +
  scale_x_continuous(name="Year") + theme_bw() + 
  scale_color_discrete(name="Legend", labels=c("Hypothetical Wage", "Actual Nominal Wage"))

## Save wage
save(wage,file="wage.Rdata")
save(wage2,file="wage2.Rdata")