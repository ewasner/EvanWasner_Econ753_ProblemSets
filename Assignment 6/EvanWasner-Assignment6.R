## Evan Wasner
## Econ 753
## Assignment 5

## Set working directory
setwd("I:/Evan/Documents/Umass/Econ 753/EvanWasner_Econ753_ProblemSets/Assignment 6")

## Libraries
library(alfred) # Read from FRED
library(tidyverse)
library(reshape2)
library(gridExtra)

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
PC.Classic <- mutate(PC.Classic, 
                     year=str_split_fixed(date, "-", 3)[,1], 
                     month=str_split_fixed(date, "-", 3)[,2])

## Get annual averages of values
PC.Classic <- aggregate(PC.Classic[,c("CPI","U3")], 
                        by=list(PC.Classic$year),
                        FUN=mean)

## Calculate inflation
PC.Classic <- mutate(PC.Classic, CPIU_Inflation=100*(CPI-lag(CPI))/lag(CPI))
PC.Classic <- cbind(data.frame(year=PC.Classic[,"Group.1"]),round(PC.Classic[,2:4],1))

## Get rid of na rows
PC.Classic <- na.omit(PC.Classic)

## Filter into different data
PC.Classic.50s <- filter(PC.Classic, year>1950 & year<=1960)
PC.Classic.60s <- filter(PC.Classic, year>1960 & year<=1970)
PC.Classic.70s <- filter(PC.Classic, year>1970 & year<=1980)
PC.Classic.80s <- filter(PC.Classic, year>1980 & year<=1990)
PC.Classic.90s <- filter(PC.Classic, year>1990 & year<=2000)
PC.Classic.00s <- filter(PC.Classic, year>2000 & year<=2010)
PC.Classic.10s <- filter(PC.Classic, year>2010 & year<=2020)

PC.Classic.List <- list(PC.Classic.50s,
                        PC.Classic.60s,
                        PC.Classic.70s,
                        PC.Classic.80s,
                        PC.Classic.90s,
                        PC.Classic.00s,
                        PC.Classic.10s)

years.List <- list("1950-1960",
                   "1960-1970",
                   "1970-1980",
                   "1980-1990",
                   "1990-2000",
                   "2000-2010",
                   "2010-2020")

for(val in c(1:7)) {
  assign(paste0("p",val),
         ggplot(PC.Classic.List[[val]], aes(x=U3, y=CPIU_Inflation)) + 
           geom_point() + 
           xlab("Unemployment (U3) Rate") +
           ylab("Inflation") +
           ggtitle(years.List[[val]]) +
           geom_smooth(method="lm", se=FALSE) +
           theme_bw())
}

grid.arrange(p1, p2, p3, p4, nrow = 2)
grid.arrange(p5,p6,p7, nrow=2)

ggplot(PC.Classic, aes(x=U3, y=CPIU_Inflation)) + 
  geom_point() + 
  xlab("Unemployment (U3) Rate") +
  ylab("Inflation") +
#  ggtitle(years.List[[val]]) +
  geom_smooth(method="lm", se=FALSE) +
  theme_bw()

ggplot(U3, aes(x=date, y=U3)) + geom_line()
ggplot(PC.Classic, aes(x=U3, y=CPIU_Inflation)) + geom_point()

PC.Classic$year <- as.double(PC.Classic$year)

PC.Classic.Melt <- melt(PC.Classic,
                        id="year",
                        measure.vars=c("U3","CPIU_Inflation"),
                        variable.name="Rate_Type",
                        value.name="Value")

PC.Classic.Melt$year <- as.factor(PC.Classic.Melt$year)

ggplot(PC.Classic.Melt, aes(x=year, y=Value, col=Rate_Type)) + 
  geom_line(size=1) +
  scale_color_discrete(name="Rate Type",
                       labels=c("U3","Inflation")) +
  theme_bw()

is.factor(PC.Classic$year)