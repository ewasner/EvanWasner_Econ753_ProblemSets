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
U3  <- get_fred_series("UNRATE", series_name="U3") # U3 unemployment  
U6  <- get_fred_series("U6RATE", series_name="U6") # U6 unemployment
TCU  <- get_fred_series("TCU", series_name="TCU") # Capacity utilization
TCU <- mutate(TCU, TCU=100-TCU) # Convert to underutilization
WageShare <- get_fred_series("W270RE1A156NBEA", series_name = "WageShare")
#WageShare <- mutate(WageShare, WageShare=100-WageShare) # Convert to WageShare
CPIU  <- get_fred_series("CPIAUCSL", series_name="CPI")  

## Merge data to Philips Curve dataframe
PC <- merge(CPIU, U3, by="date", all=TRUE)
PC <- merge(PC, U6, by="date", all=TRUE)
PC <- merge(PC, TCU, by="date", all=TRUE)
PC <- merge(PC, WageShare, by="date", all=TRUE)


## Separate into year and month
PC <- mutate(PC, 
             year=str_split_fixed(date, "-", 3)[,1], 
             month=str_split_fixed(date, "-", 3)[,2])

## Get annual averages of values
PC <- merge(aggregate(PC[,c("CPI","U3", "U6", "TCU")], 
                      by=list(PC$year),
                      FUN=mean,
                      na.action = na.pass),
            na.omit(PC[,c("year","WageShare")]),
            by.x="Group.1",
            by.y="year",
            all=TRUE)

## Calculate inflation
PC <- mutate(PC, 
             CPIU_Inflation=100*(CPI-lag(CPI))/lag(CPI),
             WageShare_Inflation=100*(WageShare-lag(WageShare))/lag(WageShare))
PC <- cbind(data.frame(year=as.double(PC[,"Group.1"])),
            round(PC[,c("U3", "U6", "TCU", "CPIU_Inflation", "WageShare_Inflation")],1))

## Get rid of na rows
#PC <- na.omit(PC)

## Filter into different data
PC.50s <- filter(PC, year>1950 & year<=1960)
PC.60s <- filter(PC, year>1960 & year<=1970)
PC.70s <- filter(PC, year>1970 & year<=1980)
PC.80s <- filter(PC, year>1980 & year<=1990)
PC.90s <- filter(PC, year>1990 & year<=2000)
PC.00s <- filter(PC, year>2000 & year<=2010)
PC.10s <- filter(PC, year>2010 & year<=2020)

## Make list with all data
PC.List <- list(PC.50s,
                PC.60s,
                PC.70s,
                PC.80s,
                PC.90s,
                PC.00s,
                PC.10s)

## Make a list of year ranges for labels
years.List <- list("1950-1960",
                   "1960-1970",
                   "1970-1980",
                   "1980-1990",
                   "1990-2000",
                   "2000-2010",
                   "2010-2020")

## Assign a ggplot for each year: U3
# for(val in c(1:7)) {
#   assign(paste0("p_U3_",val),
#          ggplot(PC.List[[val]], aes(x=U3, y=CPIU_Inflation)) + 
#            geom_point() + 
#            xlab("Unemployment (U3) Rate") +
#            ylab("Inflation (CPI)") +
#            ggtitle(years.List[[val]]) +
#            geom_smooth(method="lm", se=FALSE) +
#            theme_bw())
# }
# 
# ## Assign a ggplot for each year: U6
# for(val in c(5:7)) {
#   assign(paste0("p_U6_",val),
#          ggplot(PC.List[[val]], aes(x=U6, y=CPIU_Inflation)) + 
#            geom_point() + 
#            xlab("Unemployment (U6) Rate") +
#            ylab("Inflation (CPI)") +
#            ggtitle(years.List[[val]]) +
#            geom_smooth(method="lm", se=FALSE) +
#            theme_bw())
# }
# 
# ## Assign a ggplot for each year: TCU
# for(val in c(3:7)) {
#   assign(paste0("p_TCU_",val),
#          ggplot(PC.List[[val]], aes(x=TCU, y=CPIU_Inflation)) + 
#            geom_point() + 
#            xlab("Capacity Underutilization Rate") +
#            ylab("Inflation (CPI)") +
#            ggtitle(years.List[[val]]) +
#            geom_smooth(method="lm", se=FALSE) +
#            theme_bw())
# }
# 
# ## Assign a ggplot for each year: WageShare Inflation
# for(val in c(1:7)) {
#   assign(paste0("p_WageShare_",val),
#          ggplot(PC.List[[val]], aes(x=U3, y=WageShare_Inflation)) + 
#            geom_point() + 
#            xlab("Unemployment (U3) Rate") +
#            ylab("Inflation (WageShare)") +
#            ggtitle(years.List[[val]]) +
#            geom_smooth(method="lm", se=FALSE) +
#            theme_bw())
# }
# 
# ## Different grid plots for U3
# grid.arrange(p_U3_1, p_U3_2, p_U3_3, p_U3_4, nrow = 2)
# grid.arrange(p_U3_5,p_U3_6,p_U3_7, nrow=2)
# 
# ## Different grid plots for U6
# grid.arrange(p_U6_5,p_U6_6,p_U6_7, nrow=2)
# 
# ## Different grid plots for TCU
# grid.arrange(p_TCU_3,p_TCU_4, nrow=1)
# grid.arrange(p_TCU_5,p_TCU_6,p_TCU_7, nrow=2)
# 
# ## Different grid plots for U3 vs WageShare
# grid.arrange(p_WageShare_1, p_WageShare_2, p_WageShare_3, p_WageShare_4, nrow = 2)
# grid.arrange(p_WageShare_5,p_WageShare_6,p_WageShare_7, nrow=2)
# 
# 
# # ggplot(PC, aes(x=U3, y=CPIU_Inflation)) +
# #   geom_point() +
# #   xlab("Unemployment (U3) Rate") +
# #   ylab("Inflation") +
# # #  ggtitle(years.List[[val]]) +
# #   geom_smooth(method="lm", se=FALSE) +
# #   theme_bw()
# # 
# # ggplot(U3, aes(x=date, y=U3)) + geom_line()
# ggplot(PC, aes(x=U3, y=WageShare_Inflation)) + geom_point()

## Scale TCU
PC <- mutate(PC, TCU_Scaled=2*TCU/3)

## Melt to get multiple line plots
PC.Melt.U <- melt(PC,
                  id="year",
                  measure.vars=c("U3", 
                                 "U6", 
                                 "TCU_Scaled"),
                  variable.name="Rate_Type",
                  value.name="Value")

PC.Melt.I <- melt(PC,
                  id="year",
                  measure.vars=c("CPIU_Inflation", 
                                 "WageShare_Inflation"),
                  variable.name="Rate_Type",
                  value.name="Value")

## Save data
save(PC,
     PC.List,
     PC.Melt.I,
     PC.Melt.U,
     years.List,
     file="PC.Rdata")

## Plot time series
# grid.arrange(ggplot(PC.Melt.U, aes(x=year, y=Value, col=Rate_Type)) + 
#                geom_line(size=1) +
#                scale_color_discrete(name="Rate Type",
#                                     labels=c("U3",
#                                              "U6", 
#                                              "Capacity Underutilization\n(Scaled by 2/3)")) +
#                theme_bw(),
#              ggplot(PC.Melt.I, aes(x=year, y=Value, col=Rate_Type)) + 
#                geom_line(size=1) +
#                scale_color_discrete(name="Rate Type",
#                                     labels=c("Inflation (CPI)", 
#                                              "Inflation (Profit Share)")) +
#                theme_bw(), 
#              nrow=2)
# 
# ggplot(PC.Melt, aes(x=year, y=Value, col=Rate_Type)) + 
#   geom_line(size=1) +
#   scale_color_discrete(name="Rate Type",
#                        labels=c("U3",
#                                 "U6", 
#                                 "Capacity Underutilization\n(Scaled by 2/3)", 
#                                 "Inflation (CPI)", 
#                                 "Inflation (Profit Share)")) +
#   theme_bw()
# 
# ggplot(PC, aes(x=year, y=TCU)) + geom_line()