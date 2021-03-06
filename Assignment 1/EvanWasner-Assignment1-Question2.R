## Evan Wasner
## Econ 753
## Assignment 1, Question 2

## Set working directory
setwd("I:/Evan/Documents/Umass/Econ 753/Assignments/Assignment 1/WP322HAP-RR-GITD-code/RR-country-csv")

## Libraries
library(plyr)
library(ggplot2)
library(car)
library(foreign)
library(gridExtra)
options(scipen=10000)
options(digits=4)

## Clear Workspace
rm(list = ls())
dev.off()



Australia     <- read.csv("RR - Australia.csv") 
Austria       <- read.csv("RR - Austria.csv")   
Belgium       <- read.csv("RR - Belgium.csv")   
Canada        <- read.csv("RR - Canada.csv")    
Denmark       <- read.csv("RR - Denmark.csv")   
Finland       <- read.csv("RR - Finland.csv")   
France        <- read.csv("RR - France.csv")    
Germany       <- read.csv("RR - Germany.csv")   
Greece        <- read.csv("RR - Greece.csv")    
Ireland       <- read.csv("RR - Ireland.csv")   
Italy         <- read.csv("RR - Italy.csv")     
Japan         <- read.csv("RR - Japan.csv")     
Netherlands   <- read.csv("RR - Netherlands.csv")
NewZealand    <- read.csv("RR - New Zealand.csv")
Norway        <- read.csv("RR - Norway.csv")
Portugal      <- read.csv("RR - Portugal.csv")
Spain         <- read.csv("RR - Spain.csv")
Sweden        <- read.csv("RR - Sweden.csv")
UK            <- read.csv("RR - UK.csv")
US            <- read.csv("RR - US.csv")

RR <- merge(Australia,Austria,all=TRUE)
RR <- merge(RR,Belgium    ,all=TRUE)
RR <- merge(RR,Canada     ,all=TRUE)
RR <- merge(RR,Denmark    ,all=TRUE)
RR <- merge(RR,Finland    ,all=TRUE)
RR <- merge(RR,France     ,all=TRUE)
RR <- merge(RR,Germany    ,all=TRUE)
RR <- merge(RR,Greece     ,all=TRUE)
RR <- merge(RR,Ireland    ,all=TRUE)
RR <- merge(RR,Italy      ,all=TRUE)
RR <- merge(RR,Japan      ,all=TRUE)
RR <- merge(RR,Netherlands,all=TRUE)
RR <- merge(RR,NewZealand ,all=TRUE)
RR <- merge(RR,Norway     ,all=TRUE)
RR <- merge(RR,Portugal   ,all=TRUE)
RR <- merge(RR,Spain      ,all=TRUE)
RR <- merge(RR,Sweden     ,all=TRUE)
RR <- merge(RR,UK         ,all=TRUE)
RR <- merge(RR,US         ,all=TRUE)

RR$debtgdp <- RR$debtgdp

setwd("I:/Evan/Documents/Umass/Econ 753/Assignments/Assignment 1/WP322HAP-RR-GITD-code")



RR <- within(RR, debtgdp <- ifelse(Country=="Australia",ifelse(Year<=1948,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Austria",ifelse(Year<=1979,100*Debt1/GDP1,100*Debt2/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Belgium",ifelse(Year<=1979,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Canada",ifelse(Year<=1948,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Denmark",ifelse(Year<=1949,100*Debt1/GDP1,100*Debt1/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Finland",ifelse(Year<=1977,100*Debt1/GDP1,100*Debt2/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="France",ifelse(Year<=1948, 100*Debt1 / GDP1, ifelse(Year<=1978, 100*Debt1 / GDP2,100*Debt2/GDP2)),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Germany",ifelse(Year<=1950,100*Debt1/GDP1,100*Debt2/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Greece",ifelse((Year>=1884 & Year<=1913) | (Year>=1919 & Year<=1939) | (Year>=1970 & Year<=1992),100*Debt/GDP1, ifelse(Year==2009,100,debtgdp)),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Ireland",ifelse(Year<=2010,100*Debt/GDP2,debtgdp),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Italy",ifelse(Year<=1913,100*Debt/GDP1,ifelse(Year<=1946,100*Debt/GNI,ifelse(Year<=1998,100*Debt/GDP1,100*Debt/GDP2))),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Japan",ifelse(Year<=1953,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Netherlands",ifelse(Year<=1956,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="New Zealand",ifelse(Year<=1947,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Norway",ifelse(Year<=1948,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Portugal",ifelse(Year<=1999,100*Debt1/GDP1,100*Debt2/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Spain",ifelse(Year<=1957,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="Sweden",ifelse(Year<=1949,100*Debt/GDP1,100*Debt/GDP2),debtgdp))
RR <- within(RR, debtgdp <- ifelse(Country=="UK" , 100*Debt/GDP, debtgdp ))
RR <- within(RR, debtgdp <- ifelse(Country=="US" , 100*Debt/GDP, debtgdp ))

RR$RGDP <- as.numeric(RR$RGDP)
RR$RGDP1 <- as.numeric(RR$RGDP1)
RR$RGDP2 <- as.numeric(RR$RGDP2)

lg<-function(x)c(NA,x[1:(length(x)-1)])
RR <- ddply( RR, ~Country, transform, lRGDP=lg(RGDP), lRGDP1=lg(RGDP1), lRGDP2=lg(RGDP2)  )
RR <- within(RR, dRGDP <- ifelse( !is.na(dRGDP), dRGDP,
                                   ifelse( !is.na( RGDP / lRGDP - 1 ), 100*(RGDP / lRGDP - 1) ,
                                          ifelse( !is.na( RGDP2 / lRGDP2 - 1 ), 100*(RGDP2 / lRGDP2 - 1) ,
                                                 ifelse( !is.na( RGDP1 / lRGDP1 - 1 ), 100*(RGDP1 / lRGDP1 - 1),dRGDP )))))


RR <- subset(RR,Year>=1946 & Year<=2009 & !is.na(dRGDP) & !is.na(debtgdp))
## Italy has another data series through 1946 and is excluded from GITD until 1951
RR <- subset(RR, !(Year<1951 & Country=="Italy"))


RR$dgcat.lm <- cut(RR$debtgdp, breaks=c(0,30,60,90,Inf))
RR$dgcat <- factor(RR$dgcat.lm, labels = c("0-30%","30-60%","60-90%","Above 90%"),ordered=TRUE)

RR$dgcat2.lm <- cut(RR$debtgdp, breaks=c(0,30,60,90,120,Inf))
RR$dgcat2 <- factor(RR$dgcat2.lm, labels = c("0-30%","30-60%","60-90%","90-120%","Above 120%"),ordered=TRUE)

## Regression analysis
summary(dgcat.lm <- lm(dRGDP ~ dgcat.lm, data=RR))
summary(dgcat2.lm <- lm(dRGDP ~ dgcat2.lm, data=RR))
linearHypothesis(dgcat2.lm, paste( c("dgcat2.lm(30,60]=dgcat2.lm(60,90]", "dgcat2.lm(30,60]=dgcat2.lm(90,120]", "dgcat2.lm(30,60]=dgcat2.lm(120,Inf]") ))
linearHypothesis(dgcat2.lm, paste( c("dgcat2.lm(30,60]=dgcat2.lm(60,90]", "dgcat2.lm(30,60]=dgcat2.lm(90,120]")))


## Country-Year average by debtgdp ("correct weights")
## Table 3 Corrected
(RR.correct.mean <- with(RR, tapply( dRGDP, dgcat, mean, na.rm=TRUE )))
RR.correct.mean.df <- data.frame(RR.correct.mean, dgcat=names(RR.correct.mean) )
## Averaged Country averages by debtgdp ("equal weights")
(RR.equalwt.mean <- with(RR, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )))
## Table 3 Country equal weighting
summary(RR.equalwt.mean)

## Country-Year average by debtgdp ("correct weights") expanded categories
(RR.correct.mean.2 <- with(RR, tapply( dRGDP, dgcat2, mean, na.rm=TRUE )))
RR.correct.mean.2.df <- data.frame(RR.correct.mean.2, dgcat=names(RR.correct.mean.2) )
## Averaged Country averages by debtgdp ("equal weights")
(RR.ex.equalwt.mean <- with(RR, tapply( dRGDP, list(Country,dgcat2), mean, na.rm=TRUE )))
summary(RR.ex.equalwt.mean)


## Selective treatment of early years
RR.selective <- subset(RR,
                       !((Year<1950 & Country=="New Zealand") | (Year<1951 & Country=="Australia") | (Year<1951 & Country=="Canada") ))
(RR.selective.mean <- with(RR.selective, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )))
## Equal weights
## Table 3 Weights,Exclusion
summary(RR.selective.mean)
## Correct weights
## Table 3 Selective years exclusion
with(RR.selective, tapply( dRGDP, dgcat, mean, na.rm=TRUE ))

## And dropping because of spreadsheet error
RR.selective.spreadsheet <- subset(RR.selective, ! Country %in% c("Australia","Austria","Belgium","Canada","Denmark") )
RR.selective.spreadsheet.transcription <- with(RR.selective.spreadsheet, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE ))
## Equal weights
## Table 3 Weights,Exclusion,Spreadsheet Error
summary(RR.selective.spreadsheet.transcription)
## Correct weights
## Table 3 Exclusion,Spreadsheet Error
with(RR.selective.spreadsheet, tapply( dRGDP, dgcat, mean, na.rm=TRUE ))

## And New Zealand transcription error
## selective.spreadsheet.transcription <- with(RR.selective.spreadsheet, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE ))
RR.selective.spreadsheet.transcription["New Zealand",4] <- -7.9
summary(RR.selective.spreadsheet.transcription)
## Table 3 Weights,Exclusion,Spreadsheet Error,Transcription
(RR.published.mean <- apply(RR.selective.spreadsheet.transcription,2,mean,na.rm=TRUE))
RR.published.mean.df <- data.frame(RR.published.mean , dgcat=names(RR.published.mean) )


## Medians
(RR.correct.median <- with(RR, tapply( dRGDP, dgcat, median, na.rm=TRUE )))
(RR.eqweight.median <- summary(RR.equalwt.mean))
(RR.correct.ex.median <- with(RR, tapply( dRGDP, dgcat2, median, na.rm=TRUE )))
(RR.selective.spreadsheet.median <- with(RR.selective.spreadsheet, tapply( dRGDP, dgcat, median, na.rm=TRUE )))
(RR.published.median <- apply(RR.selective.spreadsheet.transcription,2,median,na.rm=TRUE))
RR.published.median.df <- data.frame(RR.published.median , dgcat=names(RR.published.median) )



###############################################
##            Figure 2 of RR                 ##
## Evan code: put together means and medians ##
###############################################

RR.published.figure2 <- RR.published.mean.df
colnames(RR.published.figure2) <- c("RR.published.median", "dgcat")
RR.published.figure2 <- rbind(RR.published.figure2, RR.published.median.df)
RR.published.figure2 <- cbind(RR.published.figure2, c("Mean", "Mean", "Mean", "Mean", 
                                                      "Median", "Median", "Median", "Median"))
colnames(RR.published.figure2) <- c("GDPGrowth", "dgcat", "stat")

dev.off()
ggplot(data=RR.published.figure2, aes(x=dgcat, xname="Debt Category", y=GDPGrowth, fill=stat)) + 
  geom_bar(stat="identity", width=0.5, color="black", position=position_dodge()) +
  labs(title="GDP Growth by Debt Category", x="Debt Category", y="GDP Growth") + theme_minimal()




###############################################
##       Prevalence of debt categories       ##
##    Evan Code: Create GDP Growth Category  ##
##               Create tables               ##
###############################################


## Counts of years
with(RR, table(Country,dgcat))
apply(with(RR,table( Country,dgcat)),2,sum)

## Table showing countries and number of years in debt-to-GDP category
dev.off()
grid.table(with(RR, table(Country,dgcat)))

## Create GDP Growth Categories
RR$drgdpcat.lm <- cut(RR$dRGDP, breaks=c(-Inf,-2,0,2,4,Inf))
RR$drgdpcat <- factor(RR$drgdpcat.lm, labels = c("Below -2%","-2-0%","0-2%","2-4%","Above 4%"),ordered=TRUE)

## Table showing countries and number of years in GDP Growth category
dev.off()
grid.table(with(RR, table(Country,drgdpcat)))

## Combine years
RR$yearcat.lm <- cut(RR$Year, breaks=c(1945,1950,1960,1970,1980,1990,2000,2010))
RR$yearcat <- factor(RR$yearcat.lm, labels = c("1946-1950","1951-1960","1961-1970","1971-1980",
                                               "1981-1990","1991-2000","2000-2010"),ordered=TRUE)

## Table showing years and number of countries in debt-to-GD category
dev.off()
grid.table(with(RR, table(yearcat,dgcat)))

## Table showing years and number of countries in debt-to-GD category
dev.off()
grid.table(with(RR, table(yearcat,drgdpcat)))



###############################################
##        Figure 1 of Herndon et al          ##
###############################################


RR.newzealand.1951 <- subset(RR.selective.spreadsheet,Country=="New Zealand" & Year==1951)

## Categorical scatterplot
n <- ggplot(RR, aes(x=dgcat,y=dRGDP)) + geom_point(shape=3,color='lightblue') + ylab("Real GDP Growth") + xlab("Public Debt/GDP Category")
n <- n + geom_point(RR.published.mean.df, mapping=aes(x=dgcat,y=RR.published.mean), shape=5,  size=5 ) 
n <- n + geom_text(RR.published.mean.df, mapping=aes(x=dgcat,y=RR.published.mean,label=round(RR.published.mean,1)),hjust=-0.7,size=3,color='darkgray')
n <- n + geom_point(RR.correct.mean.df,  mapping=aes(x=dgcat,y=RR.correct.mean,label=RR.correct.mean), shape=16, size=4 )  + theme_bw()
n <- n + geom_text(RR.correct.mean.df,  mapping=aes(x=dgcat,y=RR.correct.mean,label=round(RR.correct.mean,1)), hjust=1.7,size=3,color='darkgray')
n <- n + geom_point(RR.newzealand.1951,mapping=aes(x=dgcat,y=dRGDP), shape=0, size=3 )
n <- n + geom_text(RR.newzealand.1951,mapping=aes(x=dgcat,y=dRGDP,label=paste(round(dRGDP,1))), hjust=-0.7,size=3,color='darkgray')
n <- n + geom_text(RR.newzealand.1951,mapping=aes(x=dgcat,y=dRGDP,label=paste("NZ",Year)), hjust=1.2,size=3,color='darkgray')
print(n)

## Create legend for categorical scatterplot
plot(3,10,pch=0,ylim=c(0,70),xlim=c(0,5.5))
text(3.2,10,"New Zealand 1951",adj=0)
points(0,15,pch=16)
text(0.2,15,"Correct average real GDP growth",adj=0)
points(0,10,pch=5,cex=1.5)
text(0.2,10,"RR average real GDP growth",adj=0)
points(3,15,pch=3,col='darkgray')
text(3.2,15,"Country-Year real GDP growth",adj=0)



###############################################
##        Figure 2 of Herndon et al          ##
###############################################


## Expanded categories
o <- ggplot(RR, aes(x=dgcat2,y=dRGDP)) + geom_point(shape=3,color='lightblue') + ylab("Real GDP Growth") + xlab("Public Debt/GDP Category")
o <- o + geom_point(RR.correct.mean.2.df,  mapping=aes(x=dgcat,y=RR.correct.mean.2), shape=16, size=4 )  + theme_bw()
o <- o + geom_text(RR.correct.mean.2.df, mapping=aes(x=dgcat,y=RR.correct.mean.2,label=round(RR.correct.mean.2,1)), hjust=1.7, size=3,color='darkgray')
print(o)



###############################################
##        Figure 4 of Herndon et al          ##
###############################################


## Scatterplot
library(mgcv)
RR.gam <- gam(dRGDP ~ s(debtgdp, bs="cs"),data=RR)

## Cross-validation technique for loess parameters
## http://stats.stackexchange.com/questions/2002/how-do-i-decide-what-span-to-use-in-loess-regression-in-r
m <- ggplot(RR, aes(x=debtgdp,y=dRGDP))
m1 <- m + geom_vline(xintercept=90,color='blue',size=1.5)
m1 <- m1 + geom_point(color='lightblue') + ylab("Real GDP Growth") + xlab("Public Debt/GDP Ratio") + scale_x_continuous(breaks=seq(0,240,30)) + theme_bw()
m1 <- m1 + geom_smooth(method=gam, color='darkblue',formula= y ~ s(x, bs = "cs"))
print(m1)



###############################################
##                Extra Visuals              ##
##                Evan's Code                ##
###############################################


country1 <- "US"
coeff <- 10

evanPlot <- ggplot(RR, aes(x=Year)) + geom_line(aes(y=dRGDP), data=subset(RR,Country==country1), 
                                                color="blue", size=1) + 
  geom_line(aes(y=debtgdp/coeff), data=subset(RR,Country==country1), 
            color="green", size=1) +
  scale_y_continuous(name="GDP Growth Rate", sec.axis=sec_axis(~.*coeff, name="Debt-to-GDP Ratio")) +
  theme_bw() 

dev.off()
print(evanPlot)

country1 <- "UK"
coeff <- 20

evanPlot <- ggplot(RR, aes(x=Year)) + geom_line(aes(y=dRGDP), data=subset(RR,Country==country1), 
                                                color="blue", size=1) + 
  geom_line(aes(y=debtgdp/coeff), data=subset(RR,Country==country1), 
            color="green", size=1) +
  scale_y_continuous(name="GDP Growth Rate", sec.axis=sec_axis(~.*coeff, name="Debt-to-GDP Ratio")) +
  theme_bw() 

dev.off()
print(evanPlot)

country1 <- "Belgium"
coeff <- 10

evanPlot <- ggplot(RR, aes(x=Year)) + geom_line(aes(y=dRGDP), data=subset(RR,Country==country1), 
                                                color="blue", size=1) + 
  geom_line(aes(y=debtgdp/coeff), data=subset(RR,Country==country1), 
            color="green", size=1) +
  scale_y_continuous(name="GDP Growth Rate", sec.axis=sec_axis(~.*coeff, name="Debt-to-GDP Ratio")) +
  theme_bw() 

dev.off()
print(evanPlot)

country1 <- "Greece"
coeff <- 10

evanPlot <- ggplot(RR, aes(x=Year)) + geom_line(aes(y=dRGDP), data=subset(RR,Country==country1), 
                                                color="blue", size=1) + 
  geom_line(aes(y=debtgdp/coeff), data=subset(RR,Country==country1), 
            color="green", size=1) +
  scale_y_continuous(name="GDP Growth Rate", sec.axis=sec_axis(~.*coeff, name="Debt-to-GDP Ratio")) +
  theme_bw() 

dev.off()
print(evanPlot)

country1 <- "Italy"
coeff <- 10

evanPlot <- ggplot(RR, aes(x=Year)) + geom_line(aes(y=dRGDP), data=subset(RR,Country==country1), 
                                                color="blue", size=1) + 
  geom_line(aes(y=debtgdp/coeff), data=subset(RR,Country==country1), 
            color="green", size=1) +
  scale_y_continuous(name="GDP Growth Rate", sec.axis=sec_axis(~.*coeff, name="Debt-to-GDP Ratio")) +
  theme_bw() 

dev.off()
print(evanPlot)
