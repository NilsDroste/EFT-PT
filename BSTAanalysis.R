#######################################################################################
##Bayesian Structural Time Series Analysis of Decentralization Effects in Portugues EFT
##script authour: nils.droste@ufz.de
#######################################################################################

# 0 Info on variables #################################################################

# all data on Portugal

# year	<- year
# PAsum.nat	<- sum of national PA area
# PAsum.mun <- sum of municipal PA area
# PAratio	<- ratio of municipal and national PA area
# PAno.nat <- number of national PA area
# PAno.mun <- number of municpal PA area	
# PAno.mun2	<- number of municpal PA area in Portugal without those only possible with 2008 conservation law reform
# PAratio.no <- ratio of municipal and national PA numbers in Portugal
# PAratio.no2 <- ratio of municipal and national PA numbers /wo 2008 PA designations in Portugal
# VAagr	<- value added by agricultural sector in constant â¬ 2005 prices
# VAind	<- value added by industry sector in constant â¬ 2005 prices
# VAser	<- value added by service sector in constant â¬ 2005 prices
# GDPcap <- GDP per capita in constant â¬ 2005 prices
# POPdens	<- population density
# ENVexp <- municipal spending related to the environment
# ENVinc <- municipal income related to the environment 
# BIODIVexp <- municipal spending related to the biodiversity
# BIODIVinc	<- municipal income related to the biodiversity
# ENGOmem <- members of environmental NGO per 1,000 inhabitants

# 1 read and prepare data ##############################################################

# set your working directory
# setwd()

library(zoo)

df <- read.csv("TimeSeriesPortugal.csv", sep=",", dec=".")

pre.period <- c(1995,2007)
post.period <- c(2008,2014)
data<- zoo(cbind(PAratio.no=df$PAratio.no,VAagr=df$VAagr,VAind=df$VAind,VAser=df$VAser,GDPcap=df$GDPcap,POPdens=df$POPdens, ENGOmem=df$ENGOmem,ENVexp=df$ENVexp, ENVinc=df$ENVinc),df$year)

# create summary table

require(stargazer)

df.sum<-cbind(df$PAratio.no,df$VAagr,df$VAind,df$VAser,df$GDPcap,df$POPdens,df$ENVexp,df$ENVinc,df$ENGOmem)

stargazer(df.sum, summary=T, type = "html", title="Descriptive statistics", out="summary.html",covariate.labels=c("ratio municipal PA / national PA (PAratio)", "valued added by agriculture (VAagr)","valued added by industry (VAind)","valued added by service (VAser)", "GDP per capita (GDPcap)","population density (POPdens)","municipal environmental spending (ENVexp)","municipal environmental income (ENVinc))", "environmental NGO members per 1,000 inhabitants (ENGOmem)"), flip=F, notes = "Sources: author's calculations based on ICNF (2015), World Bank (2015), and INE (2015); monetary values are in constant € 2005 prices.")

# 2 Bayesian Structural Time Series Analysis ###########################################

library(CausalImpact)

# run model
set.seed(2468)

impact <- CausalImpact(data, pre.period, post.period)


# plot model

plot(impact)


# plot variable importance

plot(impact$model$bsts.model, "coefficients", inclusion.threshold = 0)

  # probability of positive sign
coefplot <- plot(impact$model$bsts.model, "coefficients", inclusion.threshold = 0)
round(plot1$positive.prob, 2)


# obtain a summary

summary(impact)
summary(impact, "report")

# 3 Robustness Check ###########################################

# 3.1 excluding municipal PAs only possible due to Nature Conservation Law / Competency reform 2008
data2<-zoo(cbind(PAratio.no=df$PAratio.no2,VAagr=df$VAagr,VAind=df$VAind,VAser=df$VAser,GDPcap=df$GDPcap,POPdens=df$POPdens, ENGOmem=df$ENGOmem,ENVexp=df$ENVexp, ENVinc=df$ENVinc),df$year)

set.seed(2468)
impact2 <- CausalImpact(data2, pre.period, post.period)

plot(impact2)

summary(impact2)

plot(impact2$model$bsts.model, "coefficients", inclusion.threshold = 0)


# 3.2 Analysis on area instead of number of PAs

## 3.2.1 All areas 

data3<-zoo(cbind(PAratio=df$PAratio,VAagr=df$VAagr,VAind=df$VAind,VAser=df$VAser,GDPcap=df$GDPcap,POPdens=df$POPdens, ENGOmem=df$ENGOmem,ENVexp=df$ENVexp, ENVinc=df$ENVinc),df$year)

set.seed(2468)
impact3 <- CausalImpact(data3, pre.period, post.period)

plot(impact3)

summary(impact3)

plot(impact2$model$bsts.model, "coefficients", inclusion.threshold = 0)

# Area data, excluding municipal PAs only possible due to Nature Conservation Law / Competency reform 2008

data4<-zoo(cbind(PAratio2=df$PAratio2,VAagr=df$VAagr,VAind=df$VAind,VAser=df$VAser,GDPcap=df$GDPcap,POPdens=df$POPdens, ENGOmem=df$ENGOmem,ENVexp=df$ENVexp, ENVinc=df$ENVinc),df$year) 

set.seed(2468)
impact4 <- CausalImpact(data4, pre.period, post.period)

summary(impact4)

plot(impact4)
