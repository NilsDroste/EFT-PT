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
impact.base <- CausalImpact(data, pre.period, post.period, model.args = list(niter = 10000))

# plot model
plot(impact.base)

# plot variable importance
plot(impact.base$model$bsts.model, "coefficients", inclusion.threshold = 0)

# probability of positive sign
coefplot <- plot(impact.base$model$bsts.model, "coefficients", inclusion.threshold = 0)
round(coefplot$positive.prob, 2)

# obtain a summary
summary(impact.base)
summary(impact.base, "report")

# display confidence intervals of estimates for post-intervention
round(impact$series[c(14,20),c(4,5,10,11,13,14)],3)

# 3 Robustness Check ###########################################

# 3.1 excluding municipal PAs only possible due to Nature Conservation Law / Competency reform 2008
data2<-zoo(cbind(PAratio.no=df$PAratio.no2,VAagr=df$VAagr,VAind=df$VAind,VAser=df$VAser,GDPcap=df$GDPcap,POPdens=df$POPdens, ENGOmem=df$ENGOmem,ENVexp=df$ENVexp, ENVinc=df$ENVinc),df$year)

set.seed(2468)
impact.inst <- CausalImpact(data2, pre.period, post.period, model.args = list(niter = 10000))

plot(impact.inst)

summary(impact.inst)

plot(impact.inst$model$bsts.model, "coefficients", inclusion.threshold = 0)


# 3.2 Analysis on area instead of number of PAs

## 3.2.1 All areas 

data3<-zoo(cbind(PAratio=df$PAratio,VAagr=df$VAagr,VAind=df$VAind,VAser=df$VAser,GDPcap=df$GDPcap,POPdens=df$POPdens, ENGOmem=df$ENGOmem,ENVexp=df$ENVexp, ENVinc=df$ENVinc),df$year)

set.seed(2468)
impact.area <- CausalImpact(data3, pre.period, post.period, model.args = list(niter = 10000))

plot(impact.area)

summary(impact.area)

# 3.2.2. Area data, excluding municipal PAs only possible due to Nature Conservation Law / Competency reform 2008

data4<-zoo(cbind(PAratio2=df$PAratio2,VAagr=df$VAagr,VAind=df$VAind,VAser=df$VAser,GDPcap=df$GDPcap,POPdens=df$POPdens, ENGOmem=df$ENGOmem,ENVexp=df$ENVexp, ENVinc=df$ENVinc),df$year) 

set.seed(2468)
impact.area.inst <- CausalImpact(data4, pre.period, post.period, model.args = list(niter = 10000))

summary(impact.area.inst)

plot(impact.area.inst)


# 3.3 a local trends model specification
require(bsts)
post.period.response <- df$PAratio.no[14 : 20]
PAratio.no <- df$PAratio.no
PAratio.no[14 : 20]<- NA

## 3.3.1 local trends
set.seed(2468)
ss <- AddLocalLinearTrend(list(), PAratio.no)
bsts.model <- bsts(PAratio.no ~ data$VAagr+data$VAind+data$VAser+data$GDPcap+data$POPdens+data$ENGOmem+data$ENVexp+data$ENVinc, ss, niter = 10000)
impact.localTrend <- CausalImpact(bsts.model = bsts.model, post.period.response = post.period.response)
summary(impact.localTrend)
summary(impact.localTrend, "report")

## 3.4.1 semi local trends  
set.seed(2468)
ss2 <- AddSemilocalLinearTrend(list(), PAratio.no)
bsts.model2 <- bsts(PAratio.no ~ data$VAagr+data$VAind+data$VAser+data$GDPcap+data$POPdens+data$ENGOmem+data$ENVexp+data$ENVinc, ss2, niter = 10000)
impact.semiLocalTrend <- CausalImpact(bsts.model = bsts.model2, post.period.response = post.period.response)
summary(impact.semiLocalTrend)
summary(impact.semiLocalTrend, "report")


# 4 Reporting Monte Carlo Standard Errors ###########################################

library(plyr)
require(LaplacesDemon)

MonCarlSE <- data.frame(matrix(NA, nrow = 9, ncol = 19))
MonCarlSE <- rename(MonCarlSE, c("X1"="Variables", "X2"="MCSE_mod1", "X3"="SD_mod1", "X4"="MCSE/SD*100_mod1", "X5"="MCSE_mod2", "X6"="SD_mod2", "X7"="MCSE/SD*100_mod2", "X8"="MCSE_mod3", "X9"="SD_mod3", "X10"="MCSE/SD*100_mod3", "X11"="MCSE_mod4", "X12"="SD_mod4", "X13"="MCSE/SD*100_mod4", "X14"="MCSE_mod5", "X15"="SD_mod5", "X16"="MCSE/SD*100_mod5", "X17"="MCSE_mod6", "X18"="SD_mod6", "X19"="MCSE/SD*100_mod6"))

MonCarlSE$Variables <- c("intercept", "valued added by agriculture (VAagr)","valued added by industry (VAind)","valued added by service (VAser)", "GDP per capita (GDPcap)","population density (POPdens)","municipal environmental spending (ENVexp)","municipal environmental income (ENVinc))", "environmental NGO members per 1,000 inhabitants (ENGOmem)")

for(i in c(2:19)){
  if(i==2){
    for(j in c(1:9)){
    MonCarlSE[j,i] <- MCSE(impact.base$model$bsts.model$coefficients[,j])
    }}
  if(i==3){
    for(j in c(1:9)){
      MonCarlSE[j,i] <- sd(impact.base$model$bsts.model$coefficients[,j])
    }}
  if(i==4){
    for(j in c(1:9)){
      MonCarlSE[j,i] <- MCSE(impact.base$model$bsts.model$coefficients[,j])/sd(impact$model$bsts.model$coefficients[,j])*100
    }}
  if(i==5){
    for(j in c(1:9)){
      MonCarlSE[j,i] <- MCSE(impact.inst$model$bsts.model$coefficients[,j])
    }}
  if(i==6){
    for(j in c(1:9)){
      MonCarlSE[j,i] <- sd(impact.inst$model$bsts.model$coefficients[,j])
    }}
  if(i==7){
    for(j in c(1:9)){
      MonCarlSE[j,i] <- MCSE(impact.inst$model$bsts.model$coefficients[,j])/sd(impact.inst$model$bsts.model$coefficients[,j])*100
    }}
  if(i==8){
    for(j in c(1:9)){
      MonCarlSE[j,i] <- MCSE(impact.area$model$bsts.model$coefficients[,j])
    }}
  if(i==9){
    for(j in c(1:9)){
      MonCarlSE[j,i] <- sd(impact.area$model$bsts.model$coefficients[,j])
    }}
  if(i==10){
    for(j in c(1:9)){
      MonCarlSE[j,i] <- MCSE(impact.area$model$bsts.model$coefficients[,j])/sd(impact.area$model$bsts.model$coefficients[,j])*100
    }}
  if(i==11){
    for(j in c(1:9)){
      MonCarlSE[j,i] <- MCSE(impact.area.inst$model$bsts.model$coefficients[,j])
    }}
  if(i==12){
    for(j in c(1:9)){
      MonCarlSE[j,i] <- sd(impact.area.inst$model$bsts.model$coefficients[,j])
    }}
  if(i==13){
    for(j in c(1:9)){
      MonCarlSE[j,i] <- MCSE(impact.area.inst$model$bsts.model$coefficients[,j])/sd(impact.area.inst$model$bsts.model$coefficients[,j])*100
    }}
  if(i==14){
    for(j in c(1:9)){
      MonCarlSE[j,i] <- MCSE(impact.localTrend$model$bsts.model$coefficients[,j])
    }}
  if(i==15){
    for(j in c(1:9)){
      MonCarlSE[j,i] <- sd(impact.localTrend$model$bsts.model$coefficients[,j])
    }}
  if(i==16){
    for(j in c(1:9)){
      MonCarlSE[j,i] <- MCSE(impact.localTrend$model$bsts.model$coefficients[,j])/sd(impact.localTrend$model$bsts.model$coefficients[,j])*100
    }}
  if(i==17){
    for(j in c(1:9)){
      MonCarlSE[j,i] <- MCSE(impact.semiLocalTrend$model$bsts.model$coefficients[,j])
    }}
  if(i==18){
    for(j in c(1:9)){
      MonCarlSE[j,i] <- sd(impact.semiLocalTrend$model$bsts.model$coefficients[,j])
    }}
  if(i==19){
    for(j in c(1:9)){
      MonCarlSE[j,i] <- MCSE(impact.semiLocalTrend$model$bsts.model$coefficients[,j])/sd(impact.semiLocalTrend$model$bsts.model$coefficients[,j])*100
    }}
}

