library(ISLR)
library(MASS)
library(splines)
library(locfit)
library(gam)
ola=read.csv("ola_data.csv",header=T,na.strings="?")
names(ola)
summary(ola)
library(moments)
skewness((ola))
kurtosis(ola)
library("car")
plot(gdp~exr, data=ola)
plot(gdp~infr, data=ola)
sd(ola$gdp);sd(ola$exr);sd(ola$infr)
#i fit a generalized additive model GAM to predict
#exchange rate using the natural spline functions of the 
#crude oil price and inflation rate with interaction effects 
#choice of basis functions, i simply used the lm() function as follows:

attach(ola)
##Natural Splines Model
gam1=lm(log(gdp)~ns(log(exr),2)+ns(infr,5)+log(exr):infr)
summary(gam1)
library(ggplot2)
#the genexric plot() function recognizes that gam2 is an object of class gam, and invokes
#the appropriate plot.gam() method. Even though gam1 is not of class gam but rather class lm
#we can still use plot.gam() on it.
plot.Gam(gam1,se=T,col="red",lwd=3)
anova(gam1)

####SMOOTHING SPLINES MODEL
###Checking for optimal splines 
require(graphics)
spl.exr <- with(ola, smooth.spline(log(gdp), log(exr)));spl.exr
spl.infr <- with(ola, smooth.spline(log(gdp), infr));spl.infr
library(gam)
gam.m3=gam(log(gdp)~s(log(exr))+s(infr)+log(exr):infr)
summary(gam.m3)
gam.m3$coef
plot(gam.m3,se=TRUE,col="blue",lwd=3)

#notice i had to use plot.gam() rather than the generic plot() function. in these plots
#the function of year looks linear. I performed a series of ANOVA tests in ordexr to detexrmine
#which of these three models is best:
#a GAM that excludes the interaction effect (M2), or 
#a GAM that includes the interaction of crude oil prices and inflation rates M3

gam.m2=gam(log(gdp)~s(log(exr))+s(infr))##WITHOUT ITERACTION EFFECTS
summary(gam.m2)
gam.m2$coef
plot(gam.m2,se=TRUE,col="yellow",lwd=3)
anova(gam.m2,gam.m3,test="F")
preds1=predict(gam.m2,newdata=ola)
preds2=predict(gam.m3,newdata=ola)
par(mfrow=c(1,2))
plot(preds1,  main="GAM pred with no interaction effect",col="blue",xlab ="Month")
plot(preds2,  main="GAM pred with interaction effect", col="blue", xlab ="Month")

###LOCAL REGRESSION
library(splines)
library(foreach)
library(gam)
gam.lo.i=gam(log(gdp)~lo(log(exr),infr,span=0.7)+log(exr):infr,data=ola)
summary(gam.lo.i)
gam.lo.i$coefficients
library(akima)
par(mfrow=c(1,2))
plot.Gam(gam.lo.i,se=T,col="blue",lwd=1)
plot(predict(gam.lo.i, newdata=ola),col="blue", xlab ="months")
#End!!!!!
