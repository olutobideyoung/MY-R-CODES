library(tseries)
olaide<-read.csv("olaide.csv",header= T)
olaide$infr
head(olaide)
plot(olaide)
plot(olaide$infr)
plot(olaide$exr)
summary(olaide)
ols<-lm(infr~exr, data=olaide)
summary(ols)
plot(fitted(ols),resid(ols), xlab="Fitted values", ylab = "Residuals")
library(car)
durbinWatsonTest(ols)
library(lmtest)
bptest(ols)
wt<-1/lm(abs(ols$residuals)~ols$fitted.values)$fitted.values^2
wt
wls_model<-lm(infr~exr, data=olaide, weights=wt)
summary(wls_model)
durbinWatsonTest(wls_model)
bptest(wls_model)
wt2<-1/lm(abs(wls_model$residuals)~wls_model$fitted.values)$fitted.values^2
wt2
wls_model2<-lm(infr~exr, data=olaide, weights=wt2)
summary(wls_model2)
durbinWatsonTest(wls_model2)
bptest(wls_model2)
