mydata=read.csv("victor_data.csv",header=T,na.strings="?");names(mydata)
attach(mydata)
mydata
dim(mydata);par(mfrow=c(1,1))
summary(mydata)
sqrt(var(mydata$pcs))
pairs(mydata)
my_train <- mydata[1:31,]
my_test <- mydata[32:39,]; my_test
lm.fit=lm(rgdp~., data=my_train)
summary(lm.fit)
library(car)
library(caret)
library(tidyverse)
vif(lm.fit)
plot(lm.fit)
##MODEL PERFORMANCE
ols.predict = lm.fit%>%predict(my_test)
ols.predict
plot(ols.predict)
data.frame(OLS_RMSEprediction=RMSE(ols.predict,my_test$rgdp),R2=R2(ols.predict,my_test$rgdp))

###PARTIAL LEAST SQUARE REGRESSION MODELING
library(pls)
library(plsdepot)
plsrfit <- plsr(rgdp~., data = my_train, validation = "LOO")
summary(plsrfit)
plot(RMSEP(plsrfit), legendpos = "topright")
explvar(plsrfit) #EXPLAINED VARIANCES
##NO. OF COMPONENT SELECTION USING ONESIGMA
ncomp<- selectNcomp(plsrfit, method = "onesigma", plot = TRUE,ylim = c(.18, .6));ncomp
plsrfit2 <- plsr(rgdp~., ncomp=3, data = my_train, validation = "LOO")
summary(plsrfit2)
plsrfit2$raw.wgs
plot(plsrfit2)
sc <- scores(plsrfit2);sc; plot(sc)
ld <- loadings(plsrfit2);ld; plot(ld)
Ysc <-Yscores(plsrfit2);Ysc; plot(Ysc)
Yld <- Yloadings(plsrfit2);Yld;

plot(plsrfit2, ncomp = 3, asp = 1, line = TRUE) #PREDICTION PLOTS FROM NUMBER OF COMPONENTS TAKEN
plot(plsrfit2, plottype = "scores", comps = 1:3)
plot(plsrfit2, "loadings", comps = 1:3, 
     legendpos = "topleft", xlab = "number of components")

plot(plsrfit2, "correlation", comps=1:3, legendpos = "topright", xlab = "number of components")
plot(plsrfit2, "coefficients", comps = 1:3, 
     legendpos = "topleft", xlab = "number of components")
plot(plsrfit2, "biplot", comps = 1:2)
coefficients=coef(plsrfit2);coefficients
sum.coef=sum(sapply(coefficients, abs))
coefficients=coefficients*100/sum.coef; coefficients ##Normalized coefficients
predict(plsrfit, comps = 3, newdata = my_test)#PREDICTION
predplot(plsrfit, ncomp = 3, newdata = my_test, asp = 1, line = TRUE)
RMSEP(plsrfit2, newdata=my_test)
weights(plsrfit2, newdata=my_test)$raw.wgs
R2=R2(plsrfit2, newdata = my_test);R2
