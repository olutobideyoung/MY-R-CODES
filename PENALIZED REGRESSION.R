## R codes on shrinkage methods for rainfall radiation model over Sokoto

rainfall=read.csv("rainfall.csv",header=T,na.strings="?");names(rainfall);attach(rainfall)
dim(rainfall);par(mfrow=c(1,1))
summary(rainfall);write.csv(summary(rainfall),"rainfall_Over_PH.csv")
fix(rainfall)

## Multiple Linear Regression
trainingRowIndex<-sample(1:row(rainfall), nrow(rainfall)/2)
trainingRowIndex
data.train<-rainfall[trainingRowIndex,]
data.train
dim(data.train)
data.test<-rainfall[-trainingRowIndex,]
data.test
dim(data.test)
library(car)
lm.fit=lm(rl~., data = data.train)
summary(lm.fit)
ols.predict = predict.lm(lm.fit,data=data.test); write.csv(predict.lm(lm.fit,data=data.test),"rainfall_PH.csv")
ols.predict
vif(lm.fit)
AIC(lm.fit)
plot(predict.lm(lm.fit, data.test))
plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

## Ridge Regression

x=model.matrix(rl~.,rainfall)[,-1]
y=rainfall$rl
library(glmnet)
grid.rl=10^seq(10,-2,length=100)
ridge.rl=glmnet(x,y,alpha=0,lambda=grid.rl)
ridge.rl
dim(coef(ridge.rl))
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.rl=glmnet(x[train,],y[train],alpha=0,
                lambda=grid,thresh=1e-12)##coefficient when lambda=ridge.rainfall$lambda which expected 
##to be smaller for large values of lambda
sqrt(sum(coef(ridge.rl)^2))
ridge.rl
plot(ridge.rl)
set.seed(1)
cv.out.rl=cv.glmnet(x[train,],y[train],alpha=0)
cv.out.rl
plot(cv.out.rl)
bestlam.rl=cv.out.rl$lambda.min
bestlam.rl
ridge.pred.rl=predict(ridge.rl,s=bestlam.rl,newx=x[test,])
ridge.pred.rl
mean((ridge.pred.rl-y.test)^2)
out.rl=glmnet(x,y,alpha=0)
out.rl
predict(out.rl,type="coefficients",s=bestlam.rl)[1:7,]

## The Lasso

lasso.rl=glmnet(x[train,],y[train],alpha=1,lambda=grid.rl)
lasso.rl
sqrt(sum(abs(coef(lasso.rl))))
plot(lasso.rl)
set.seed(1)
cv.out.rl=cv.glmnet(x[train,],y[train],alpha=1)
cv.out.rl
plot(cv.out.rl) 
bestlam.rl=cv.out.rl$lambda.min
bestlam.rl
lasso.pred.rl=predict(lasso.rl,s=bestlam.rl,newx=x[test,])
lasso.pred.rl
mean((lasso.pred.rl-y.test)^2)
out.rl=glmnet(x,y,alpha=1,lambda=grid.rl)
out.rl
lasso.coef.rl=predict(out.rl,type="coefficients",s=bestlam.rl)[1:7,]
lasso.coef.rl
lasso.coef.rl[lasso.coef.rl!=0]
lasso.coef.rl[lasso.coef.rl==0]

