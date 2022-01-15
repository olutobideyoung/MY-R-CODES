sti<-read.csv("sti.csv", header=T)
library(tidyverse)
library(caret)
summary(sti)
model <- glm( status ~.,data = sti, family = binomial)
summary(model) ##Model coefficients tests
confint(model)  ##Confidence interval
summary(resid(model)) ##Deviance Residuals
probabilities <- model %>% predict(sti, type = "response")
round(probabilities, digits = 3)
summary(probabilities)
predicted.classes <- ifelse(probabilities > 0.5, "positive", "negative")
predicted.classes
chisq.test(sti$sex, sti$status) #Chi-square test for sex and status
chisq.test(sti$age, sti$status) #Chi-square test for age and status
chisq.test(sti$dysuria, sti$status) #Chi-square test for dysuria and status
chisq.test(sti$peniel_itching, sti$status) #Chi-square test for peniel itching and status
chisq.test(sti$vagina_discharge, sti$status) #Chi-square test for vaginal discharge and status
chisq.test(sti$vaginal_itching, sti$status) #Chi-square test for vaginal itching and status
chisq.test(sti$vaginitis, sti$status) #Chi-square test for vaginitis and status
chisq.test(sti$foul.smelling, sti$status) #Chi-square test for foul smelling and status