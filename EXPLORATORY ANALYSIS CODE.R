library(foreign)
library(ggplot2)# Package for nice visualiztion
# Loading data into R
w_africans<-read.csv("social.csv",header=T);w_africans
#Displying the first 15 observations of the w_africans dataset
print(head(w_africans, n=15))
str(w_africans) #Data structure
#Dataset variable names can be viewed using names(dataset) or ls(dataset)
ls(w_africans)
#Viewing the number of rows and columns in the w_africans dataset; use ncol(dataset) and nrow(dataset)
ncol(w_africans); nrow(w_africans)
#Calculate the mean of variable with mean(DATAVAR$VAR)
mean(w_africans$GDPPC_PPP, na.rm=T)
mean(w_africans$GNIPC_PPP, na.rm=T)
mean(w_africans$ER, na.rm=T)
sd(w_africans$GDPPC_PPP, na.rm=TRUE)#Standard deviation of GDPPC_PPP
sd(w_africans$GNIPC_PPP, na.rm=TRUE)#Standard deviation of GNIPC_PPP
sd(w_africans$ER, na.rm=TRUE)#Standard deviation of ER
#Minimum and maximum GDPPC_PPP of the selected w_african countries
min(w_africans$GDPPC_PPP, na.rm = TRUE); max(w_africans$GDPPC_PPP, na.rm = TRUE)
#Minimum and maximum GNIPC_PPP of the selected w_african countries
min(w_africans$GNIPC_PPP, na.rm = TRUE); max(w_africans$GNIPC_PPP, na.rm = TRUE)
#Minimum and maximum ER of the selected w_african countries
min(w_africans$ER, na.rm = TRUE); max(w_africans$ER, na.rm = TRUE)
#Calculate the range of a variable with range(VAR)
range(w_africans$GDPPC_PPP, na.rm=TRUE)#Range of variable GNDPPC_PPP
range(w_africans$GNIPC_PPP, na.rm=TRUE)#Range of variable GNIPC_PPP
range(w_africans$ER, na.rm=TRUE)#Range of variable ER

#Calculate the 25th, 50th, 75th percentile for GDPPC_PPP
quantile(w_africans$GDPPC_PPP, na.rm=TRUE, prob=c(0.25,0.50, 0.75,0.95))

# Calculate the 25th, 50th, 75th percentile for GDPPC_PPP
quantile(w_africans$GNIPC_PPP, na.rm=TRUE, prob=c(0.25,0.50, 0.75,0.95))

#Calculate the 25th, 50th, 75th percentile for ER
quantile(w_africans$ER, na.rm=TRUE, prob=c(0.25,0.50, 0.75,0.95))

#Summarize the w_africans dataset using the command summary(x)
print(summary(w_africans))
library(moments)
skewness(w_africans$GDPPC_PPP, na.rm = T)
skewness(w_africans$GNIPC_PPP, na.rm = T)
skewness(w_africans$ER, na.rm = T)
kurtosis(w_africans$GDPPC_PPP, na.rm = T)
kurtosis(w_africans$GNIPC_PPP, na.rm = T)
kurtosis(w_africans$ER, na.rm = T)
shapiro.test(w_africans$GDPPC_PPP)
shapiro.test(w_africans$GNIPC_PPP)
shapiro.test(w_africans$ER)
par(mfrow=c(1,1))
qqnorm(w_africans$GDPPC_PPP)
qqline(w_africans$GDPPC_PPP,col="red")
qqnorm(w_africans$GNIPC_PPP)
qqline(w_africans$GNIPC_PPP,col="blue")
qqnorm(w_africans$ER)
qqline(w_africans$ER,col="green")

#EXTRACTION OF EACH WEST AFRICAN COUNTRIES DATA STRUCTURE WITH THEIR AVERAGES AND STANDARD DEVIATION
#BENIN REPUBLIC
benin_d<-print(head(w_africans,n=20));benin_d
benin_d<-w_africans[1:20,];benin_d
mean(benin_d$GDPPC_PPP);sd(benin_d$GDPPC_PPP)
mean(benin_d$GNIPC_PPP);sd(benin_d$GNIPC_PPP)
mean(benin_d$ER);sd(benin_d$ER)
#BURKINA FASO
burkina_faso<-w_africans[21:40,];burkina_faso
mean(burkina_faso$GDPPC_PPP);sd(burkina_faso$GDPPC_PPP)
mean(burkina_faso$GNIPC_PPP);sd(burkina_faso$GNIPC_PPP)
mean(burkina_faso$ER);sd(burkina_faso$ER)
#CAPE VERDE
cape_verde<-w_africans[41:60,];cape_verde
mean(cape_verde$GDPPC_PPP);sd(cape_verde$GDPPC_PPP)
  mean(cape_verde$GNIPC_PPP);sd(cape_verde$GNIPC_PPP)
mean(cape_verde$ER);sd(cape_verde$ER)
#GAMBIA
gambia_d<-w_africans[61:80,];gambia_d
mean(gambia_d$GDPPC_PPP);sd(gambia_d$GDPPC_PPP)
mean(gambia_d$GNIPC_PPP);sd(gambia_d$GNIPC_PPP)
mean(gambia_d$ER);sd(gambia_d$ER)
#GHANA
ghana_d<-w_africans[81:100,];ghana_d
mean(ghana_d$GDPPC_PPP);sd(ghana_d$GDPPC_PPP)
mean(ghana_d$GNIPC_PPP);sd(ghana_d$GNIPC_PPP)
mean(ghana_d$ER);sd(ghana_d$ER)
#GUINEA
guinea_d<-w_africans[101:120,];guinea_d
mean(guinea_d$GDPPC_PPP);sd(guinea_d$GDPPC_PPP)
mean(guinea_d$GNIPC_PPP);sd(guinea_d$GNIPC_PPP)
mean(guinea_d$ER);sd(guinea_d$ER)
#GUINEA BISSAU
guinea_bissau<-w_africans[121:140,];guinea_bissau
mean(guinea_bissau$GDPPC_PPP);sd(guinea_bissau$GDPPC_PPP)
mean(guinea_bissau$GNIPC_PPP);sd(guinea_bissau$GNIPC_PPP)
mean(guinea_bissau$ER);sd(guinea_bissau$ER)
#IVORY COAST
ivory_coast<-w_africans[141:160,];ivory_coast
mean(ivory_coast$GDPPC_PPP);sd(ivory_coast$GDPPC_PPP)
mean(ivory_coast$GNIPC_PPP);sd(ivory_coast$GNIPC_PPP)
mean(ivory_coast$ER);sd(ivory_coast$ER)
#LIBERIA
liberia<-w_africans[161:180,];liberia
mean(liberia$GDPPC_PPP, na.rm = T);sd(liberia$GDPPC_PPP, na.rm = T)
mean(liberia$GNIPC_PPP, na.rm = T);sd(liberia$GNIPC_PPP, na.rm = T)
mean(liberia$ER);sd(liberia$ER)
#MALI
mali<-w_africans[181:200,];mali
mean(mali$GDPPC_PPP);sd(mali$GDPPC_PPP)
mean(mali$GNIPC_PPP);sd(mali$GNIPC_PPP)
mean(mali$ER);sd(mali$ER)
#MAURITANIA
mauritania<-w_africans[201:220,];mauritania
mean(mauritania$GDPPC_PPP);sd(mauritania$GDPPC_PPP)
mean(mauritania$GNIPC_PPP);sd(mauritania$GNIPC_PPP)
mean(mauritania$ER);sd(mauritania$ER)
#NIGER REPUBLIC
niger<-w_africans[221:240,];niger
mean(niger$GDPPC_PPP);sd(niger$GDPPC_PPP)
mean(niger$GNIPC_PPP);sd(niger$GNIPC_PPP)
mean(niger$ER);sd(niger$ER)
#NIGERIA
nigeria<-w_africans[241:260,];nigeria
mean(nigeria$GDPPC_PPP);sd(nigeria$GDPPC_PPP)
mean(nigeria$GNIPC_PPP);sd(nigeria$GNIPC_PPP)
mean(nigeria$ER);sd(nigeria$ER)
#SENEGAL
senegal<-w_africans[261:280,];senegal
mean(senegal$GDPPC_PPP);sd(senegal$GDPPC_PPP)
mean(senegal$GNIPC_PPP);sd(senegal$GNIPC_PPP)
mean(senegal$ER);sd(senegal$ER)
#SIERRA LEONE
sierra_leone<-w_africans[281:300,];sierra_leone
mean(sierra_leone$GDPPC_PPP);sd(sierra_leone$GDPPC_PPP)
mean(sierra_leone$GNIPC_PPP);sd(sierra_leone$GNIPC_PPP)
mean(sierra_leone$ER);sd(sierra_leone$ER)
#TOGO
togo_d<-print(tail(w_africans,n=20));togo_d
mean(togo_d$GDPPC_PPP);sd(guinea_d$GDPPC_PPP)
mean(togo_d$GNIPC_PPP);sd(guinea_d$GNIPC_PPP)
mean(togo_d$ER);sd(togo_d$ER)

#average percentage pie chart of variable GDPPC_PPP
average<-c(6391890886,7936971004,1357911696,23767608121,960513188,30520310962,6160408459,802169449,1979172684,9422871920,3532041834,5144264215,292211877711,14361291315,2601392699,3164636979)
percent<-round((average/sum(average)*100),1)
countries=c("BJ", "BF", "CV","CI", "GM", "GH","GN", "GW","LR","ML","MR","NE", "NG","SN","SL","TG") #Country names
pie=pie(average,percent,col = rainbow(length(average),rev = TRUE))
legend("topright",cex=0.6, countries,fill=rainbow(length(average))) #FOR KEY
#DATA EXPLORATION USING ExPanDaR PACKAGE
library(ExPanDaR)
ExPanD(df=w_africans)
