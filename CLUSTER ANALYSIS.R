#####CLUSTER ANALYSIS ON COVID-19 PATTERN OF INFECTION IN NIGERIA####
daily<-read.csv("daily.csv", header = TRUE, sep=",")
daily
library(ggplot2)
Dates<-seq(as.Date("2020-02-27"), as.Date("2021-10-31"), "day")
daily_new<-data.frame(Dates, daily)
min<-as.Date("2020-02-27"); max<-NA
ggplot(data=daily_new, aes(x = Dates, y=confirmed_cases))+geom_line(color="#56B4E9", size=0.8)+labs(title = "Time Plot of Nigeria COVID-19 Daily Confirmed Cases")+scale_x_date(date_labels ="%b/%d")+theme_minimal()
ggplot(data=daily_new, aes(x = Dates, y=recovery_cases))+geom_line(color="green", size=0.8)+labs(title = "Time Plot of Nigeria COVID-19 Daily Recovered Cases")+scale_x_date(date_labels ="%b/%y")+theme_minimal()  ##Time plot with GGPlot
ggplot(data=daily_new, aes(x = Dates, y=death_cases))+geom_line(color="red", size=0.8)+labs(title = "Time Plot of Nigeria COVID-19 Daily Death Cases")+scale_x_date(date_labels ="%b/%y")+theme_minimal()  ##Time plot with GGPlot
###Descriptive Statistics of Daily COVID-19 Cases
summary(daily_new)
library(moments)
skewness(daily_new)
kurtosis(daily_new)
sqrt(var(daily_new)) ##Standard deviation
jarque.test(daily_new$confirmed_cases) ##Normality test of daily confirmed cases
jarque.test(daily_new$recovery_cases) ##Normality test of daily recovered cases
jarque.test(daily_new$death_cases) ##Normality test of daily death cases
states<-read.csv("states.csv", header = TRUE)
states
ggplot(data=states, aes(x=confirmed_cases, y=state))+geom_bar(stat="identity", fill="red")+labs(title = "Nigeria COVID-19 Confirmed Cases by States")+geom_text(aes(label=confirmed_cases), vjust=0.4, size=2.0, color="black")+theme_minimal()
ggplot(data=states, aes(x=recovery_cases, y=state))+geom_bar(stat="identity", fill="green")+labs(title = "Nigeria COVID-19 Recovered Cases by States")+geom_text(aes(label=recovery_cases), vjust=0.4, size=2.0, color="black")+theme_minimal()
ggplot(data=states, aes(x=deaths, y=state))+geom_bar(stat="identity", fill="#999999")+labs(title = "Nigeria COVID-19 Death Cases by States")+geom_text(aes(label=deaths), vjust=0.4, size=2.0, color="red")+theme_minimal()
library(cluster)
library(factoextra)
library(dendextend)
library(tidyverse)
library(dplyr)
library(gridExtra)
require(stats)
#####CLUSTERING PATTERN BY STATES (CONFIRMED CASES) #####
s_conf<-read.csv("s_confirmed.csv", row.names = 1, header = TRUE, sep=",")
head(s_conf)
#Euclidean based distance method
df<-scale(s_conf)
res.dist<-get_dist(df,method="euclidean") #Dissimilarity matrix
head(round(as.matrix(res.dist), 2))[1:37]
#Visualize the dissimilarity matrix
fviz_dist(res.dist, lab_size=6)
hclust(res.dist, method="ward.D2")
plot(hclust(res.dist, method="ward.D2"), cex=.8, col="brown")
fviz_nbclust(df, FUN=hcut, method = "silhouette") #Average Silhouette Method for optimal no. of clusters ()
#Enhanced KM clustering
eclust(df, "kmeans", k=2)
eclust(df, "kmeans", k=3)
fviz_silhouette(eclust(df, "kmeans", k=2)) #Silhouette plot for 2 clusters
fviz_silhouette(eclust(df, "kmeans", k=3)) #Silhouette plot for 3 clusters

#####CLUSTERING PATTERN BY STATES (RECOVERED CASES)#####
s_recovery<-read.csv("s_recovery.csv", row.names = 1, header = TRUE, sep=",")
#Euclidean based distance method
head(s_recovery)
#Euclidean based distance method
df1<-scale(s_recovery)
res.dist1<-get_dist(df1,method="euclidean") #Dissimilarity matrix
head(round(as.matrix(res.dist1), 2))[1:37]
#Visualize the dissimilarity matrix
fviz_dist(res.dist1, lab_size=6)
hclust(res.dist1, method="ward.D2")
plot(hclust(res.dist1, method="ward.D2"), cex=.8, col="green") #Dendogram
fviz_nbclust(df1, FUN=hcut, method = "silhouette") #Average Silhouette Method for optimal no. of clusters ()
#Enhanced KM clustering
eclust(df1, "kmeans", k=2)
eclust(df1, "kmeans", k=3)
fviz_silhouette(eclust(df1, "kmeans", k=2)) #Silhouette plot for 2 clusters
fviz_silhouette(eclust(df1, "kmeans", k=3)) #Silhouette plot for 3 clusters

#####CLUSTERING PATTERN BY STATES (DEATH CASES)#####
s_deaths<-read.csv("s_deaths.csv", row.names = 1, header = TRUE, sep=",")
#Euclidean based distance method
head(s_deaths)
#Euclidean based distance method
df2<-scale(s_deaths)
res.dist2<-get_dist(df2,method="euclidean") #Dissimilarity matrix
head(round(as.matrix(res.dist2), 2))[1:37]
#Visualize the dissimilarity matrix
fviz_dist(res.dist2, lab_size=6)
hclust(res.dist2, method="ward.D2")
plot(hclust(res.dist2, method="ward.D2"), cex=.8, col="brown") ##Dendrogram
fviz_nbclust(df2, FUN=hcut, method = "silhouette") #Average Silhouette Method for optimal no. of clusters ()
#Enhanced K-Means clustering
eclust(df2, "kmeans", k=2)
eclust(df2, "kmeans", k=3)
fviz_silhouette(eclust(df2, "kmeans", k=2)) #Silhouette plot for 2 clusters
fviz_silhouette(eclust(df2, "kmeans", k=3)) #Silhouette plot for 3 clusters