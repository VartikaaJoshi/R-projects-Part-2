print("Vartika Joshi")

getwd()
setwd("/Users/vj/Documents/ALY 6010 (Prob and Intro to Stats)/Project")

global_data <- read.csv("global air pollution dataset.csv")

library("dplyr")

unique(global_data)

head(global_data)
summary(global_data)
class(global_data)
str(global_data)

global_data$Country <- factor(global_data$Country)
global_data$City <- factor(global_data$City)
global_data$AQI.Category <- factor(global_data$AQI.Category)
global_data$CO.AQI.Category <- factor(global_data$CO.AQI.Category)
global_data$Ozone.AQI.Category <- factor(global_data$Ozone.AQI.Category)
global_data$NO2.AQI.Category <- factor(global_data$NO2.AQI.Category)
global_data$PM2.5.AQI.Category <- factor(global_data$PM2.5.AQI.Category)

str(global_data)
View(global_data)

library(tidyverse)

global_data %>% 
  count(AQI.Category)

global_data %>% 
  count(CO.AQI.Category)

global_data %>% 
  count(Ozone.AQI.Category)

global_data %>% 
  count(NO2.AQI.Category)

global_data %>% 
  count(PM2.5.AQI.Category)
  
#Descriptive Analysis
sd(global_data$AQI.Value)
sd(global_data$CO.AQI.Value)
sd(global_data$Ozone.AQI.Value)
sd(global_data$NO2.AQI.Value )
sd(global_data$PM2.5.AQI.Value)

install.packages("moments")
library(moments)

skewness(global_data$AQI.Value)
skewness(global_data$CO.AQI.Value)
skewness(global_data$Ozone.AQI.Value)
skewness(global_data$NO2.AQI.Value )
skewness(global_data$PM2.5.AQI.Value)

kurtosis(global_data$AQI.Value)
kurtosis(global_data$CO.AQI.Value)
kurtosis(global_data$Ozone.AQI.Value)
kurtosis(global_data$NO2.AQI.Value )
kurtosis(global_data$PM2.5.AQI.Value)

#Visualization

library("ggplot2")


ggplot(data = global_data) +
  geom_bar(mapping = aes(y = Country))

#AQI
#central tendency
summary(global_data$AQI.Value )
boxplot(global_data$AQI.Value )
#spread
hist(global_data$AQI.Value )
plot(density(global_data$AQI.Value ), main ="AQI")

#CO.AQI.Value
#central tendency
summary(global_data$CO.AQI.Value )
boxplot(global_data$CO.AQI.Value )
#spread
hist(global_data$CO.AQI.Value)
plot(density(global_data$CO.AQI.Value),  main ="Carbon mono-oxide AQI")


#Ozone.AQI.Value
#central tendency
summary(global_data$Ozone.AQI.Value )
boxplot(global_data$Ozone.AQI.Value)
#spread
hist(global_data$Ozone.AQI.Value)
plot(density(global_data$Ozone.AQI.Value),  main ="Ozone AQI")


#NO2.AQI.Value
#central tendency
summary(global_data$NO2.AQI.Value )
boxplot(global_data$NO2.AQI.Value)
#spread
hist(global_data$NO2.AQI.Value)
plot(density(global_data$NO2.AQI.Value),  main ="Nitrogen dioxide AQI")


#PM2.5.AQI.Value
#central tendency
summary(global_data$PM2.5.AQI.Value)
boxplot(global_data$PM2.5.AQI.Value)
#spread
hist(global_data$PM2.5.AQI.Value)
plot(density(global_data$PM2.5.AQI.Value),  main ="PM 2.5 AQI")


