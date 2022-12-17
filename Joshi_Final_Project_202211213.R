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


# Create a t-test plot using the AQI Value column
library(ggplot2)
ggplot(global_data, aes(x = AQI.Value)) +
  geom_density(alpha = 0.5) +
  stat_function(fun = dt, args = list(global_data = global_data$AQI.Value, global_data= global_data$AQI.Category), geom = "line", color = "red")



# Create a scatter plot using the AQI Value and PM2.5 AQI Value columns
ggplot(global_data, aes(x = AQI.Value, y = PM2.5.AQI.Value)) +
  geom_point()

# Load the data into a data frame
df <- data.frame(read.csv("global air pollution dataset.csv"), stringsAsFactors = FALSE)

# Calculate the means of the two groups
us_mean <- mean(df[df$Country == "United States of America", "AQI Value"])
all_mean <- mean(df$AQI.Value)

# Create a t-test plot comparing the means of the two groups
library(ggplot2)
ggplot(data.frame(mean = c(us_mean, all_mean)), aes(x = mean)) +
  geom_density(alpha = 0.5) +
  stat_function(fun = dt, args = list(df = 1, df = 1), geom = "line", color = "red")

# Create a scatter plot using the AQI Value and PM2.5 AQI Value columns
# Color the points based on the Country column
ggplot(df, aes(x = AQI.Value, y = PM2.5.AQI.Value, color = Country)) +
  geom_point()

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

## Milestone 2 : Hypothesis testing 

install.packages("gginference")
library(gginference)

#Hypothesis test for air quality index value
aqi_value <- global_data$AQI.Value
mean(aqi_value)
t.test(aqi_value, mu =72, alternative = "two.sided", conf.level = 0.95)

ggttest(t.test(aqi_value, mu= 72, alternative = "two.sided", conf.level = 0.95))

#Hypothesis test for co value
co_value <- global_data$CO.AQI.Value
t.test(co_value, mu =1.3, alternative = "less", conf.level = 0.95)
ggttest(t.test(co_value, mu= 1.3, alternative = "less", conf.level = 0.95))


#Hypothesis test for ozone value
ozone_value <- global_data$Ozone.AQI.Value
t.test(ozone_value, mu =35, alternative = "two.sided", conf.level = 0.95)
ggttest(t.test(ozone_value, mu= 35, alternative = "two.sided", conf.level = 0.95))


#Hypothesis test for no2 value
no2_value <- global_data$NO2.AQI.Value 
t.test(no2_value, mu =3, alternative = "greater", conf.level = 0.95)
ggttest(t.test(no2_value, mu= 3, alternative = "greater", conf.level = 0.95))


#Hypothesis test for pm2.5 value
pm2.5_value <- global_data$PM2.5.AQI.Value
t.test(pm2.5_value, mu =68, alternative = "greater", conf.level = 0.95)
ggttest(t.test(pm2.5_value, mu= 68, alternative = "greater", conf.level = 0.95))

#Final Compilation
# Check the structure of the dataset
str(global_data)

# Check for missing values
sum(is.na(global_data))

# Impute missing values, if any
global_data <- global_data %>%
  mutate_all(funs(replace(., is.na(.), 0)))

# Check the summary statistics for the dataset
summary(global_data)

# Create a bar chart to visualize the distribution of AQI values across different countries
ggplot(global_data, aes(x = Country, y = AQI.Value)) +
  geom_bar(stat = "identity") +
  xlab("Country") +
  ylab("AQI Value") +
  ggtitle("Distribution of AQI Values Across Different Countries")

# Create a scatter plot to visualize the relationship between AQI Value and Ozone AQI Value
ggplot(global_data, aes(x = AQI.Value, y = Ozone.AQI.Value)) +
  geom_point() +
  xlab("AQI Value") +
  ylab("Ozone AQI Value") +
  ggtitle("Relationship Between AQI Value and Ozone AQI Value")


install.packages("corrplot")
library(corrplot)
new_data <- data.matrix(global_data)
# Compute the pairwise correlations between the different variables
cor(new_data)


# Load the required libraries
install.packages("ggplot2")
install.packages("dplyr")
install.packages("gginference")
library(gginference)
library(dplyr)
library(ggplot2)
library(tidyr)
library(e1071)

# Read the dataset into R
dataset <- read.csv("global air pollution dataset.csv")

# Calculate the average AQI value for cities in the United States
us_mean <- dataset %>%
  filter(Country == "United States of America") %>%
  summarise(mean_aqi = mean(AQI.Value))

# Calculate the average AQI value for cities in all countries
all_mean <- dataset %>%
  summarise(mean_aqi = mean(AQI.Value))

# Extract the numeric values from the us_mean and all_mean objects
us_mean_value <- us_mean$mean_aqi
all_mean_value <- all_mean$mean_aqi

# Perform a t-test to compare the two averages
t.test(us_mean_value, all_mean_value)


# Calculate the mean value for each column of data 
mean(global_data$AQI.Value) 
mean(global_data$CO.AQI.Value) 
mean(global_data$Ozone.AQI.Value) 
mean(global_data$NO2.AQI.Value) 
mean(global_data$PM2.5.AQI.Value)

# Visualize the data using a scatter plot 
plot(global_data$AQI_Value, global_data$CO_AQI_Value)


# Calculate the Pearson correlation coefficient
cor(global_data$AQI.Value, global_data$Ozone.AQI.Value)
cor(global_data$AQI.Value, global_data$PM2.5.AQI.Value)
cor(global_data$AQI.Value, global_data$NO2.AQI.Value)
