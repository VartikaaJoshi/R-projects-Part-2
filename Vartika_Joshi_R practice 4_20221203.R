#Installing packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("gginference")
library(gginference)
library(dplyr)
library(ggplot2)
library(tidyr)
library(e1071)

getwd()
setwd("/Users/vj/Documents/ALY 6010 (Prob and Intro to Stats)/R practice 4")
getwd()
my_data <- read.csv("world_happiness_report_20221128.csv")
my_data
dim(my_data)
colnames(my_data)

#Change column names
names(my_data) <- c("Country_name", "Regional_indicator", "Ladder_score", "Standard_error_of_ladder_score", 
                    "Upperwhisker", "Lowerwhisker", "Logged_GDP_per_capita", "Social_support", "Healthy_life_expectancy", 
                    "Freedom_to_make_life_choices", "Generosity", "Perceptions_of_corruption", "Ladder_score_in_Dystopia", 
                    "Explained_by_Log_GDP_per_capita", "Explained_by_Social_support", "Explained_by_Healthy_life_expectancy", 
                    "Explained_by_Freedom_to_make_life_choices", "Explained_by_Generosity", "Explained_by_Perceptions_of_corruption", "Dystopia_residual")
colnames(my_data)
my_data
#Structure and summary of the data set
str(my_data)
summary(my_data)

#Checking null values of the data set
colSums(is.na(my_data))

#Head and Tail of the data set
head(my_data)
tail(my_data)

#Descriptive statistics
mean(my_data$Ladder_score)
median(my_data$Ladder_score)
mode(my_data$Ladder_score)
sd(my_data$Ladder_score)
var(my_data$Ladder_score)
skewness(my_data$Ladder_score)
kurtosis(my_data$Ladder_score)


#Plotting frequency table for regional indicator
Regional_indicator_Freq <- my_data %>%
  select(Regional_indicator) %>%
  group_by(Regional_indicator) %>%
  dplyr:: summarize(Regional_indicator_Count = n())
Regional_indicator_Freq

#Cross Tabulation
CrossT1 <- xtabs(~Regional_indicator + Country_name, data = my_data)
CrossT1

#Satter Plot
plot(my_data$Ladder_score, main = "Plot:score")
plot(my_data$Generosity, main = "Plot:Generocity")
plot(my_data$Logged_GDP_per_capita, ylab = "Logged GDP per capita", main = "Plot:Logged GDP per capita", col = "red")


#Boxplot
boxplot(my_data$Generosity, ylab = "Generocity", main = "Boxplot - Generocity", col = "Cyan2", las = 1)
boxplot(my_data$Explained_by_Social_support, ylab = "Social Support", main = "Boxplot - Social Support", col = "Cyan2", las = 1)

#Histogram
hist(my_data$Social_support, main = "Histogram - Social Support", col = "pink", xlab = "Socail Support")
hist(my_data$Logged_GDP_per_capita, xlab = "Logged GDP per capita", main = "Plot:Logged GDP per capita", col = "turquoise")

#ggplot
ggplot(data = my_data, aes(x = Ladder_score, y = Logged_GDP_per_capita))+
  geom_point()+
  labs(title = "Ladder score Vs Logged GDP per capita",
       x = "Ladder score",
       y = "Logged GDP per capita")

ggplot(data = my_data, aes(x = Regional_indicator, y = Ladder_score))+
  geom_point()+
  labs(title = "Regional indicator Vs Ladder score",
       x = "Regional indicator",
       y = "Ladder score")


#subsetting

rich <- my_data$Ladder_score[1:74]
mean_rich <- mean(rich)
mean_rich

poor <- my_data$Ladder_score[76:149]
mean_poor <- mean(poor)
mean_poor

#independent 
difference <- mean_rich - mean_poor
difference

#Shapiro-wilk test

shapiro.test(rich)
shapiro.test(poor)

#population parameters
var.test(rich, poor)


#two sample t-test, conf = 95%
t.test(rich, poor,
       paired = TRUE,   
       #var.equal = TRUE,
       conf.level = 0.95)

#twosample t-test, conf = 90%
t.test(rich, poor,
       paired = TRUE,   
       #var.equal = TRUE,
       conf.level = 0.9)

