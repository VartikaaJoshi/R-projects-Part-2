print("Vartika Joshi")

getwd()
setwd("/Users/vj/Documents/ALY 6010 (Prob and Intro to Stats)/R practice 6")

install.packages("ggplot2")
library(ggplot2)
# Loading the data frame
drinks <- read.csv("drinks.csv")


# Inspect the data
head(drinks)
str(drinks)

drinks<- data
mean(data$beer_servings)
mean(data$spirit_servings)
mean(data$wine_servings)
mean(data$total_liters_of_pure_alcohol)

getwd()

drinks <- read.csv("drinks.csv")
summary(drinks)
mean(drinks$total_litres_of_pure_alcohol)
sd(drinks$total_litres_of_pure_alcohol)
sd(drinks$beer_servings)
sd(drinks$spirit_servings)
sd(drinks$wine_servings)

# calculating Pearson's correlation coefficient between beer servings and wine servings
cor(drinks$beer_servings, drinks$total_litres_of_pure_alcohol)


# Check for missing values
summary(data)

# Visualize the relationships between variables
# Scatterplot of beer servings vs. wine servings
plot(data$beer_servings, data$wine_servings, xlab = "Beer Servings", ylab = "Wine Servings")

# Boxplot of total litres of pure alcohol by continent
boxplot(data$total_litres_of_pure_alcohol ~ data$continent)

# Test for relationships between variables
# Linear regression of total litres of pure alcohol on beer, spirit, and wine servings
model <- lm(data$total_litres_of_pure_alcohol ~ data$beer_servings + data$spirit_servings + data$wine_servings)
summary(model)

# ANOVA test of total litres of pure alcohol by continent
model <- aov(data$total_litres_of_pure_alcohol ~ data$continent)
summary(model)

new_data <- data.matrix(data)
# Correlation matrix of all variables
cor(new_data)

dummies <- model.matrix(drinks$continent)

# bind the dummy variables to the original data frame
drinks <- cbind(drinks, dummies)

# subset the data based on the dummy variables
drinks_asia <- subset(drinks, Asia == 1)
drinks_europe <- subset(df, Europe == 1)
drinks_africa <- subset(df, Africa == 1)
drinks_north_america <- subset(df, North.America == 1)
drinks_south_america <- subset(df, South.America == 1)
drinks_oceania <- subset(df, Oceania == 1)

# install and load the ggplot2 package
install.packages("ggplot2")
library(ggplot2)


asia <- read.csv("drinks_asia.csv")
europe <- read.csv("drinks_europe.csv")
africa <- read.csv("drinks_africa.csv")
north_america <- read.csv("drinks_north_america.csv")
south_america <- read.csv("drinks_south_america.csv")
oceania <- read.csv("drinks_oceania.csv")

# create a scatterplot with multiple regression lines
ggplot(drinks, aes(x = beer_servings, y = wine_servings)) +
  geom_point() +
  geom_smooth(data = asia, color = "red") +
  geom_smooth(data = europe, color = "green") +
  geom_smooth(data = africa, color = "blue") +
  geom_smooth(data = north_america, color = "purple") +
  geom_smooth(data = south_america, color = "orange") +
  geom_smooth(data = oceania, color = "pink")

# create a scatterplot with multiple regression lines
ggplot(drinks, aes(x = beer_servings, y = total_litres_of_pure_alcohol)) +
  geom_point() +
  geom_smooth(data = asia, color = "red") +
  geom_smooth(data = europe, color = "green") +
  geom_smooth(data = africa, color = "blue") +
  geom_smooth(data = north_america, color = "purple") +
  geom_smooth(data = south_america, color = "orange") +
  geom_smooth(data = oceania, color = "pink")


# Load the data
data <- drinks

# Create the dummy variables for the 'continent' variable
continent_dummies <- model.matrix(~continent, data=data)
continent_dummies

# Subset the data using the dummy variables
subset_data <- data[,c("beer_servings", "spirit_servings", "wine_servings", "total_litres_of_pure_alcohol")]
subset_data <- cbind(subset_data, continent_dummies)
subset_data

# Fit the multiple linear regression model
model <- lm(total_litres_of_pure_alcohol ~ ., data=subset_data)
model
# View the summary of the model
summary(model)
plot(model)

# calculate Pearson's correlation coefficient between beer servings and wine servings
cor(df$beer_servings, df$wine_servings)


install.packages("ggplot2")
# Load the ggplot2 package
library(ggplot2)

library(ggplot2)

ggplot(data, aes(x = beer_servings, y = total_litres_of_pure_alcohol)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + dummy_vars)


