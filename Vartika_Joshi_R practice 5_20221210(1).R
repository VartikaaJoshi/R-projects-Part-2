# Import necessary libraries
library(tidyverse)

# Read in the data
data <- read_csv("scrap price.csv")

# Check the structure of the data
str(data)

# Check the summary statistics of the data
summary(data)

# Check the number of rows and columns in the dataset
dim(data)

# Check the data types of each variable
sapply(data, class)

# Check for missing values
colSums(is.na(data))

# Visualize the distribution of the variables using histograms
ggplot(data, aes(x = price)) + geom_histogram()
ggplot(data, aes(x = horsepower)) + geom_histogram()
ggplot(data, aes(x = carwidth)) + geom_histogram()

# Check the correlations between variables using a correlation matrix
cor(data)

# Visualize the correlations using a heatmap
library(corrplot)
corrplot(cor(data), type = "lower")

# Check the relationship between variables using scatter plots
ggplot(data, aes(x = carwidth, y = price)) + geom_point()
ggplot(data, aes(x = horsepower, y = price)) + geom_point()




install.packages("corrplot")
library(corrplot)
# create a matrix of random values
data <- read.csv("scrap price.csv")

# Compute the correlations
correlations <- cor(data[, c("price", "horsepower", "enginesize")])

# Print the correlations
print(correlations)

price ~ enginesize

# Install and load the GGally package 
install.packages("GGally") 
library(GGally)

# Produce the correlation chart
ggcorr(data, columns = c("price", "horsepower"))

new_data <- data.matrix(data)
# calculate the correlation matrix
cor_matrix <- cor(new_data)

# plot the correlation matrix
corrplot(cor_matrix)

# Load the necessary packages
library(tidyverse)
# Load the dataset
df <- data
# Fit a linear regression model to predict the price of a car based on its horsepower 
model <- lm(price ~ horsepower, data = df)
# Print the model summary
summary(model)

