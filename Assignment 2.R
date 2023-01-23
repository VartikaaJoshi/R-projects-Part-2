#section 11-1
critical_value <- qchisq(0.10, 3)
critical_value

expected_A <- 50 * 0.20
expected_B <- 50 * 0.28
expected_O <- 50 * 0.36
expected_AB <- 50 * 0.16

test_statistic <- sum((c(12,8,24,6) - c(expected_A,expected_B,expected_O,expected_AB))^2/c(expected_A,expected_B,expected_O,expected_AB))
test_statistic

# calculate the expected frequencies for each category, assuming the null hypothesis is true
expected_ontime <- 200 * 0.708
expected_nas_delay <- 200 * 0.082
expected_late <- 200 * 0.09
expected_other <- 200 * 0.12

# calculate the test statistic
observed <- c(125,40,10,35)
expected <- c(expected_ontime,expected_other,expected_nas_delay,expected_late)
test_statistic <- sum((observed - expected)^2/expected)

# find the critical value
critical_value <- qchisq(0.05, 3)

# make the decision
if (test_statistic > critical_value) {
  print("Reject the null hypothesis")
} else {
  print("Fail to reject the null hypothesis")
}


#Section 11-2

critical_value <- qchisq(0.05, 2)
critical_value

install.packages("tidyverse")
library(tidyverse)
observed <- matrix(c(724, 335, 174, 107, 370, 292, 152, 140), nrow = 4, byrow = TRUE)
chisq.test(observed)                    
                      
#Section 12-1

# Significance level = 0.05
# Degrees of freedom = (3-1) * (8-1) = 6
critical_value <- qf(0.95, 2, 6)
critical_value 

# Create a new data frame with 'sodium' variable
df <- data.frame(sodium = c(270, 130, 230, 180, 80, 70, 200), 
                 food = c("condiments", "cereals", "desserts", "condiments", "cereals", "desserts", "condiments"))

# Perform the ANOVA test
test_value <- aov(sodium ~ food, data = df)

# Print the summary of the ANOVA test
summary(test_value)

# Perform multiple comparison test
TukeyHSD(test_value)


#Section 12-2

# Create a new data frame with the sales data
df <- data.frame(sales = c(578, 311, 261, 320, 106, 185, 264, 109, 302, 249, 125, 689, 237, 173),
                 product = c("Cereal", "Cereal", "Cereal", "Chocolate Candy", "Chocolate Candy", "Chocolate Candy", "Coffee", "Coffee", "Coffee", "Coffee", "Coffee", "Coffee", "Coffee", "Coffee"))

# Perform the ANOVA test
test_value <- aov(sales ~ product, data = df)

# Print the summary of the ANOVA test
summary(test_value)

# Create a new data frame with the sales data
df <- data.frame(sales = c(578, 311, 261, 320, 106, 185, 264, 109, 302, 249, 125, 689, 237, 173),
                 product = c("Cereal", "Cereal", "Cereal", "Chocolate Candy", "Chocolate Candy", "Chocolate Candy", "Coffee", "Coffee", "Coffee", "Coffee", "Coffee", "Coffee", "Coffee", "Coffee"))

# Perform the ANOVA test
test_value <- aov(sales ~ product, data = df)

# Print the summary of the ANOVA test
summary(test_value)

dataframe <- data.frame(EasternThird = c(4946, 5953, 6202),
                        MiddleThird = c(6149, 7451, 6000),
                        WesternThird = c(5282, 8605, 6528))
dataframe$region <- factor(c("EasternThird", "MiddleThird", "WesternThird"))

# Perform the ANOVA
aov_result <- aov(EasternThird ~ region, data = dataframe)

# Print the summary of the ANOVA
summary(aov_result)

#section 12-3
# find the critical value for interaction
qf(0.95, 2, 6)
# find the critical value for light
qf(0.95, 1, 6)
# find the critical value for plant food
qf(0.95, 1, 6)


# Create dataframe
dataframe <- data.frame(Grow_light = c(rep("Grow_light1", 6), rep("Grow_light2", 6)),
                        Plant_food = c(rep("Plant_food_A", 3), rep("Plant_food_A", 3), 
                                       rep("Plant_food_B", 3), rep("Plant_food_B", 3)),
                        Growth = c(9.2, 9.4, 8.9, 8.5, 9.2, 8.9, 7.1, 7.2, 8.5, 5.5, 5.8, 7.6))

# Run ANOVA
anova_results <- aov(Growth ~ Grow_light*Plant_food, data = dataframe)

summary(anova_results)

#On your Own
getwd()

baseball_data <- read.csv("baseball-1.csv")

# Get summary statistics
summary(baseball_data)

str(baseball_data)

mean(baseball_data$RS)
mean(baseball_data$RA)
mean(baseball_data$W)
mean(baseball_data$OBP)
mean(baseball_data$SLG)
mean(baseball_data$BA)

sd(baseball_data$RS)
sd(baseball_data$RA)
sd(baseball_data$W)
sd(baseball_data$OBP)
sd(baseball_data$SLG)
sd(baseball_data$BA)

sample(baseball_data)

kurtosis(baseball_data$RS)
kurtosis(baseball_data$RA)
kurtosis(baseball_data$W)
kurtosis(baseball_data$OBP)
kurtosis(baseball_data$SLG)
kurtosis(baseball_data$BA)

skewness(baseball_data$RS)
skewness(baseball_data$RA)
skewness(baseball_data$W)
skewness(baseball_data$OBP)
skewness(baseball_data$SLG)
skewness(baseball_data$BA)





# Scatter plot of wins and OBP
library(ggplot2)
ggplot(data=baseball_data, aes(x = W, y = OBP)) +
  geom_point() +
  ggtitle("Wins vs On-base Percentage")

# Histogram of runs scored and runs allowed
ggplot(data=baseball_data, aes(x = RS)) +
  geom_histogram(binwidth = 25) +
  ggtitle("Histogram of Runs Scored")

ggplot(data=baseball_data, aes(x = RA)) +
  geom_histogram(binwidth = 25) +
  ggtitle("Histogram of Runs Allowed")

# Create a new variable "Decade"
baseball_data$Decade <- floor(baseball_data$Year/10)*10

# Count the number of wins in each decade
wins_by_decade <- with(baseball_data, tapply(W, Decade, sum))

# Expected frequencies are equal
expected_freq <- sum(wins_by_decade) / length(wins_by_decade)
expected_freq
# Compute the test value
test_value <- sum((wins_by_decade - expected_freq)^2 / expected_freq)
test_value
