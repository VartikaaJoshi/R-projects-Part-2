getwd()
setwd("/Users/vj/Documents/ALY 6010 (Prob and Intro to Stats)/R Practice 2")

student_data <- read.csv("StudentsPerformance.csv")
student_data

install.packages("dplyr")

library(dplyr)

duplicate(student_data)

head(student_data)
str(student_data)
summary(student_data)

student_data$gender <- factor(student_data$gender)
student_data$race.ethnicity <- factor(student_data$race.ethnicity)
student_data$parental.level.of.education <- factor(student_data$parental.level.of.education)
student_data$lunch <- factor(student_data$lunch)
student_data$test.preparation.course <- factor(student_data$test.preparation.course)

str(student_data)

install.packages("moments")
library(moments)

sd(student_data$math.score)
sd(student_data$reading.score)
sd(student_data$writing.score)

skewness(student_data$math.score)
skewness(student_data$reading.score)
skewness(student_data$writing.score)

kurtosis(student_data$math.score)
kurtosis(student_data$reading.score)
kurtosis(student_data$writing.score)


library(ggplot2)

#Visualization
#Math score histogram
t<- ggplot(data=student_data, aes(x=math.score))
t+ geom_histogram(binwidth=10, fill="Yellow", colour="Black")

#Reading score histogram
s<- ggplot(data=student_data, aes(x=reading.score))
s+ geom_histogram(binwidth=10, fill="Pink", colour="Black")

#Writing score histogram
u<- ggplot(data=student_data, aes(x=writing.score))
u+ geom_histogram(binwidth=10, fill="Light Blue", colour="Black")

#Parental level of education vs reading score
ggplot(data=student_data, aes(x= parental.level.of.education, y= reading.score))+ geom_point() + geom_smooth()

#Ethnicity vs math score
ggplot(data=student_data, aes(x= race.ethnicity, y=math.score))+ geom_point() + geom_smooth()

#Test preparation vs writing score
ggplot(data=student_data, aes(x= test.preparation.course, y=writing.score))+ geom_point() + geom_smooth()

#Math score vs writing score - gender
qplot(data=student_data, x=math.score, y=writing.score,colour=gender, size=I(5), alpha=I(0.6), main="Math score vs writing score")

#reading score vs writing score - gender
qplot(data=student_data, x=reading.score, y=writing.score,colour=gender, size=I(5), alpha=I(0.6), main="Reading score vs writing score")

#Math score vs writing score - parental level of education
qplot(data=student_data, x=math.score, y=writing.score,colour=parental.level.of.education, size=I(5), alpha=I(0.6), main="Math score vs writing score")

#Reading score vs writing score - parental level of education
qplot(data=student_data, x=reading.score, y=writing.score,colour=parental.level.of.education, size=I(5), alpha=I(0.6), main="Reading score vs writing score")


