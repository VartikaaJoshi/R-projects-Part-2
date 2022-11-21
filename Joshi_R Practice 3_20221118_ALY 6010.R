print("Vartika Joshi")

getwd()
setwd("/Users/vj/Documents/ALY 6010 (Prob and Intro to Stats)/R practice 3")

sample_data <- read.csv("median_income_by_state_2010updated.csv")

sample_data

str(sample_data)
summary(sample_data)
head(sample_data)

view(sample_data)


south_atlantic <- subset(sample_data, Zone=="South Atlantic States")
sa <-south_atlantic$X2010
summary(south_atlantic)
t.test(sa, mu =50000, alternative = "greater", conf.level = 0.95)


install.packages("gginference")
library(gginference)

ggttest(t.test(sa, mu =50000, alternative = "greater", conf.level = 0.95))

ggttest(t.test(sa, mu =50000, alternative = "two.sided", conf.level = 0.95))

ggttest(t.test(sa, mu =50000, alternative = "less", conf.level = 0.95))



west_south <- subset(sample_data, Zone=="West south central states")
ws <-west_south$X2010
summary(west_south)
t.test(ws, mu =50000, alternative = "less", conf.level = 0.95)

ggttest(t.test(ws, mu =50000, alternative = "less", conf.level = 0.95))

east_south <- subset(sample_data, Zone=="East south central states")
es <-east_south$X2010
summary(east_south)

t.test(es, mu =50000, alternative = "greater", conf.level = 0.95)

ggttest(t.test(es, mu =50000, alternative = "greater", conf.level = 0.95))
