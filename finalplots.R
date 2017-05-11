
data <- read.csv("~/Desktop/stat152_finalproject/full_data_v4.csv")

options(scipen=5)

hist(data$exam_wt, breaks=15, xlab="Exam Weight", main="Histogram of Exam Weight")


library(ggplot2)


er <- table(data$bmi, data$enjoy_pe)

mosaicplot(er, las=1, xlab="BMI", ylab="Enjoy PE", main="BMI and PE Enjoyment")


age <- table(data$age, data$enjoy_pe)
mosaicplot(age, las=1, xlab="Age", ylab="Enjoy PE", main="Age and PE Enjoyment")

enj <- table(data$bmi,data$opinion_wt)
mosaicplot(enj, las=1, xlab="BMI", ylab="Perception of Weight", main="BMI and Weight Perception")

library(plyr)
data$bmi <- revalue(data$bmi, c("overweight"="over", "underweight"="under"))

boxplot(data$exam_wt ~ data$stratum, main="Exam Weight by Psuedo-Stratum")
boxplot(data$exam_wt ~ data$psu, main="Exam Weight by Psuedo-PSU")
