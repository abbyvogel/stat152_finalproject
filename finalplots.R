
data <- read.csv("~/Desktop/stat152_finalproject/full_data_v4.csv")

options(scipen=5)

hist(data$exam_wt, breaks=15, xlab="Exam Weight", main="Histogram of Exam Weight")

library(ggplot2)

##### Plot: BMI and PE Enjoyment #####
er <- table(data$bmi, data$enjoy_pe)
mosaicplot(er, las=1, xlab="BMI", ylab="Enjoy PE", main="BMI and PE Enjoyment")

##### Plot: Age and PE enjoyment #####
age <- table(data$age, data$enjoy_pe)
mosaicplot(age, las=1, xlab="Age", ylab="Enjoy PE", main="Age and PE Enjoyment")
##### Plot: BMI and Weight Perception #####
enj <- table(data$bmi,data$opinion_wt)
mosaicplot(enj, las=1, xlab="BMI", ylab="Perception of Weight", main="BMI and Weight Perception")

##### Plot: Exam Weight and Pseudo-Stratum and Pseudo PSU #####

library(plyr)
data$bmi <- revalue(data$bmi, c("overweight"="over", "underweight"="under"))

boxplot(data$exam_wt ~ data$stratum, main="Exam Weight by Pseudo-Stratum")
boxplot(data$exam_wt ~ data$psu, main="Exam Weight by Pseudo-PSU")

hist(data$exam_wt~data$bmi)

##### Plot: Total Exam Weight By BMI Class #####
ugh <- c(
sum(data$exam_wt[which(data$bmi=="obese")]),
sum(data$exam_wt[which(data$bmi=="overweight")]),
sum(data$exam_wt[which(data$bmi=="normal")]),
sum(data$exam_wt[which(data$bmi=="underweight")]))

barplot(ugh, names.arg=c("Obese", "Overweight", "Normal", "Underweight"), main="Total Exam Weight in Sample by BMI Class")


##### Plot: Total Exam Weight By PE Enjoyment#####
ugh2 <- c(
  sum(data$exam_wt[which(data$enjoy_pe=="strongly agree")]),
  sum(data$exam_wt[which(data$enjoy_pe=="agree")]),
  sum(data$exam_wt[which(data$enjoy_pe=="neither agree nor disagree")]),
  sum(data$exam_wt[which(data$enjoy_pe=="disagree")]),
  sum(data$exam_wt[which(data$enjoy_pe=="strongly disagree")]),
  sum(data$exam_wt[which(data$enjoy_pe=="unsure")])
   )

barplot(ugh2, main="Total Exam Weights by PE Enjoyment")
x.labs = c("Strongly Agree", "Agree", "Neither", "Disagree", "Strongly Disagree", "Unsure")
axis(1,at=c(0.7, 1.9, 3.1, 4.3, 5.5, 6.6), labels = x.labs, cex.axis = .5)


##### Plot: Total Exam Weight By PE Presence#####
ugh4 <- c(
  sum(data$exam_wt[which(data$pe_yn=="yes")]),
  sum(data$exam_wt[which(data$pe_yn=="no")])
)

barplot(ugh4, main="Total Exam Weights by PE Presence")
x.labs = c("Present", "Absent")
axis(1,at=c(0.7, 1.9), labels = x.labs, cex.axis = 1.5)



##### Plot: Total Exam Weight By PE Enjoyment#####

data$freq_pe <- as.factor(data$freq_pe)

ugh3 <- c(
  sum(data$exam_wt[which(data$freq_pe=="0")]), 
  sum(data$exam_wt[which(data$freq_pe=="1")]),
  sum(data$exam_wt[which(data$freq_pe=="2")]),
  sum(data$exam_wt[which(data$freq_pe=="3")]),
  sum(data$exam_wt[which(data$freq_pe=="4")]),
  sum(data$exam_wt[which(data$freq_pe=="5")])
)

barplot(ugh3, main="Total Exam Weights by PE Frequency", xlab= "Days per Week")
x.labs = c("NA", "1", "2", "3", "4", "5")
axis(1,at=c(0.7, 1.9, 3.1, 4.3, 5.5, 6.6), labels = x.labs, cex.axis = 1)






