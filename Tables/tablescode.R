library(dplyr)
library(survey)

nhanes = read.csv("full_data_v4.csv")[ , -c(1,2)] %>% 
  mutate(is_actual_under = ifelse(nhanes$bmi == "underweight", 1, 0)) %>% 
  mutate(is_actual_normal = ifelse(nhanes$bmi == "normal", 1, 0)) %>% 
  mutate(is_actual_over = ifelse(nhanes$bmi == "overweight", 1, 0)) %>% 
  mutate(is_actual_obese = ifelse(nhanes$bmi == "obese", 1, 0))
nhanes_design = svydesign(ids = ~psu+id, strata = ~stratum, 
                          nest = TRUE, weights = ~exam_wt, data = nhanes)

round(svyby(~is_actual_under, by = ~freq_pe, design = nhanes_design, svymean), 5)
round(svyby(~is_actual_normal, by = ~freq_pe, design = nhanes_design, svymean), 5)
round(svyby(~is_actual_over, by = ~freq_pe, design = nhanes_design, svymean), 5)
round(svyby(~is_actual_obese, by = ~freq_pe, design = nhanes_design, svymean), 5)

temp = round(svymean(~interaction(freq_pe,bmi,opinion_wt),design=nhanes_design),5)

prop.table(svytable(~freq_pe+bmi+opinion_wt,design=nhanes_design),5)

library(dplyr)
library(survey)
library(ggplot2)

nhanes = read.csv("~/Desktop/Stat152/FinalProject/full_data_v4.csv")[ , -c(1,2)] %>% 
  mutate(bmi1 = as.factor(ifelse(bmi == "obese", "overweight", as.character(bmi)))) %>% 
  mutate(correct_perception = as.logical(bmi1 == opinion_wt))
nhanes_design = svydesign(ids = ~psu+id, strata = ~stratum, 
                          nest = TRUE, weights = ~exam_wt, data = nhanes)


#Proportion in Each Weight Category by Frequency of PE
svymean(~interaction(freq_pe, bmi), design = nhanes_design)
bmi_tbl = svytable(~freq_pe+bmi, nhanes_design)
bmi_prop_tbl = round(prop.table(bmi_tbl), 5)
summary(bmi_tbl, statistic = "F")
svychisq(~freq_pe+bmi, nhanes_design, statistic = "F")

#Barplot
bmi_prop_df = as.data.frame(bmi_prop_tbl)
prop_freq_pe = tapply(bmi_prop_df$Freq, bmi_prop_df$freq_pe, sum)
bmi_prop_df$prop_freq_pe = prop_freq_pe[bmi_prop_df$freq_pe]
plot_df = bmi_prop_df %>% 
  mutate(std_prop = Freq/prop_freq_pe)

ggplot(plot_df, aes(freq_pe, std_prop)) +   
  geom_bar(aes(fill = bmi), position = "dodge", stat="identity") + 
  labs(x = "Frequency of PE",
       y = "Proportion in Each Weight Category",
       fill = "Weight Category",
       title = "Proportion of Students in Each Weight Category by Frequency of PE")

######################################################################################

#Perceived and Actual Weight Category by Frequency of PE
svymean(~interaction(freq_pe, bmi, opinion_wt), design = nhanes_design)
perception_tbl = svytable(~freq_pe+correct_perception, nhanes_design)
perception_prop_tbl = round(prop.table(perception_tbl), 5) # make a table of this
summary(perception_tbl, statistic = "F")
svychisq(~freq_pe+correct_perception, nhanes_design, statistic = "F")

#Barplot
perception_prop_df = as.data.frame(perception_prop_tbl)
prop_freq_pe = tapply(perception_prop_df$Freq, perception_prop_df$freq_pe, sum)
perception_prop_df$prop_freq_pe = prop_freq_pe[perception_prop_df$freq_pe]
plot_df = perception_prop_df %>% 
  mutate(std_prop = Freq/prop_freq_pe)

ggplot(plot_df, aes(freq_pe, std_prop)) +   
  geom_bar(aes(fill = correct_perception), position = "dodge", stat="identity") + 
  labs(x = "Frequency of PE",
       y = "Proportion Whose Weight Perception Matched BMI Category",
       fill = "Correct Perception",
       title = "Proportion of Students Whose Weight Perception Matched BMI Category\nby Frequency of PE")

######################################################################################

#Proportion in Each Weight Category by Enjoyment of PE
svymean(~interaction(enjoy_pe, bmi), design = nhanes_design)
bmi_tbl = svytable(~enjoy_pe+bmi, nhanes_design)
bmi_prop_tbl = round(prop.table(bmi_tbl), 5)
summary(bmi_tbl, statistic = "F")
svychisq(~enjoy_pe+bmi, nhanes_design, statistic = "F")

#Barplot
bmi_prop_df = as.data.frame(bmi_prop_tbl)
prop_enjoy_pe = tapply(bmi_prop_df$Freq, bmi_prop_df$enjoy_pe, sum)
bmi_prop_df$prop_enjoy_pe = prop_enjoy_pe[bmi_prop_df$enjoy_pe]
plot_df = bmi_prop_df %>% 
  mutate(std_prop = Freq/prop_enjoy_pe)

ggplot(plot_df, aes(enjoy_pe, std_prop)) +   
  geom_bar(aes(fill = bmi), position = "dodge", stat="identity") + 
  labs(x = "Enjoyment of PE",
       y = "Proportion in Each Weight Category",
       fill = "Weight Category",
       title = "Proportion of Students in Each Weight Category by Enjoyment of PE")

######################################################################################

#Perceived and Actual Weight Category by Enjoyment of PE
svymean(~interaction(enjoy_pe, bmi, opinion_wt), design = nhanes_design)
perception_tbl = svytable(~enjoy_pe+correct_perception, nhanes_design)
perception_prop_tbl = round(prop.table(perception_tbl), 5)
summary(perception_tbl, statistic = "F")
svychisq(~enjoy_pe+correct_perception, nhanes_design, statistic = "F")

#Barplot
perception_prop_df = as.data.frame(perception_prop_tbl)
prop_enjoy_pe = tapply(perception_prop_df$Freq, perception_prop_df$enjoy_pe, sum)
perception_prop_df$prop_enjoy_pe = prop_enjoy_pe[perception_prop_df$enjoy_pe]
plot_df = perception_prop_df %>% 
  mutate(std_prop = Freq/prop_enjoy_pe)

ggplot(plot_df, aes(enjoy_pe, std_prop)) +   
  geom_bar(aes(fill = correct_perception), position = "dodge", stat="identity") + 
  labs(x = "Enjoyment of PE",
       y = "Proportion Whose Weight Perception Matched BMI Category",
       fill = "Correct Perception",
       title = "Proportion of Students Whose Weight Perception Matched BMI Category\nby Enjoyment of PE")


###########################################
dataNA <- read.csv('full_data_v3.csv', header = TRUE)
dataNA$X <- NULL
dataNA$X.1 <- NULL

dataNA1 <- dataNA[dataNA$exam_wt != 0, ]


subset1 <- dataNA1[!complete.cases(dataNA1), ]

library(dplyr)
agedf <- subset1 %>% 
  group_by(age, gender) %>% 
  tally()


ggplot(agedf, aes(age, n)) +   
  geom_bar(aes(fill = gender), position = "dodge", stat="identity") + 
  scale_y_continuous(breaks = seq(0,10,by=2))+
  labs(x = "Age",
       y = "Frequency",
       fill = "Gender",
       title = "Frequency of Nonresponse by Age")



