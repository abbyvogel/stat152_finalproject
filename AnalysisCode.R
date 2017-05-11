library(dplyr)
library(survey)
library(ggplot2)

nhanes = read.csv("~/Desktop/Stat152/FinalProject/full_data_v4.csv")[ , -c(1,2)] %>% 
  mutate(bmi1 = as.factor(ifelse(bmi == "obese", "overweight", as.character(bmi)))) %>% 
  mutate(correct_perception = as.logical(bmi1 == opinion_wt))
nhanes_design = svydesign(ids = ~psu+id, strata = ~stratum, 
                          nest = TRUE, weights = ~exam_wt, data = nhanes)


#Proportion in Each Weight Category by Existence of PE
svymean(~interaction(pe_yn, bmi), design = nhanes_design)
bmi_tbl = svytable(~pe_yn+bmi, nhanes_design)
bmi_prop_tbl = round(prop.table(bmi_tbl), 5)
summary(bmi_tbl, statistic = "F")
svychisq(~freq_pe+bmi, nhanes_design, statistic = "F")

#Barplot
bmi_prop_df = as.data.frame(bmi_prop_tbl)
prop_pe_yn = tapply(bmi_prop_df$Freq, bmi_prop_df$pe_yn, sum)
bmi_prop_df$prop_pe_yn = prop_pe_yn[bmi_prop_df$pe_yn]
plot_df = bmi_prop_df %>% 
  mutate(std_prop = Freq/prop_pe_yn)

ggplot(plot_df, aes(pe_yn, std_prop)) +   
  geom_bar(aes(fill = bmi), position = "dodge", stat="identity") + 
  labs(x = "Existence of PE",
       y = "Proportion in Each Weight Category",
       fill = "Weight Category",
       title = "Proportion of Adolescents in Each Weight Category by Existence of PE") +
  theme(plot.title = element_text(hjust = 0.5))

######################################################################################

#Proportion of Correct Perceptions by Existence of PE
svymean(~interaction(pe_yn, correct_perception), design = nhanes_design)
bmi_tbl = svytable(~pe_yn+correct_perception, nhanes_design)
bmi_prop_tbl = round(prop.table(bmi_tbl), 5)
summary(bmi_tbl, statistic = "F")
svychisq(~pe_yn+correct_perception, nhanes_design, statistic = "F")

#Barplot
bmi_prop_df = as.data.frame(bmi_prop_tbl)
prop_pe_yn = tapply(bmi_prop_df$Freq, bmi_prop_df$pe_yn, sum)
bmi_prop_df$prop_pe_yn = prop_pe_yn[bmi_prop_df$pe_yn]
plot_df = bmi_prop_df %>% 
  mutate(std_prop = Freq/prop_pe_yn)

ggplot(plot_df, aes(pe_yn, std_prop)) +   
  geom_bar(aes(fill = correct_perception), position = "dodge", stat="identity") + 
  labs(x = "Existence of PE",
       y = "Proportion",
       fill = "Correct Perception",
       title = "Proportion of Correct Perceptions by Existence of PE") +
  theme(plot.title = element_text(hjust = 0.5))

######################################################################################

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
       title = "Proportion of Adolescents in Each Weight Category by Frequency of PE") +
  theme(plot.title = element_text(hjust = 0.5))

######################################################################################

#Perceived and Actual Weight Category by Frequency of PE
svymean(~interaction(freq_pe, bmi, opinion_wt), design = nhanes_design)
perception_tbl = svytable(~freq_pe+correct_perception, nhanes_design)
perception_prop_tbl = round(prop.table(perception_tbl), 5)
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
       y = "Proportion",
       fill = "Correct Perception",
       title = "Proportion of Adolescents Whose Weight Perception Matched BMI Category\nby Frequency of PE") + 
  theme(plot.title = element_text(hjust = 0.5))

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
       title = "Proportion of Adolescents in Each Weight Category by Enjoyment of PE") +
  theme(plot.title = element_text(hjust = 0.5))

######################################################################################


#Proportion Who Enjoy PE by Weight Class
svymean(~interaction(enjoy_pe, bmi), design = nhanes_design)
bmi_tbl = svytable(~enjoy_pe+bmi, nhanes_design)
bmi_prop_tbl = round(prop.table(bmi_tbl), 5)
summary(bmi_tbl, statistic = "F")
svychisq(~enjoy_pe+bmi, nhanes_design, statistic = "F")

#Barplot
bmi_prop_df = as.data.frame(bmi_prop_tbl) %>% 
  mutate(enjoy_pe = factor(enjoy_pe, levels = c("strongly agree", 
                                                "agree",
                                                "neither agree nor disagree",
                                                "disagree",
                                                "strongly disagree",
                                                "unsure"))) 
prop_weight = tapply(bmi_prop_df$Freq, bmi_prop_df$bmi, sum)
bmi_prop_df$prop_weight = prop_weight[bmi_prop_df$bmi]
new_prop = bmi_prop_df$Freq/bmi_prop_df$prop_weight
plot_df = bmi_prop_df %>% 
  mutate(std_prop = new_prop)

ggplot(plot_df, aes(bmi, new_prop)) +   
  geom_bar(aes(fill = enjoy_pe), position = "dodge", stat="identity") + 
  labs(x = "Weight Category by BMI ",
       y = "Proportion",
       fill = "Enjoyment of PE",
       title = "Proportion of Adolescents Who Enjoy PE by Weight Category") +
  theme(plot.title = element_text(hjust = 0.5))


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
       y = "Proportion",
       fill = "Correct Perception",
       title = "Proportion of Adolescents Whose Weight Perception Matched BMI Category\nby Enjoyment of PE") +
  theme(plot.title = element_text(hjust = 0.5))
