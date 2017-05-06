library(dplyr)
library(survey)

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

#Proportion in Each Weight Category by Existence of PE
svymean(~interaction(pe_yn, bmi), design = nhanes_design)
bmi_tbl = svytable(~pe_yn+bmi, nhanes_design)
bmi_prop_tbl = round(prop.table(bmi_tbl), 5)
summary(bmi_tbl, statistic = "F")
svychisq(~pe_yn+bmi, nhanes_design, statistic = "F")

#Perceived and Actual Weight Category by Frequency of PE
svymean(~interaction(freq_pe, bmi, opinion_wt), design = nhanes_design)
perception_tbl = svytable(~freq_pe+correct_perception, nhanes_design)
perception_prop_tbl = round(prop.table(perception_tbl), 5)
summary(perception_tbl, statistic = "F")
svychisq(~freq_pe+correct_perception, nhanes_design, statistic = "F")

#Perceived and Actual Weight Category by Existence of PE
svymean(~interaction(pe_yn, bmi, opinion_wt), design = nhanes_design)
perception_tbl = svytable(~pe_yn+correct_perception, nhanes_design)
perception_prop_tbl = round(prop.table(perception_tbl), 5)
summary(perception_tbl, statistic = "F")
svychisq(~pe_yn+correct_perception, nhanes_design, statistic = "F")

#Proportion in Each Weight Category by Enjoyment of PE
svymean(~interaction(enjoy_pe, bmi), design = nhanes_design)
bmi_tbl = svytable(~enjoy_pe+bmi, nhanes_design)
bmi_prop_tbl = round(prop.table(bmi_tbl), 5)
summary(bmi_tbl, statistic = "F")
svychisq(~enjoy_pe+bmi, nhanes_design, statistic = "F")

#Perceived and Actual Weight Category by Enjoyment of PE
svymean(~interaction(enjoy_pe, bmi, opinion_wt), design = nhanes_design)
perception_tbl = svytable(~enjoy_pe+correct_perception, nhanes_design)
perception_prop_tbl = round(prop.table(perception_tbl), 5)
summary(perception_tbl, statistic = "F")
svychisq(~enjoy_pe+correct_perception, nhanes_design, statistic = "F")
