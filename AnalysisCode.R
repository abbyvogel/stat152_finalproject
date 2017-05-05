library(dplyr)
library(survey)

nhanes = read.csv("~/Desktop/Stat152/FinalProject/full_data_v4.csv")[ , -c(1,2)] 
nhanes_design = svydesign(ids = ~psu+id, strata = ~stratum, 
                          nest = TRUE, weights = ~exam_wt, data = nhanes)


#Proportion in Each Weight Category by Frequency of PE
svymean(~interaction(freq_pe, bmi), design = nhanes_design)

#Perceived and Actual Weight Category by Frequency of PE
svymean(~interaction(freq_pe, bmi, opinion_wt), design = nhanes_design)




