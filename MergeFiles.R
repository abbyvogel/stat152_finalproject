library(Hmisc)
demographics <- sasxport.get("~/Downloads/DEMO_H.XPT")
bmi <- sasxport.get("~/Downloads/BMX_H.XPT")
activity <- sasxport.get("~/Downloads/PAQ_H.XPT")
weights <- sasxport.get("~/Downloads/WHQMEC_H.XPT")

library(plyr)
library(dplyr)

list_dfs <- list(demographics, bmi, activity, weights)

full_data <- join_all(list_dfs) %>% 
  select(seqn, riagendr, ridageyr, bmdbmic, whq030m, whq500, paq744, paq746, paq750) %>% 
  filter(ridageyr %in% c(12:15)) %>% 
  dplyr::rename(`id` = seqn, `gender` = riagendr, `age` = ridageyr, `bmi` = bmdbmic, 
                `opinion_wt` = whq030m, `action_wt` = whq500, pe_yn = paq744, 
                freq_pe = paq746, enjoy_pe = paq750) %>% 
  mutate(gender = factor(gender, levels = 1:2, labels = c("M", "F")),
         bmi = factor(bmi, levels = 1:4, 
                      labels = c("underweight", "normal", "overweight", "obese")),
         opinion_wt = factor(opinion_wt, levels = c(1,2,3,7,9), 
                             labels = c("overweight", "underweight", "normal", 
                                        "refused", "unsure")),
         action_wt = factor(action_wt, levels = c(1,2,3,4,7,9), 
                            labels = c("lose", "gain", "maintain", 
                                       "nothing", "refused", "unsure")),
         pe_yn = factor(pe_yn, levels = c(1,2,7,9), labels = c("yes", "no", "refused", "unsure")),
         enjoy_pe = factor(enjoy_pe, levels = c(1,2,3,4,5,7,9), 
                           labels = c("strongly agree", "agree", "neither agree nor disagree", 
                                      "disagree", "strongly disagree", "refused", "unsure"))) %>% 
  mutate(freq_pe = ifelse(pe_yn == "no", 0, freq_pe))
  

#Fix missing values, throw out units with more than four missing responses.
full_data$bmi[c(552, 654, 421, 422)] <- "normal"
full_data = full_data[complete.cases(full_data), ]

write.csv(full_data, "~/Desktop/Stat152/FinalProject/full_data_v2.csv")



