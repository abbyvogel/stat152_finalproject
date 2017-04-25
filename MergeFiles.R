library(Hmisc)
demographics <- sasxport.get("~/Downloads/DEMO_H.XPT")
bmi <- sasxport.get("~/Downloads/BMX_H.XPT")
activity <- sasxport.get("~/Downloads/PAQ_H.XPT")
weights <- sasxport.get("~/Downloads/WHQMEC_H.XPT")

library(plyr)
library(dplyr)

list_dfs <- list(demographics, bmi, activity, weights)

full_data <- join_all(list_dfs) %>% 
  select(seqn, ridageyr, bmdbmic, whq030m, whq500, paq744, paq746, paq750, paq755) %>% 
  filter(ridageyr %in% c(12:15))

write.csv(full_data, "~/Desktop/Stat152/FinalProject/full_data.csv")
