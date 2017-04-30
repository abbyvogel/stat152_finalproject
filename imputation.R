
data3 <- read.csv("~/Desktop/stat152_finalproject/full_data_v3.csv")
set.seed(510)

#########################
# Unit Non-Response
# Remove wt = 0 
#########################

data4 <- data3[-(which(data3$exam_wt==0)),]


#########################
# Item Non-Response
# Random Imputation 
#########################

#rows with NAs
nas<- which(is.na(data4), arr.ind=TRUE)
need_buddy <- unique(nas[,1])

# picks a random row that is same age and gender from complete cases
find_buddy <- function(x){
allpossible <- which(data5$gender==data4$gender[x] & data5$age==data4$age[x])
rand <- sample(allpossible, 1) 
return(rand)
}

buddies<- data.frame(need_buddy, "buddy"=sapply(need_buddy, find_buddy))

for(j in 1:nrow(buddies))
    for(i in seq_along(data4[buddies$need_buddy[j],]))
      if(is.na(data4[buddies$need_buddy[j],][i]))
          data4[buddies$need_buddy[j],][i] <- data4[buddies$buddy[j],][i]

write.csv(data4, "~/Desktop/stat152_finalproject/full_data_v4.csv")

#creates data.frame with indicator of imputed values 

data5 <- data3[-(which(data3$exam_wt==0)),]
mat <- is.na.data.frame(data5)

write.csv(mat, "~/Desktop/stat152_finalproject/imputed.csv")
