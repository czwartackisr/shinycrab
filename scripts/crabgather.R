library(dplyr)
library(xls)
crab = read.csv("data.csv")
crab = crab %>% select(1:68, 70:93) 

lmcrab = crab %>%
  


lmList(B90_CPUE~.|survey, data = na.omit(crab))

?lm
