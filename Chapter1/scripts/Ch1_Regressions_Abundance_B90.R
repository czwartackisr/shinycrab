library(dplyr)
library(tidyr)
library(ggplot2)
library(PerformanceAnalytics)
library(MuMIn)
library(MASS)

# Read in data ------------------------------------------------------------

crab <- read.csv("./Chapter1/data/CH1data.csv", stringsAsFactors = FALSE)


###B90_CPUE~All

# 1yr.
B90Abun_lm1 <- lm(crab$B90_CPUE ~ lag(crab$B90_JuvCPUE))
summary(B90Abun_lm1) 
B90Abun_lm2 <- lm(crab$B90_CPUE ~ lag(crab$B90_SubadultCPUE))
summary(B90Abun_lm2) 
B90Abun_lm3 <- lm(crab$B90_CPUE ~ lag(crab$B90_AdultCPUE))
summary(B90Abun_lm3) 
B90Abun_lm4 <- lm(crab$B90_CPUE ~ lag(crab$B90_ImmatureFemaleCPUE))
summary(B90Abun_lm4) 
B90Abun_lm5 <- lm(crab$B90_CPUE ~ lag(crab$B90_ImmatureMaleCPUE))
summary(B90Abun_lm5) 
B90Abun_lm6 <- lm(crab$B90_CPUE ~ lag(crab$B90_MatureFemaleCPUE))
summary(B90Abun_lm6)
B90Abun_lm7 <- lm(crab$B90_CPUE ~ lag(crab$B90_MatureMaleCPUE))
summary(B90Abun_lm7) 
B90Abun_lm8 <- lm(crab$B90_CPUE ~ lag(crab$B90_SublegalCPUE))
summary(B90Abun_lm8) 
B90Abun_lm9 <- lm(crab$B90_CPUE ~ lag(crab$B90_LegalCPUE))
summary(B90Abun_lm9) 
B90Abun_lm10 <- lm(crab$B90_CPUE ~ lag(crab$B90_CPUE))
summary(B90Abun_lm10) 


# 2yr.
B90AbunLead_lm1 <- lm(lead(crab$B90_CPUE) ~ lag(crab$B90_JuvCPUE))
summary(B90AbunLead_lm1) 
B90AbunLead_lm2 <- lm(lead(crab$B90_CPUE) ~ lag(crab$B90_SubadultCPUE))
summary(B90AbunLead_lm2) 
B90AbunLead_lm3 <- lm(lead(crab$B90_CPUE) ~ lag(crab$B90_AdultCPUE))
summary(B90AbunLead_lm3) 
B90AbunLead_lm4 <- lm(lead(crab$B90_CPUE) ~ lag(crab$B90_ImmatureFemaleCPUE))
summary(B90AbunLead_lm4) 
B90AbunLead_lm5 <- lm(lead(crab$B90_CPUE) ~ lag(crab$B90_ImmatureMaleCPUE))
summary(B90AbunLead_lm5) 
B90AbunLead_lm6 <- lm(lead(crab$B90_CPUE) ~ lag(crab$B90_MatureFemaleCPUE))
summary(B90AbunLead_lm6) 
B90AbunLead_lm7 <- lm(lead(crab$B90_CPUE) ~ lag(crab$B90_MatureMaleCPUE))
summary(B90AbunLead_lm7) 
B90AbunLead_lm8 <- lm(lead(crab$B90_CPUE) ~ lag(crab$B90_SublegalCPUE))
summary(B90AbunLead_lm8) 
B90AbunLead_lm9 <- lm(lead(crab$B90_CPUE) ~ lag(crab$B90_LegalCPUE))
summary(B90AbunLead_lm9) 
B90AbunLead_lm10 <- lm(lead(crab$B90_CPUE) ~ lag(crab$B90_CPUE))
summary(B90AbunLead_lm10) 




###Adult ~ All

# 1yr.
B90Abun_lm1 <- lm(crab$B90_AdultCPUE ~ lag(crab$B90_JuvCPUE))
summary(B90Abun_lm1) 
B90Abun_lm2 <- lm(crab$B90_AdultCPUE ~ lag(crab$B90_SubadultCPUE))
summary(B90Abun_lm2) 
B90Abun_lm3 <- lm(crab$B90_AdultCPUE ~ lag(crab$B90_AdultCPUE))
summary(B90Abun_lm3) 
B90Abun_lm4 <- lm(crab$B90_AdultCPUE ~ lag(crab$B90_ImmatureFemaleCPUE))
summary(B90Abun_lm4) 
B90Abun_lm5 <- lm(crab$B90_AdultCPUE ~ lag(crab$B90_ImmatureMaleCPUE))
summary(B90Abun_lm5) 
B90Abun_lm6 <- lm(crab$B90_AdultCPUE ~ lag(crab$B90_MatureFemaleCPUE))
summary(B90Abun_lm6)
B90Abun_lm7 <- lm(crab$B90_AdultCPUE ~ lag(crab$B90_MatureMaleCPUE))
summary(B90Abun_lm7) 
B90Abun_lm8 <- lm(crab$B90_AdultCPUE ~ lag(crab$B90_SublegalCPUE))
summary(B90Abun_lm8) 
B90Abun_lm9 <- lm(crab$B90_AdultCPUE ~ lag(crab$B90_LegalCPUE))
summary(B90Abun_lm9) 
B90Abun_lm10 <- lm(crab$B90_AdultCPUE ~ lag(crab$B90_CPUE))
summary(B90Abun_lm10) 


# 2yr.
B90AbunLead_lm1 <- lm(lead(crab$B90_AdultCPUE) ~ lag(crab$B90_JuvCPUE))
summary(B90AbunLead_lm1)
B90AbunLead_lm2 <- lm(lead(crab$B90_AdultCPUE) ~ lag(crab$B90_SubadultCPUE))
summary(B90AbunLead_lm2) 
B90AbunLead_lm3 <- lm(lead(crab$B90_AdultCPUE) ~ lag(crab$B90_AdultCPUE))
summary(B90AbunLead_lm3) 
B90AbunLead_lm4 <- lm(lead(crab$B90_AdultCPUE) ~ lag(crab$B90_ImmatureFemaleCPUE))
summary(B90AbunLead_lm4)
B90AbunLead_lm5 <- lm(lead(crab$B90_AdultCPUE) ~ lag(crab$B90_ImmatureMaleCPUE))
summary(B90AbunLead_lm5) 
B90AbunLead_lm6 <- lm(lead(crab$B90_AdultCPUE) ~ lag(crab$B90_MatureFemaleCPUE))
summary(B90AbunLead_lm6) 
B90AbunLead_lm7 <- lm(lead(crab$B90_AdultCPUE) ~ lag(crab$B90_MatureMaleCPUE))
summary(B90AbunLead_lm7) 
B90AbunLead_lm8 <- lm(lead(crab$B90_AdultCPUE) ~ lag(crab$B90_SublegalCPUE))
summary(B90AbunLead_lm8) 
B90AbunLead_lm9 <- lm(lead(crab$B90_AdultCPUE) ~ lag(crab$B90_LegalCPUE))
summary(B90AbunLead_lm9) 
B90AbunLead_lm10 <- lm(lead(crab$B90_AdultCPUE) ~ lag(crab$B90_CPUE))
summary(B90AbunLead_lm10)




###Subdult ~ All

# 1yr.
B90Abun_lm1 <- lm(crab$B90_SubadultCPUE ~ lag(crab$B90_JuvCPUE))
summary(B90Abun_lm1) 
B90Abun_lm2 <- lm(crab$B90_SubadultCPUE ~ lag(crab$B90_SubadultCPUE))
summary(B90Abun_lm2) 
B90Abun_lm3 <- lm(crab$B90_SubadultCPUE ~ lag(crab$B90_AdultCPUE))
summary(B90Abun_lm3) 
B90Abun_lm4 <- lm(crab$B90_SubadultCPUE ~ lag(crab$B90_ImmatureFemaleCPUE))
summary(B90Abun_lm4) 
B90Abun_lm5 <- lm(crab$B90_SubadultCPUE ~ lag(crab$B90_ImmatureMaleCPUE))
summary(B90Abun_lm5) 
B90Abun_lm6 <- lm(crab$B90_SubadultCPUE ~ lag(crab$B90_MatureFemaleCPUE))
summary(B90Abun_lm6)
B90Abun_lm7 <- lm(crab$B90_SubadultCPUE ~ lag(crab$B90_MatureMaleCPUE))
summary(B90Abun_lm7) 
B90Abun_lm8 <- lm(crab$B90_SubadultCPUE ~ lag(crab$B90_SublegalCPUE))
summary(B90Abun_lm8) 
B90Abun_lm9 <- lm(crab$B90_SubadultCPUE ~ lag(crab$B90_LegalCPUE))
summary(B90Abun_lm9) 
B90Abun_lm10 <- lm(crab$B90_SubadultCPUE ~ lag(crab$B90_CPUE))
summary(B90Abun_lm10) 

# 2yr.
B90AbunLead_lm1 <- lm(lead(crab$B90_SubadultCPUE) ~ lag(crab$B90_JuvCPUE))
summary(B90AbunLead_lm1) 
B90AbunLead_lm2 <- lm(lead(crab$B90_SubadultCPUE) ~ lag(crab$B90_SubadultCPUE))
summary(B90AbunLead_lm2) 
B90AbunLead_lm3 <- lm(lead(crab$B90_SubadultCPUE) ~ lag(crab$B90_AdultCPUE))
summary(B90AbunLead_lm3) 
B90AbunLead_lm4 <- lm(lead(crab$B90_SubadultCPUE) ~ lag(crab$B90_ImmatureFemaleCPUE))
summary(B90AbunLead_lm4) 
B90AbunLead_lm5 <- lm(lead(crab$B90_SubadultCPUE) ~ lag(crab$B90_ImmatureMaleCPUE))
summary(B90AbunLead_lm5) 
B90AbunLead_lm6 <- lm(lead(crab$B90_SubadultCPUE) ~ lag(crab$B90_MatureFemaleCPUE))
summary(B90AbunLead_lm6) 
B90AbunLead_lm7 <- lm(lead(crab$B90_SubadultCPUE) ~ lag(crab$B90_MatureMaleCPUE))
summary(B90AbunLead_lm7) 
B90AbunLead_lm8 <- lm(lead(crab$B90_SubadultCPUE) ~ lag(crab$B90_SublegalCPUE))
summary(B90AbunLead_lm8) 
B90AbunLead_lm9 <- lm(lead(crab$B90_SubadultCPUE) ~ lag(crab$B90_LegalCPUE))
summary(B90AbunLead_lm9) 
B90AbunLead_lm10 <- lm(lead(crab$B90_SubadultCPUE) ~ lag(crab$B90_CPUE))
summary(B90AbunLead_lm10) 




###Juv ~ All

# 1yr.
B90Abun_lm1 <- lm(crab$B90_JuvCPUE ~ lag(crab$B90_JuvCPUE))
summary(B90Abun_lm1) # p-value: 0.008705 - Multiple R-squared:  0.1762
B90Abun_lm2 <- lm(crab$B90_JuvCPUE ~ lag(crab$B90_SubadultCPUE))
summary(B90Abun_lm2) #  p-value: 0.003395 - Multiple R-squared:  0.2147
B90Abun_lm3 <- lm(crab$B90_JuvCPUE ~ lag(crab$B90_AdultCPUE))
summary(B90Abun_lm3) 
B90Abun_lm4 <- lm(crab$B90_JuvCPUE ~ lag(crab$B90_ImmatureFemaleCPUE))
summary(B90Abun_lm4) # p-value: 0.009853 - Multiple R-squared:  0.171
B90Abun_lm5 <- lm(crab$B90_JuvCPUE ~ lag(crab$B90_ImmatureMaleCPUE))
summary(B90Abun_lm5) # p-value: 0.002073 - Multiple R-squared:  0.2344
B90Abun_lm6 <- lm(crab$B90_JuvCPUE ~ lag(crab$B90_MatureFemaleCPUE))
summary(B90Abun_lm6)
B90Abun_lm7 <- lm(crab$B90_JuvCPUE ~ lag(crab$B90_MatureMaleCPUE))
summary(B90Abun_lm7) 
B90Abun_lm8 <- lm(crab$B90_JuvCPUE ~ lag(crab$B90_SublegalCPUE))
summary(B90Abun_lm8) # p-value: 0.002014 - Multiple R-squared:  0.2355
B90Abun_lm9 <- lm(crab$B90_JuvCPUE ~ lag(crab$B90_LegalCPUE))
summary(B90Abun_lm9) 
B90Abun_lm10 <- lm(crab$B90_JuvCPUE ~ lag(crab$B90_CPUE))
summary(B90Abun_lm10) # p-value: 0.03095 - Multiple R-squared:  0.1229

# 2yr.
B90AbunLead_lm1 <- lm(lead(crab$B90_JuvCPUE) ~ lag(crab$B90_JuvCPUE))
summary(B90AbunLead_lm1) 
B90AbunLead_lm2 <- lm(lead(crab$B90_JuvCPUE) ~ lag(crab$B90_SubadultCPUE))
summary(B90AbunLead_lm2) 
B90AbunLead_lm3 <- lm(lead(crab$B90_JuvCPUE) ~ lag(crab$B90_AdultCPUE))
summary(B90AbunLead_lm3) 
B90AbunLead_lm4 <- lm(lead(crab$B90_JuvCPUE) ~ lag(crab$B90_ImmatureFemaleCPUE))
summary(B90AbunLead_lm4) 
B90AbunLead_lm5 <- lm(lead(crab$B90_JuvCPUE) ~ lag(crab$B90_ImmatureMaleCPUE))
summary(B90AbunLead_lm5) 
B90AbunLead_lm6 <- lm(lead(crab$B90_JuvCPUE) ~ lag(crab$B90_MatureFemaleCPUE))
summary(B90AbunLead_lm6) 
B90AbunLead_lm7 <- lm(lead(crab$B90_JuvCPUE) ~ lag(crab$B90_MatureMaleCPUE))
summary(B90AbunLead_lm7) 
B90AbunLead_lm8 <- lm(lead(crab$B90_JuvCPUE) ~ lag(crab$B90_SublegalCPUE))
summary(B90AbunLead_lm8) 
B90AbunLead_lm9 <- lm(lead(crab$B90_JuvCPUE) ~ lag(crab$B90_LegalCPUE))
summary(B90AbunLead_lm9) 
B90AbunLead_lm10 <- lm(lead(crab$B90_JuvCPUE) ~ lag(crab$B90_CPUE))
summary(B90AbunLead_lm10)
