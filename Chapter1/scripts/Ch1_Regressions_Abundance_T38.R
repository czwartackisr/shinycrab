library(dplyr)
library(tidyr)
library(ggplot2)
library(PerformanceAnalytics)
library(MuMIn)
library(MASS)

# Read in data ------------------------------------------------------------

crab <- read.csv("./Chapter1/data/CH1data.csv", stringsAsFactors = FALSE)


###T38_CPUE~All

# 1yr.
T38Abun_lm1 <- lm(crab$T38_CPUE ~ lag(crab$T38_JuvCPUE))
summary(T38Abun_lm1) 
T38Abun_lm2 <- lm(crab$T38_CPUE ~ lag(crab$T38_SubadultCPUE))
summary(T38Abun_lm2) 
T38Abun_lm3 <- lm(crab$T38_CPUE ~ lag(crab$T38_AdultCPUE))
summary(T38Abun_lm3) 
T38Abun_lm4 <- lm(crab$T38_CPUE ~ lag(crab$T38_ImmatureFemaleCPUE))
summary(T38Abun_lm4) 
T38Abun_lm5 <- lm(crab$T38_CPUE ~ lag(crab$T38_ImmatureMaleCPUE))
summary(T38Abun_lm5) 
T38Abun_lm6 <- lm(crab$T38_CPUE ~ lag(crab$T38_MatureFemaleCPUE))
summary(T38Abun_lm6)
T38Abun_lm7 <- lm(crab$T38_CPUE ~ lag(crab$T38_MatureMaleCPUE))
summary(T38Abun_lm7) # 0.005966 - 0.1918
T38Abun_lm8 <- lm(crab$T38_CPUE ~ lag(crab$T38_SublegalCPUE))
summary(T38Abun_lm8) 
T38Abun_lm9 <- lm(crab$T38_CPUE ~ lag(crab$T38_LegalCPUE))
summary(T38Abun_lm9) 
T38Abun_lm10 <- lm(crab$T38_CPUE ~ lag(crab$T38_CPUE))
summary(T38Abun_lm10) 


# 2yr.
T38AbunLead_lm1 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_JuvCPUE))
summary(T38AbunLead_lm1) 
T38AbunLead_lm2 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_SubadultCPUE))
summary(T38AbunLead_lm2) # 0.0004261 - 0.3021
T38AbunLead_lm3 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_AdultCPUE))
summary(T38AbunLead_lm3) 
T38AbunLead_lm4 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_ImmatureFemaleCPUE))
summary(T38AbunLead_lm4) 
T38AbunLead_lm5 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_ImmatureMaleCPUE))
summary(T38AbunLead_lm5) # 0.0003695 - 0.3074
T38AbunLead_lm6 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_MatureFemaleCPUE))
summary(T38AbunLead_lm6) 
T38AbunLead_lm7 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_MatureMaleCPUE))
summary(T38AbunLead_lm7) 
T38AbunLead_lm8 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_SublegalCPUE))
summary(T38AbunLead_lm8) # 0.001162 - 0.2634
T38AbunLead_lm9 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_LegalCPUE))
summary(T38AbunLead_lm9) 
T38AbunLead_lm10 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_CPUE))
summary(T38AbunLead_lm10) # 0.001152 - 0.2637




###Adult ~ All

# 1yr.
T38Abun_lm1 <- lm(crab$T38_AdultCPUE ~ lag(crab$T38_JuvCPUE))
summary(T38Abun_lm1) 
T38Abun_lm2 <- lm(crab$T38_AdultCPUE ~ lag(crab$T38_SubadultCPUE))
summary(T38Abun_lm2) 
T38Abun_lm3 <- lm(crab$T38_AdultCPUE ~ lag(crab$T38_AdultCPUE))
summary(T38Abun_lm3) 
T38Abun_lm4 <- lm(crab$T38_AdultCPUE ~ lag(crab$T38_ImmatureFemaleCPUE))
summary(T38Abun_lm4) 
T38Abun_lm5 <- lm(crab$T38_AdultCPUE ~ lag(crab$T38_ImmatureMaleCPUE))
summary(T38Abun_lm5) 
T38Abun_lm6 <- lm(crab$T38_AdultCPUE ~ lag(crab$T38_MatureFemaleCPUE))
summary(T38Abun_lm6)
T38Abun_lm7 <- lm(crab$T38_AdultCPUE ~ lag(crab$T38_MatureMaleCPUE))
summary(T38Abun_lm7) 
T38Abun_lm8 <- lm(crab$T38_AdultCPUE ~ lag(crab$T38_SublegalCPUE))
summary(T38Abun_lm8) 
T38Abun_lm9 <- lm(crab$T38_AdultCPUE ~ lag(crab$T38_LegalCPUE))
summary(T38Abun_lm9) 
T38Abun_lm10 <- lm(crab$T38_AdultCPUE ~ lag(crab$T38_CPUE))
summary(T38Abun_lm10) 


# 2yr.
T38AbunLead_lm1 <- lm(lead(crab$T38_AdultCPUE) ~ lag(crab$T38_JuvCPUE))
summary(T38AbunLead_lm1)
T38AbunLead_lm2 <- lm(lead(crab$T38_AdultCPUE) ~ lag(crab$T38_SubadultCPUE))
summary(T38AbunLead_lm2) 
T38AbunLead_lm3 <- lm(lead(crab$T38_AdultCPUE) ~ lag(crab$T38_AdultCPUE))
summary(T38AbunLead_lm3) 
T38AbunLead_lm4 <- lm(lead(crab$T38_AdultCPUE) ~ lag(crab$T38_ImmatureFemaleCPUE))
summary(T38AbunLead_lm4)
T38AbunLead_lm5 <- lm(lead(crab$T38_AdultCPUE) ~ lag(crab$T38_ImmatureMaleCPUE))
summary(T38AbunLead_lm5) 
T38AbunLead_lm6 <- lm(lead(crab$T38_AdultCPUE) ~ lag(crab$T38_MatureFemaleCPUE))
summary(T38AbunLead_lm6) 
T38AbunLead_lm7 <- lm(lead(crab$T38_AdultCPUE) ~ lag(crab$T38_MatureMaleCPUE))
summary(T38AbunLead_lm7) 
T38AbunLead_lm8 <- lm(lead(crab$T38_AdultCPUE) ~ lag(crab$T38_SublegalCPUE))
summary(T38AbunLead_lm8) 
T38AbunLead_lm9 <- lm(lead(crab$T38_AdultCPUE) ~ lag(crab$T38_LegalCPUE))
summary(T38AbunLead_lm9) 
T38AbunLead_lm10 <- lm(lead(crab$T38_AdultCPUE) ~ lag(crab$T38_CPUE))
summary(T38AbunLead_lm10)




###Subdult ~ All

# 1yr.
T38Abun_lm1 <- lm(crab$T38_SubadultCPUE ~ lag(crab$T38_JuvCPUE))
summary(T38Abun_lm1) 
T38Abun_lm2 <- lm(crab$T38_SubadultCPUE ~ lag(crab$T38_SubadultCPUE))
summary(T38Abun_lm2) #  p-value: 0.006454 - Multiple R-squared:  0.1885
T38Abun_lm3 <- lm(crab$T38_SubadultCPUE ~ lag(crab$T38_AdultCPUE))
summary(T38Abun_lm3) 
T38Abun_lm4 <- lm(crab$T38_SubadultCPUE ~ lag(crab$T38_ImmatureFemaleCPUE))
summary(T38Abun_lm4) 
T38Abun_lm5 <- lm(crab$T38_SubadultCPUE ~ lag(crab$T38_ImmatureMaleCPUE))
summary(T38Abun_lm5) # p-value: 0.04906 - Multiple R-squared:  0.1033
T38Abun_lm6 <- lm(crab$T38_SubadultCPUE ~ lag(crab$T38_MatureFemaleCPUE))
summary(T38Abun_lm6)
T38Abun_lm7 <- lm(crab$T38_SubadultCPUE ~ lag(crab$T38_MatureMaleCPUE))
summary(T38Abun_lm7) # p-value: 0.002539 -  Multiple R-squared:  0.2263
T38Abun_lm8 <- lm(crab$T38_SubadultCPUE ~ lag(crab$T38_SublegalCPUE))
summary(T38Abun_lm8) # p-value: 0.02708 - Multiple R-squared:  0.1285
T38Abun_lm9 <- lm(crab$T38_SubadultCPUE ~ lag(crab$T38_LegalCPUE))
summary(T38Abun_lm9) 
T38Abun_lm10 <- lm(crab$T38_SubadultCPUE ~ lag(crab$T38_CPUE))
summary(T38Abun_lm10) # p-value: 0.02422 - Multiple R-squared:  0.1333

# 2yr.
T38AbunLead_lm1 <- lm(lead(crab$T38_SubadultCPUE) ~ lag(crab$T38_JuvCPUE))
summary(T38AbunLead_lm1) # p-value: 0.01327 - Multiple R-squared:  0.1628 
T38AbunLead_lm2 <- lm(lead(crab$T38_SubadultCPUE) ~ lag(crab$T38_SubadultCPUE))
summary(T38AbunLead_lm2) # p-value: 0.001455 - Multiple R-squared:  0.2544
T38AbunLead_lm3 <- lm(lead(crab$T38_SubadultCPUE) ~ lag(crab$T38_AdultCPUE))
summary(T38AbunLead_lm3) 
T38AbunLead_lm4 <- lm(lead(crab$T38_SubadultCPUE) ~ lag(crab$T38_ImmatureFemaleCPUE))
summary(T38AbunLead_lm4) # p-value: 0.004265 - Multiple R-squared:  0.2107
T38AbunLead_lm5 <- lm(lead(crab$T38_SubadultCPUE) ~ lag(crab$T38_ImmatureMaleCPUE))
summary(T38AbunLead_lm5) # p-value: 0.002316 - Multiple R-squared:  0.2358
T38AbunLead_lm6 <- lm(lead(crab$T38_SubadultCPUE) ~ lag(crab$T38_MatureFemaleCPUE))
summary(T38AbunLead_lm6) # p-value: 0.02803 - Multiple R-squared:  0.1305
T38AbunLead_lm7 <- lm(lead(crab$T38_SubadultCPUE) ~ lag(crab$T38_MatureMaleCPUE))
summary(T38AbunLead_lm7) # p-value: 0.04762 - Multiple R-squared:  0.1075
T38AbunLead_lm8 <- lm(lead(crab$T38_SubadultCPUE) ~ lag(crab$T38_SublegalCPUE))
summary(T38AbunLead_lm8) # p-value: 0.002236 - Multiple R-squared:  0.2372
T38AbunLead_lm9 <- lm(lead(crab$T38_SubadultCPUE) ~ lag(crab$T38_LegalCPUE))
summary(T38AbunLead_lm9) 
T38AbunLead_lm10 <- lm(lead(crab$T38_SubadultCPUE) ~ lag(crab$T38_CPUE))
summary(T38AbunLead_lm10) #  p-value: 0.003555 - Multiple R-squared:  0.2183




###Juv ~ All

# 1yr.
T38Abun_lm1 <- lm(crab$T38_JuvCPUE ~ lag(crab$T38_JuvCPUE))
summary(T38Abun_lm1) 
T38Abun_lm2 <- lm(crab$T38_JuvCPUE ~ lag(crab$T38_SubadultCPUE))
summary(T38Abun_lm2) #  p-value: 0.02818 - Multiple R-squared:  0.1269
T38Abun_lm3 <- lm(crab$T38_JuvCPUE ~ lag(crab$T38_AdultCPUE))
summary(T38Abun_lm3) # p-value: 0.02734 - Multiple R-squared:  0.1281
T38Abun_lm4 <- lm(crab$T38_JuvCPUE ~ lag(crab$T38_ImmatureFemaleCPUE))
summary(T38Abun_lm4) 
T38Abun_lm5 <- lm(crab$T38_JuvCPUE ~ lag(crab$T38_ImmatureMaleCPUE))
summary(T38Abun_lm5) 
T38Abun_lm6 <- lm(crab$T38_JuvCPUE ~ lag(crab$T38_MatureFemaleCPUE))
summary(T38Abun_lm6)
T38Abun_lm7 <- lm(crab$T38_JuvCPUE ~ lag(crab$T38_MatureMaleCPUE))
summary(T38Abun_lm7) # p-value: 0.01211 -  Multiple R-squared:  0.1624
T38Abun_lm8 <- lm(crab$T38_JuvCPUE ~ lag(crab$T38_SublegalCPUE))
summary(T38Abun_lm8) # p-value: 0.04467 - Multiple R-squared:  0.1073
T38Abun_lm9 <- lm(crab$T38_JuvCPUE ~ lag(crab$T38_LegalCPUE))
summary(T38Abun_lm9) # p-value: 0.02734 - Multiple R-squared:  0.1281
T38Abun_lm10 <- lm(crab$T38_JuvCPUE ~ lag(crab$T38_CPUE))
summary(T38Abun_lm10) # p-value: 0.03064 - Multiple R-squared:  0.1233

# 2yr.
T38AbunLead_lm1 <- lm(lead(crab$T38_JuvCPUE) ~ lag(crab$T38_JuvCPUE))
summary(T38AbunLead_lm1) 
T38AbunLead_lm2 <- lm(lead(crab$T38_JuvCPUE) ~ lag(crab$T38_SubadultCPUE))
summary(T38AbunLead_lm2) # p-value: 0.00132 - Multiple R-squared:  0.2583
T38AbunLead_lm3 <- lm(lead(crab$T38_JuvCPUE) ~ lag(crab$T38_AdultCPUE))
summary(T38AbunLead_lm3) 
T38AbunLead_lm4 <- lm(lead(crab$T38_JuvCPUE) ~ lag(crab$T38_ImmatureFemaleCPUE))
summary(T38AbunLead_lm4) # p-value: 0.02453 - Multiple R-squared:  0.1363
T38AbunLead_lm5 <- lm(lead(crab$T38_JuvCPUE) ~ lag(crab$T38_ImmatureMaleCPUE))
summary(T38AbunLead_lm5) # p-value: 0.003867 - Multiple R-squared:  0.2148
T38AbunLead_lm6 <- lm(lead(crab$T38_JuvCPUE) ~ lag(crab$T38_MatureFemaleCPUE))
summary(T38AbunLead_lm6) # p-value: 0.04709 - Multiple R-squared:  0.108
T38AbunLead_lm7 <- lm(lead(crab$T38_JuvCPUE) ~ lag(crab$T38_MatureMaleCPUE))
summary(T38AbunLead_lm7) # p-value: 0.03325 - Multiple R-squared:  0.1231
T38AbunLead_lm8 <- lm(lead(crab$T38_JuvCPUE) ~ lag(crab$T38_SublegalCPUE))
summary(T38AbunLead_lm8) # p-value: 0.006612 - Multiple R-squared:  0.1924
T38AbunLead_lm9 <- lm(lead(crab$T38_JuvCPUE) ~ lag(crab$T38_LegalCPUE))
summary(T38AbunLead_lm9) 
T38AbunLead_lm10 <- lm(lead(crab$T38_JuvCPUE) ~ lag(crab$T38_CPUE))
summary(T38AbunLead_lm10) # p-value: 0.006525 - Multiple R-squared:  0.193
