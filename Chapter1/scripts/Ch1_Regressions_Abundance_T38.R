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
summary(T38Abun_lm2) # 0.007774 - 0.1809
T38Abun_lm3 <- lm(crab$T38_CPUE ~ lag(crab$T38_AdultCPUE))
summary(T38Abun_lm3) # 0.0312 - 0.1225
T38Abun_lm4 <- lm(crab$T38_CPUE ~ lag(crab$T38_ImmatureFemaleCPUE))
summary(T38Abun_lm4) # 0.05007 - 0.1025
T38Abun_lm5 <- lm(crab$T38_CPUE ~ lag(crab$T38_ImmatureMaleCPUE))
summary(T38Abun_lm5) # 0.04854 - 0.1038
T38Abun_lm6 <- lm(crab$T38_CPUE ~ lag(crab$T38_MatureFemaleCPUE))
summary(T38Abun_lm6)
T38Abun_lm7 <- lm(crab$T38_CPUE ~ lag(crab$T38_MatureMaleCPUE))
summary(T38Abun_lm7) # 0.002197 - 0.2321
T38Abun_lm8 <- lm(crab$T38_CPUE ~ lag(crab$T38_SublegalCPUE))
summary(T38Abun_lm8) # 0.0252 - 0.1314
T38Abun_lm9 <- lm(crab$T38_CPUE ~ lag(crab$T38_LegalCPUE))
summary(T38Abun_lm9) # 0.0312 - 0.1225
T38Abun_lm10 <- lm(crab$T38_CPUE ~ lag(crab$T38_CPUE))
summary(T38Abun_lm10) # 0.01906 - 0.1434


# 2yr.
T38AbunLead_lm1 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_JuvCPUE))
summary(T38AbunLead_lm1) #  0.03282 - 0.1237
T38AbunLead_lm2 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_SubadultCPUE))
summary(T38AbunLead_lm2) # 0.001926 - 0.2432
T38AbunLead_lm3 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_AdultCPUE))
summary(T38AbunLead_lm3) 
T38AbunLead_lm4 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_ImmatureFemaleCPUE))
summary(T38AbunLead_lm4) # 0.01038 - 0.1733
T38AbunLead_lm5 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_ImmatureMaleCPUE))
summary(T38AbunLead_lm5) # 0.004023 - 0.2131
T38AbunLead_lm6 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_MatureFemaleCPUE))
summary(T38AbunLead_lm6) # 0.019 - 0.1473
T38AbunLead_lm7 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_MatureMaleCPUE))
summary(T38AbunLead_lm7) # 0.03076 - 0.1265
T38AbunLead_lm8 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_SublegalCPUE))
summary(T38AbunLead_lm8) # 0.004317 - 0.2102
T38AbunLead_lm9 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_LegalCPUE))
summary(T38AbunLead_lm9) 
T38AbunLead_lm10 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_CPUE))
summary(T38AbunLead_lm10) # 0.004898 - 0.205




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
summary(T38Abun_lm7) #  p-value: 0.01417; Multiple R-squared:  0.1558
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
summary(T38AbunLead_lm6) #p-value: 0.03677l; Multiple R-squared:  0.1187
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
