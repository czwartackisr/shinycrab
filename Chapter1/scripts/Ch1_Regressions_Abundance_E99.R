library(dplyr)
library(tidyr)
library(ggplot2)
library(PerformanceAnalytics)
library(MuMIn)
library(MASS)

# Read in data ------------------------------------------------------------

crab <- read.csv("./Chapter1/data/CH1data.csv", stringsAsFactors = FALSE)


###E99_CPUE~All

# 1yr.
E99Abun_lm1 <- lm(crab$E99_CPUE ~ lag(crab$E99_JuvCPUE))
summary(E99Abun_lm1) 
E99Abun_lm2 <- lm(crab$E99_CPUE ~ lag(crab$E99_SubadultCPUE))
summary(E99Abun_lm2) 
E99Abun_lm3 <- lm(crab$E99_CPUE ~ lag(crab$E99_AdultCPUE))
summary(E99Abun_lm3) 
E99Abun_lm8 <- lm(crab$E99_CPUE ~ lag(crab$E99_SublegalCPUE))
summary(E99Abun_lm8) 
E99Abun_lm9 <- lm(crab$E99_CPUE ~ lag(crab$E99_LegalCPUE))
summary(E99Abun_lm9) 
E99Abun_lm10 <- lm(crab$E99_CPUE ~ lag(crab$E99_CPUE))
summary(E99Abun_lm10) 


# 2yr.
E99AbunLead_lm1 <- lm(lead(crab$E99_CPUE) ~ lag(crab$E99_JuvCPUE))
summary(E99AbunLead_lm1) 
E99AbunLead_lm2 <- lm(lead(crab$E99_CPUE) ~ lag(crab$E99_SubadultCPUE))
summary(E99AbunLead_lm2) 
E99AbunLead_lm3 <- lm(lead(crab$E99_CPUE) ~ lag(crab$E99_AdultCPUE))
summary(E99AbunLead_lm3) 
E99AbunLead_lm8 <- lm(lead(crab$E99_CPUE) ~ lag(crab$E99_SublegalCPUE))
summary(E99AbunLead_lm8) 
E99AbunLead_lm9 <- lm(lead(crab$E99_CPUE) ~ lag(crab$E99_LegalCPUE))
summary(E99AbunLead_lm9) 
E99AbunLead_lm10 <- lm(lead(crab$E99_CPUE) ~ lag(crab$E99_CPUE))
summary(E99AbunLead_lm10) 




###Adult ~ All

# 1yr.
E99Abun_lm1 <- lm(crab$E99_AdultCPUE ~ lag(crab$E99_JuvCPUE))
summary(E99Abun_lm1) 
E99Abun_lm2 <- lm(crab$E99_AdultCPUE ~ lag(crab$E99_SubadultCPUE))
summary(E99Abun_lm2) 
E99Abun_lm3 <- lm(crab$E99_AdultCPUE ~ lag(crab$E99_AdultCPUE))
summary(E99Abun_lm3) # p-value: 0.04672 - Multiple R-squared:  0.2385
E99Abun_lm8 <- lm(crab$E99_AdultCPUE ~ lag(crab$E99_SublegalCPUE))
summary(E99Abun_lm8) 
E99Abun_lm9 <- lm(crab$E99_AdultCPUE ~ lag(crab$E99_LegalCPUE))
summary(E99Abun_lm9) # p-value: 0.04672 - Multiple R-squared:  0.2385
E99Abun_lm10 <- lm(crab$E99_AdultCPUE ~ lag(crab$E99_CPUE))
summary(E99Abun_lm10) 


# 2yr.
E99AbunLead_lm1 <- lm(lead(crab$E99_AdultCPUE) ~ lag(crab$E99_JuvCPUE))
summary(E99AbunLead_lm1)
E99AbunLead_lm2 <- lm(lead(crab$E99_AdultCPUE) ~ lag(crab$E99_SubadultCPUE))
summary(E99AbunLead_lm2) 
E99AbunLead_lm3 <- lm(lead(crab$E99_AdultCPUE) ~ lag(crab$E99_AdultCPUE))
summary(E99AbunLead_lm3) 
E99AbunLead_lm8 <- lm(lead(crab$E99_AdultCPUE) ~ lag(crab$E99_SublegalCPUE))
summary(E99AbunLead_lm8) 
E99AbunLead_lm9 <- lm(lead(crab$E99_AdultCPUE) ~ lag(crab$E99_LegalCPUE))
summary(E99AbunLead_lm9) 
E99AbunLead_lm10 <- lm(lead(crab$E99_AdultCPUE) ~ lag(crab$E99_CPUE))
summary(E99AbunLead_lm10) 




###Subdult ~ All

# 1yr.
E99Abun_lm1 <- lm(crab$E99_SubadultCPUE ~ lag(crab$E99_JuvCPUE))
summary(E99Abun_lm1) 
E99Abun_lm2 <- lm(crab$E99_SubadultCPUE ~ lag(crab$E99_SubadultCPUE))
summary(E99Abun_lm2) 
E99Abun_lm3 <- lm(crab$E99_SubadultCPUE ~ lag(crab$E99_AdultCPUE))
summary(E99Abun_lm3) 
E99Abun_lm8 <- lm(crab$E99_SubadultCPUE ~ lag(crab$E99_SublegalCPUE))
summary(E99Abun_lm8) 
E99Abun_lm9 <- lm(crab$E99_SubadultCPUE ~ lag(crab$E99_LegalCPUE))
summary(E99Abun_lm9) 
E99Abun_lm10 <- lm(crab$E99_SubadultCPUE ~ lag(crab$E99_CPUE))
summary(E99Abun_lm10) 

# 2yr.
E99AbunLead_lm1 <- lm(lead(crab$E99_SubadultCPUE) ~ lag(crab$E99_JuvCPUE))
summary(E99AbunLead_lm1) 
E99AbunLead_lm2 <- lm(lead(crab$E99_SubadultCPUE) ~ lag(crab$E99_SubadultCPUE))
summary(E99AbunLead_lm2) 
E99AbunLead_lm3 <- lm(lead(crab$E99_SubadultCPUE) ~ lag(crab$E99_AdultCPUE))
summary(E99AbunLead_lm3) 
E99AbunLead_lm8 <- lm(lead(crab$E99_SubadultCPUE) ~ lag(crab$E99_SublegalCPUE))
summary(E99AbunLead_lm8) 
E99AbunLead_lm9 <- lm(lead(crab$E99_SubadultCPUE) ~ lag(crab$E99_LegalCPUE))
summary(E99AbunLead_lm9) 
E99AbunLead_lm10 <- lm(lead(crab$E99_SubadultCPUE) ~ lag(crab$E99_CPUE))
summary(E99AbunLead_lm10) 




###Juv ~ All

# 1yr.
E99Abun_lm1 <- lm(crab$E99_JuvCPUE ~ lag(crab$E99_JuvCPUE))
summary(E99Abun_lm1) 
E99Abun_lm2 <- lm(crab$E99_JuvCPUE ~ lag(crab$E99_SubadultCPUE))
summary(E99Abun_lm2) 
E99Abun_lm3 <- lm(crab$E99_JuvCPUE ~ lag(crab$E99_AdultCPUE))
summary(E99Abun_lm3) 
E99Abun_lm8 <- lm(crab$E99_JuvCPUE ~ lag(crab$E99_SublegalCPUE))
summary(E99Abun_lm8) 
E99Abun_lm9 <- lm(crab$E99_JuvCPUE ~ lag(crab$E99_LegalCPUE))
summary(E99Abun_lm9) 
E99Abun_lm10 <- lm(crab$E99_JuvCPUE ~ lag(crab$E99_CPUE))
summary(E99Abun_lm10) 

# 2yr.
E99AbunLead_lm1 <- lm(lead(crab$E99_JuvCPUE) ~ lag(crab$E99_JuvCPUE))
summary(E99AbunLead_lm1) 
E99AbunLead_lm2 <- lm(lead(crab$E99_JuvCPUE) ~ lag(crab$E99_SubadultCPUE))
summary(E99AbunLead_lm2) 
E99AbunLead_lm3 <- lm(lead(crab$E99_JuvCPUE) ~ lag(crab$E99_AdultCPUE))
summary(E99AbunLead_lm3) 
E99AbunLead_lm8 <- lm(lead(crab$E99_JuvCPUE) ~ lag(crab$E99_SublegalCPUE))
summary(E99AbunLead_lm8) 
E99AbunLead_lm9 <- lm(lead(crab$E99_JuvCPUE) ~ lag(crab$E99_LegalCPUE))
summary(E99AbunLead_lm9) 
E99AbunLead_lm10 <- lm(lead(crab$E99_JuvCPUE) ~ lag(crab$E99_CPUE))
summary(E99AbunLead_lm10)
