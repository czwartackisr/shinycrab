library(dplyr)
library(tidyr)
library(ggplot2)
library(PerformanceAnalytics)
library(MuMIn)
library(MASS)

# Read in data ------------------------------------------------------------

crab <- read.csv("./Chapter1/data/CH1data.csv", stringsAsFactors = FALSE)


###P88_CPUE~All

# 1yr.

P88Abun_lm8 <- lm(crab$P88_CPUE ~ lag(crab$P88_SublegalCPUE))
summary(P88Abun_lm8) 
P88Abun_lm9 <- lm(crab$P88_CPUE ~ lag(crab$P88_LegalCPUE))
summary(P88Abun_lm9) 
P88Abun_lm10 <- lm(crab$P88_CPUE ~ lag(crab$P88_CPUE))
summary(P88Abun_lm10) 


# 2yr.

P88AbunLead_lm8 <- lm(lead(crab$P88_CPUE) ~ lag(crab$P88_SublegalCPUE))
summary(P88AbunLead_lm8) 
P88AbunLead_lm9 <- lm(lead(crab$P88_CPUE) ~ lag(crab$P88_LegalCPUE))
summary(P88AbunLead_lm9) 
P88AbunLead_lm10 <- lm(lead(crab$P88_CPUE) ~ lag(crab$P88_CPUE))
summary(P88AbunLead_lm10) 




###Legal ~ All

# 1yr.

P88Abun_lm8 <- lm(crab$P88_LegalCPUE ~ lag(crab$P88_SublegalCPUE))
summary(P88Abun_lm8) 
P88Abun_lm9 <- lm(crab$P88_LegalCPUE ~ lag(crab$P88_LegalCPUE))
summary(P88Abun_lm9) 
P88Abun_lm10 <- lm(crab$P88_LegalCPUE ~ lag(crab$P88_CPUE))
summary(P88Abun_lm10) 


# 2yr.

P88AbunLead_lm8 <- lm(lead(crab$P88_LegalCPUE) ~ lag(crab$P88_SublegalCPUE))
summary(P88AbunLead_lm8) 
P88AbunLead_lm9 <- lm(lead(crab$P88_LegalCPUE) ~ lag(crab$P88_LegalCPUE))
summary(P88AbunLead_lm9) 
P88AbunLead_lm10 <- lm(lead(crab$P88_LegalCPUE) ~ lag(crab$P88_CPUE))
summary(P88AbunLead_lm10)




###Subdult ~ All

# 1yr.

P88Abun_lm8 <- lm(crab$P88_SublegalCPUE ~ lag(crab$P88_SublegalCPUE))
summary(P88Abun_lm8) 
P88Abun_lm9 <- lm(crab$P88_SublegalCPUE ~ lag(crab$P88_LegalCPUE))
summary(P88Abun_lm9) 
P88Abun_lm10 <- lm(crab$P88_SublegalCPUE ~ lag(crab$P88_CPUE))
summary(P88Abun_lm10) 

# 2yr.

P88AbunLead_lm8 <- lm(lead(crab$P88_SublegalCPUE) ~ lag(crab$P88_SublegalCPUE))
summary(P88AbunLead_lm8) 
P88AbunLead_lm9 <- lm(lead(crab$P88_SublegalCPUE) ~ lag(crab$P88_LegalCPUE))
summary(P88AbunLead_lm9) 
P88AbunLead_lm10 <- lm(lead(crab$P88_SublegalCPUE) ~ lag(crab$P88_CPUE))
summary(P88AbunLead_lm10) 





