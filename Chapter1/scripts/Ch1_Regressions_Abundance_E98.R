library(dplyr)
library(tidyr)
library(ggplot2)
library(PerformanceAnalytics)
library(MuMIn)
library(MASS)

# Read in data ------------------------------------------------------------

crab <- read.csv("./Chapter1/data/CH1data.csv", stringsAsFactors = FALSE) %>%
  mutate(E98_CPUE = log(E98_CPUE)) %>%
  mutate(E98_CPUE = if_else(is.infinite(E98_CPUE), 0.00000001, E98_CPUE)) %>%
  mutate(E98_JuvCPUE = log(E98_JuvCPUE)) %>%
  mutate(E98_JuvCPUE = if_else(is.infinite(E98_JuvCPUE), 0.00000001, E98_JuvCPUE)) %>%
  mutate(E98_SubadultCPUE = log(E98_SubadultCPUE)) %>%
  mutate(E98_SubadultCPUE = if_else(is.infinite(E98_SubadultCPUE), 0.00000001, E98_SubadultCPUE)) %>%
  mutate(E98_AdultCPUE = log(E98_AdultCPUE)) %>%
  mutate(E98_AdultCPUE = if_else(is.infinite(E98_AdultCPUE), 0.00000001, E98_AdultCPUE)) %>%
  mutate(E98_SublegalCPUE = log(E98_SublegalCPUE)) %>%
  mutate(E98_SublegalCPUE = if_else(is.infinite(E98_SublegalCPUE), 0.00000001, E98_SublegalCPUE))
names(crab)


###E98_CPUE~All

# 1yr.
E98Abun_lm1 <- lm(crab$E98_CPUE ~ lag(crab$E98_JuvCPUE))
summary(E98Abun_lm1) 
E98Abun_lm2 <- lm(crab$E98_CPUE ~ lag(crab$E98_SubadultCPUE))
summary(E98Abun_lm2) 
E98Abun_lm3 <- lm(crab$E98_CPUE ~ lag(crab$E98_AdultCPUE))
summary(E98Abun_lm3) 
E98Abun_lm8 <- lm(crab$E98_CPUE ~ lag(crab$E98_SublegalCPUE))
summary(E98Abun_lm8) 
E98Abun_lm9 <- lm(crab$E98_CPUE ~ lag(crab$E98_LegalCPUE))
summary(E98Abun_lm9) 
E98Abun_lm10 <- lm(crab$E98_CPUE ~ lag(crab$E98_CPUE))
summary(E98Abun_lm10) 


# 2yr.
E98AbunLead_lm1 <- lm(lead(crab$E98_CPUE) ~ lag(crab$E98_JuvCPUE))
summary(E98AbunLead_lm1) 
E98AbunLead_lm2 <- lm(lead(crab$E98_CPUE) ~ lag(crab$E98_SubadultCPUE))
summary(E98AbunLead_lm2) # p-value: 0.040; 0.3567
E98AbunLead_lm3 <- lm(lead(crab$E98_CPUE) ~ lag(crab$E98_AdultCPUE))
summary(E98AbunLead_lm3) 
E98AbunLead_lm8 <- lm(lead(crab$E98_CPUE) ~ lag(crab$E98_SublegalCPUE))
summary(E98AbunLead_lm8) # p-value: 0.0234 - 0.4166
E98AbunLead_lm9 <- lm(lead(crab$E98_CPUE) ~ lag(crab$E98_LegalCPUE))
summary(E98AbunLead_lm9) 
E98AbunLead_lm10 <- lm(lead(crab$E98_CPUE) ~ lag(crab$E98_CPUE))
summary(E98AbunLead_lm10) 




###Adult ~ All

# 1yr.
E98Abun_lm1 <- lm(crab$E98_AdultCPUE ~ lag(crab$E98_JuvCPUE))
summary(E98Abun_lm1) 
E98Abun_lm2 <- lm(crab$E98_AdultCPUE ~ lag(crab$E98_SubadultCPUE))
summary(E98Abun_lm2) 
E98Abun_lm3 <- lm(crab$E98_AdultCPUE ~ lag(crab$E98_AdultCPUE))
summary(E98Abun_lm3) 
E98Abun_lm8 <- lm(crab$E98_AdultCPUE ~ lag(crab$E98_SublegalCPUE))
summary(E98Abun_lm8) 
E98Abun_lm9 <- lm(crab$E98_AdultCPUE ~ lag(crab$E98_LegalCPUE))
summary(E98Abun_lm9) 
E98Abun_lm10 <- lm(crab$E98_AdultCPUE ~ lag(crab$E98_CPUE))
summary(E98Abun_lm10) 


# 2yr.
E98AbunLead_lm1 <- lm(lead(crab$E98_AdultCPUE) ~ lag(crab$E98_JuvCPUE))
summary(E98AbunLead_lm1)
E98AbunLead_lm2 <- lm(lead(crab$E98_AdultCPUE) ~ lag(crab$E98_SubadultCPUE))
summary(E98AbunLead_lm2) # p-value: 0.03083 - 0.3868
E98AbunLead_lm3 <- lm(lead(crab$E98_AdultCPUE) ~ lag(crab$E98_AdultCPUE))
summary(E98AbunLead_lm3) 
E98AbunLead_lm8 <- lm(lead(crab$E98_AdultCPUE) ~ lag(crab$E98_SublegalCPUE))
summary(E98AbunLead_lm8) # p-value: 0.01602 - 0.4557
E98AbunLead_lm9 <- lm(lead(crab$E98_AdultCPUE) ~ lag(crab$E98_LegalCPUE))
summary(E98AbunLead_lm9) 
E98AbunLead_lm10 <- lm(lead(crab$E98_AdultCPUE) ~ lag(crab$E98_CPUE))
summary(E98AbunLead_lm10) 




###Subdult ~ All

# 1yr.
E98Abun_lm1 <- lm(crab$E98_SubadultCPUE ~ lag(crab$E98_JuvCPUE))
summary(E98Abun_lm1) 
E98Abun_lm2 <- lm(crab$E98_SubadultCPUE ~ lag(crab$E98_SubadultCPUE))
summary(E98Abun_lm2) 
E98Abun_lm3 <- lm(crab$E98_SubadultCPUE ~ lag(crab$E98_AdultCPUE))
summary(E98Abun_lm3) 
E98Abun_lm8 <- lm(crab$E98_SubadultCPUE ~ lag(crab$E98_SublegalCPUE))
summary(E98Abun_lm8) 
E98Abun_lm9 <- lm(crab$E98_SubadultCPUE ~ lag(crab$E98_LegalCPUE))
summary(E98Abun_lm9) 
E98Abun_lm10 <- lm(crab$E98_SubadultCPUE ~ lag(crab$E98_CPUE))
summary(E98Abun_lm10) 

# 2yr.
E98AbunLead_lm1 <- lm(lead(crab$E98_SubadultCPUE) ~ lag(crab$E98_JuvCPUE))
summary(E98AbunLead_lm1) 
E98AbunLead_lm2 <- lm(lead(crab$E98_SubadultCPUE) ~ lag(crab$E98_SubadultCPUE))
summary(E98AbunLead_lm2) # p-value: 3.932e-07 - Multiple R-squared:  0.9311
E98AbunLead_lm3 <- lm(lead(crab$E98_SubadultCPUE) ~ lag(crab$E98_AdultCPUE))
summary(E98AbunLead_lm3) # p-value: 0.006081 - Multiple R-squared:  0.5454
E98AbunLead_lm8 <- lm(lead(crab$E98_SubadultCPUE) ~ lag(crab$E98_SublegalCPUE))
summary(E98AbunLead_lm8) # p-value: 7.322e-07 - Multiple R-squared:  0.9221
E98AbunLead_lm9 <- lm(lead(crab$E98_SubadultCPUE) ~ lag(crab$E98_LegalCPUE))
summary(E98AbunLead_lm9) # p-value: 0.006081 - Multiple R-squared:  0.5454
E98AbunLead_lm10 <- lm(lead(crab$E98_SubadultCPUE) ~ lag(crab$E98_CPUE))
summary(E98AbunLead_lm10) # p-value: 5.278e-06 - Multiple R-squared:  0.8847




###Juv ~ All

# 1yr.
E98Abun_lm1 <- lm(crab$E98_JuvCPUE ~ lag(crab$E98_JuvCPUE))
summary(E98Abun_lm1) 
E98Abun_lm2 <- lm(crab$E98_JuvCPUE ~ lag(crab$E98_SubadultCPUE))
summary(E98Abun_lm2) 
E98Abun_lm3 <- lm(crab$E98_JuvCPUE ~ lag(crab$E98_AdultCPUE))
summary(E98Abun_lm3) 
E98Abun_lm8 <- lm(crab$E98_JuvCPUE ~ lag(crab$E98_SublegalCPUE))
summary(E98Abun_lm8) 
E98Abun_lm9 <- lm(crab$E98_JuvCPUE ~ lag(crab$E98_LegalCPUE))
summary(E98Abun_lm9) 
E98Abun_lm10 <- lm(crab$E98_JuvCPUE ~ lag(crab$E98_CPUE))
summary(E98Abun_lm10) 

# 2yr.
E98AbunLead_lm1 <- lm(lead(crab$E98_JuvCPUE) ~ lag(crab$E98_JuvCPUE))
summary(E98AbunLead_lm1) 
E98AbunLead_lm2 <- lm(lead(crab$E98_JuvCPUE) ~ lag(crab$E98_SubadultCPUE))
summary(E98AbunLead_lm2) 
E98AbunLead_lm3 <- lm(lead(crab$E98_JuvCPUE) ~ lag(crab$E98_AdultCPUE))
summary(E98AbunLead_lm3) 
E98AbunLead_lm8 <- lm(lead(crab$E98_JuvCPUE) ~ lag(crab$E98_SublegalCPUE))
summary(E98AbunLead_lm8) 
E98AbunLead_lm9 <- lm(lead(crab$E98_JuvCPUE) ~ lag(crab$E98_LegalCPUE))
summary(E98AbunLead_lm9) 
E98AbunLead_lm10 <- lm(lead(crab$E98_JuvCPUE) ~ lag(crab$E98_CPUE))
summary(E98AbunLead_lm10)
