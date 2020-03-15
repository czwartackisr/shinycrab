library(dplyr)
library(tidyr)
library(ggplot2)
library(PerformanceAnalytics)
library(MuMIn)
library(MASS)

# Read in data ------------------------------------------------------------

crab <- read.csv("./Chapter1/data/CH1data.csv", stringsAsFactors = FALSE)


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
summary(E98AbunLead_lm2) # p-value: 4.207e-07 - Multiple R-squared:  0.9302
E98AbunLead_lm3 <- lm(lead(crab$E98_CPUE) ~ lag(crab$E98_AdultCPUE))
summary(E98AbunLead_lm3) # p-value: 0.006421 - Multiple R-squared:  0.5408
E98AbunLead_lm8 <- lm(lead(crab$E98_CPUE) ~ lag(crab$E98_SublegalCPUE))
summary(E98AbunLead_lm8) # p-value: 7.502e-07 - Multiple R-squared:  0.9217
E98AbunLead_lm9 <- lm(lead(crab$E98_CPUE) ~ lag(crab$E98_LegalCPUE))
summary(E98AbunLead_lm9) # p-value: 0.006421 - Multiple R-squared:  0.5408
E98AbunLead_lm10 <- lm(lead(crab$E98_CPUE) ~ lag(crab$E98_CPUE))
summary(E98AbunLead_lm10) # p-value: 6.252e-06 - Multiple R-squared:  0.8808




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
summary(E98AbunLead_lm2) # - p-value: 1.829e-06 - Multiple R-squared:  0.9065
E98AbunLead_lm3 <- lm(lead(crab$E98_AdultCPUE) ~ lag(crab$E98_AdultCPUE))
summary(E98AbunLead_lm3) # - p-value: 0.008725 - Multiple R-squared:  0.5136
E98AbunLead_lm8 <- lm(lead(crab$E98_AdultCPUE) ~ lag(crab$E98_SublegalCPUE))
summary(E98AbunLead_lm8) # p-value: 2.329e-06 - Multiple R-squared:  0.9019
E98AbunLead_lm9 <- lm(lead(crab$E98_AdultCPUE) ~ lag(crab$E98_LegalCPUE))
summary(E98AbunLead_lm9) # - p-value: 0.008725 - Multiple R-squared:  0.5136
E98AbunLead_lm10 <- lm(lead(crab$E98_AdultCPUE) ~ lag(crab$E98_CPUE))
summary(E98AbunLead_lm10) # p-value: 2.075e-05 - Multiple R-squared:  0.8489




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
