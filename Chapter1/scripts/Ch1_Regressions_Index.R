library(dplyr)
library(tidyr)
library(ggplot2)
library(PerformanceAnalytics)
library(MuMIn)
library(MASS)

# Read in data ------------------------------------------------------------

crab <- read.csv("./Chapter1/data/CH1_T38Dredge.csv", stringsAsFactors = FALSE)



# Multiple Regression -----------------------------------------------------

# Interactive
T38Index_lm1 <- lm(crab$CPUE ~ crab$IM_2 * crab$MM_1)
T38Index_lm2 <- lm(crab$CPUE ~ crab$MM_1 * crab$Subadult_2)
T38Index_lm3 <- lm(crab$CPUE ~ crab$MM_1 * crab$MM_2 * crab$Subadult_2)
T38Index_lm4 <- lm(crab$CPUE ~ crab$MM_1 * crab$Sublegal_2)
T38Index_lm5 <- lm(crab$CPUE ~ crab$IF_1 * crab$IM_2 * crab$Subadult_1)
T38Index_lm6 <- lm(crab$CPUE ~ crab$IF_1 * crab$IM_2 * crab$MM_1)
T38Index_lm7 <- lm(crab$CPUE ~ crab$IF_1 * crab$Subadult_2 * crab$Subadult_1)
T38Index_lm8 <- lm(crab$CPUE ~ crab$CPUE_2 * crab$MM_1)
T38Index_lm9 <- lm(crab$CPUE ~ crab$CPUE_1 * crab$IF_1 + crab$IM_2)
T38Index_lm10 <- lm(crab$CPUE ~ crab$CPUE_1 * crab$IF_1 + crab$Subadult_2)
T38Index_lm11 <- lm(crab$CPUE ~ crab$IM_2 * crab$Legal_1 * crab$MM_1)
T38Index_lm12 <- lm(crab$CPUE ~ crab$IM_2 * crab$Adult_1 * crab$MM_1)
T38Index_lm13 <- lm(crab$CPUE ~ crab$IM_2 * crab$Adult_1 * crab$MM_1)
T38Index_lm14 <- lm(crab$CPUE ~ crab$IF_1 * crab$IM_2 * crab$MM_1 * crab$Subadult_1)
T38Index_lm15 <- lm(crab$CPUE ~ crab$IF_1 * crab$MM_1 * crab$Subadult_2)
T38Index_lm16 <- lm(crab$CPUE ~ crab$CPUE_2 * crab$MM_1 * crab$MM_2)
T38Index_lm17 <- lm(crab$CPUE ~ crab$IF_2 * crab$MM_1)
T38Index_lm18 <- lm(crab$CPUE ~ crab$IM_2 * crab$Subadult_1 * crab$Sublegal_1)
T38Index_lm19 <- lm(crab$CPUE ~ crab$IF_1 * crab$Subadult_1)
T38Index_lm20 <- lm(crab$CPUE ~ crab$MF_2 * crab$MM_1 * crab$MM_2 * crab$Subadult_2)
T38Index_lm21 <- lm(crab$CPUE ~ crab$IM_2 * crab$MM_1 * crab$MM_2)


summary(T38Index_lm1)
summary(T38Index_lm2)
summary(T38Index_lm3)
summary(T38Index_lm4) 
summary(T38Index_lm5)
summary(T38Index_lm6)
summary(T38Index_lm7)
summary(T38Index_lm8)
summary(T38Index_lm9)# first model with main effects significant
summary(T38Index_lm10)# sig main effects
summary(T38Index_lm11)
summary(T38Index_lm12)
summary(T38Index_lm13)
summary(T38Index_lm14)
summary(T38Index_lm15)
summary(T38Index_lm16)
summary(T38Index_lm17)
summary(T38Index_lm18)
summary(T38Index_lm19)# sig intercept only
summary(T38Index_lm20)
summary(T38Index_lm21)


#Additive

T38Ind_lm1 <- lm(crab$CPUE ~ crab$IM_2 + crab$MM_1)
T38Ind_lm2 <- lm(crab$CPUE ~ crab$MM_1 + crab$Subadult_2)
T38Ind_lm3 <- lm(crab$CPUE ~ crab$MM_1 + crab$MM_2 + crab$Subadult_2)
T38Ind_lm4 <- lm(crab$CPUE ~ crab$MM_1 + crab$Sublegal_2)
T38Ind_lm5 <- lm(crab$CPUE ~ crab$IF_1 + crab$IM_2 + crab$Subadult_1)
T38Ind_lm6 <- lm(crab$CPUE ~ crab$IF_1 + crab$IM_2 + crab$MM_1)
T38Ind_lm7 <- lm(crab$CPUE ~ crab$IF_1 + crab$Subadult_2 + crab$Subadult_1)
T38Ind_lm8 <- lm(crab$CPUE ~ crab$CPUE_2 + crab$MM_1)
T38Ind_lm9 <- lm(crab$CPUE ~ crab$CPUE_1 + crab$IF_1 + crab$IM_2)
T38Ind_lm10 <- lm(crab$CPUE ~ crab$CPUE_1 + crab$IF_1 + crab$Subadult_2)
T38Ind_lm11 <- lm(crab$CPUE ~ crab$IM_2 + crab$Legal_1 + crab$MM_1)
T38Ind_lm12 <- lm(crab$CPUE ~ crab$IM_2 + crab$Adult_1 + crab$MM_1)
T38Ind_lm13 <- lm(crab$CPUE ~ crab$IM_2 + crab$Adult_1 + crab$MM_1)
T38Ind_lm14 <- lm(crab$CPUE ~ crab$IF_1 + crab$IM_2 + crab$MM_1 + crab$Subadult_1)
T38Ind_lm15 <- lm(crab$CPUE ~ crab$IF_1 + crab$MM_1 + crab$Subadult_2)
T38Ind_lm16 <- lm(crab$CPUE ~ crab$CPUE_2 + crab$MM_1 + crab$MM_2)
T38Ind_lm17 <- lm(crab$CPUE ~ crab$IF_2 + crab$MM_1)
T38Ind_lm18 <- lm(crab$CPUE ~ crab$IM_2 + crab$Subadult_1 + crab$Sublegal_1)
T38Ind_lm19 <- lm(crab$CPUE ~ crab$IF_1 + crab$Subadult_1)
T38Ind_lm20 <- lm(crab$CPUE ~ crab$MF_2 + crab$MM_1 + crab$MM_2 + crab$Subadult_2)
T38Ind_lm21 <- lm(crab$CPUE ~ crab$IM_2 + crab$MM_1 + crab$MM_2)

summary(T38Ind_lm1)
summary(T38Ind_lm2)
summary(T38Ind_lm3)# One term not sig
summary(T38Ind_lm4)
summary(T38Ind_lm5)# One term not sig
summary(T38Ind_lm6)# One term not sig
summary(T38Ind_lm7)# One term not sig
summary(T38Ind_lm8)
summary(T38Ind_lm9)
summary(T38Ind_lm10)# One term barely not dig 
summary(T38Ind_lm11)# One term not sig
summary(T38Ind_lm12)# One term not sig
summary(T38Ind_lm13)# One term not sig
summary(T38Ind_lm14)# One term not sig
summary(T38Ind_lm15)# One term not sig
summary(T38Ind_lm16)# One term not sig
summary(T38Ind_lm17)# One term not sig
summary(T38Ind_lm18)# One term not sig
summary(T38Ind_lm19)# One term not sig
summary(T38Ind_lm20)# One term not sig
summary(T38Ind_lm21)# One term not sig
