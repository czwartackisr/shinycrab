library(dplyr)
library(tidyr)
library(ggplot2)
library(PerformanceAnalytics)
library(MuMIn)
library(MASS)

# Read in data ------------------------------------------------------------

crab <- read.csv("./Chapter1/data/CH1data.csv", stringsAsFactors = FALSE) %>%
  filter(Year > 1979) 

crab03 <- filter(crab, Year > 2002) 




# T38 Landings ------------------------------------------------------------


###1980###

#Insignificant
T38Land_lm1 <- lm(crab$LandingsSum ~ crab$T38_CPUE)
summary(T38Land_lm1)
T38Land_lm2 <- lm(crab$LandingsSum ~ crab$T38_JuvCPUE)
summary(T38Land_lm2)
T38Land_lm3 <- lm(crab$LandingsSum ~ crab$T38_SubadultCPUE)
summary(T38Land_lm3)
T38Land_lm4 <- lm(crab$LandingsSum ~ crab$T38_AdultCPUE)
summary(T38Land_lm4)
T38Land_lm5 <- lm(crab$LandingsSum ~ crab$T38_ImmatureFemaleCPUE)
summary(T38Land_lm5)
T38Land_lm6 <- lm(crab$LandingsSum ~ crab$T38_ImmatureMaleCPUE)
summary(T38Land_lm6)
T38Land_lm7 <- lm(crab$LandingsSum ~ crab$T38_MatureFemaleCPUE)
summary(T38Land_lm7)
T38Land_lm8 <- lm(crab$LandingsSum ~ crab$T38_MatureMaleCPUE)
summary(T38Land_lm8)
T38Land_lm9 <- lm(crab$LandingsSum ~ crab$T38_SublegalCPUE)
summary(T38Land_lm9)
T38Land_lm10 <- lm(crab$LandingsSum ~ crab$T38_LegalCPUE)
summary(T38Land_lm10)


#With Lag

#Insignificant
T38LandLAG_lm1 <- lm(crab$LandingsSum ~ lag(crab$T38_CPUE))
summary(T38LandLAG_lm1)
T38LandLAG_lm2 <- lm(crab$LandingsSum ~ lag(crab$T38_JuvCPUE))
summary(T38LandLAG_lm2)
T38LandLAG_lm3 <- lm(crab$LandingsSum ~ lag(crab$T38_SubadultCPUE))
summary(T38LandLAG_lm3)
T38LandLAG_lm4 <- lm(crab$LandingsSum ~ lag(crab$T38_AdultCPUE))
summary(T38LandLAG_lm4)
T38LandLAG_lm5 <- lm(crab$LandingsSum ~ lag(crab$T38_ImmatureFemaleCPUE))
summary(T38LandLAG_lm5)
T38LandLAG_lm6 <- lm(crab$LandingsSum ~ lag(crab$T38_ImmatureMaleCPUE))
summary(T38LandLAG_lm6)
T38LandLAG_lm7 <- lm(crab$LandingsSum ~ lag(crab$T38_MatureFemaleCPUE))
summary(T38LandLAG_lm7)
T38LandLAG_lm8 <- lm(crab$LandingsSum ~ lag(crab$T38_MatureMaleCPUE))
summary(T38LandLAG_lm8) 
T38LandLAG_lm9 <- lm(crab$LandingsSum ~ lag(crab$T38_SublegalCPUE))
summary(T38LandLAG_lm9)
T38LandLAG_lm10 <- lm(crab$LandingsSum ~ lag(crab$T38_LegalCPUE))
summary(T38LandLAG_lm10)

#Significant - NONE


#With LEad Lag

#Insignificant
T38LandLAGLead_lm1 <- lm(lead(crab$LandingsSum) ~ lag(crab$T38_CPUE))
summary(T38LandLAGLead_lm1)
T38LandLAGLead_lm2 <- lm(lead(crab$LandingsSum) ~ lag(crab$T38_JuvCPUE))
summary(T38LandLAGLead_lm2)
T38LandLAGLead_lm3 <- lm(lead(crab$LandingsSum) ~ lag(crab$T38_SubadultCPUE))
summary(T38LandLAGLead_lm3)
T38LandLAGLead_lm4 <- lm(lead(crab$LandingsSum) ~ lag(crab$T38_AdultCPUE))
summary(T38LandLAGLead_lm4)
T38LandLAGLead_lm5 <- lm(lead(crab$LandingsSum) ~ lag(crab$T38_ImmatureFemaleCPUE))
summary(T38LandLAGLead_lm5)
T38LandLAGLead_lm6 <- lm(lead(crab$LandingsSum) ~ lag(crab$T38_ImmatureMaleCPUE))
summary(T38LandLAGLead_lm6)
T38LandLAGLead_lm7 <- lm(lead(crab$LandingsSum) ~ lag(crab$T38_MatureFemaleCPUE))
summary(T38LandLAGLead_lm7)
T38LandLAGLead_lm8 <- lm(lead(crab$LandingsSum) ~ lag(crab$T38_MatureMaleCPUE))
summary(T38LandLAGLead_lm8) 
T38LandLAGLead_lm9 <- lm(lead(crab$LandingsSum) ~ lag(crab$T38_SublegalCPUE))
summary(T38LandLAGLead_lm9)
T38LandLAGLead_lm10 <- lm(lead(crab$LandingsSum) ~ lag(crab$T38_LegalCPUE))
summary(T38LandLAGLead_lm10)

#Significant 





###2003###

#Insignificant
T38Land_lm1 <- lm(crab03$LandingsSum ~ crab03$T38_CPUE)
summary(T38Land_lm1)
T38Land_lm2 <- lm(crab03$LandingsSum ~ crab03$T38_JuvCPUE)
summary(T38Land_lm2)
T38Land_lm3 <- lm(crab03$LandingsSum ~ crab03$T38_SubadultCPUE)
summary(T38Land_lm3)
T38Land_lm4 <- lm(crab03$LandingsSum ~ crab03$T38_AdultCPUE)
summary(T38Land_lm4)
T38Land_lm5 <- lm(crab03$LandingsSum ~ crab03$T38_ImmatureFemaleCPUE)
summary(T38Land_lm5)
T38Land_lm6 <- lm(crab03$LandingsSum ~ crab03$T38_ImmatureMaleCPUE)
summary(T38Land_lm6)
T38Land_lm7 <- lm(crab03$LandingsSum ~ crab03$T38_MatureFemaleCPUE)
summary(T38Land_lm7)
T38Land_lm8 <- lm(crab03$LandingsSum ~ crab03$T38_MatureMaleCPUE)
summary(T38Land_lm8)
T38Land_lm9 <- lm(crab03$LandingsSum ~ crab03$T38_SublegalCPUE)
summary(T38Land_lm9)
T38Land_lm10 <- lm(crab03$LandingsSum ~ crab03$T38_LegalCPUE)
summary(T38Land_lm10)


#With Lag

#Insignificant
T38LandLAG_lm1 <- lm(crab03$LandingsSum ~ lag(crab03$T38_CPUE))
summary(T38LandLAG_lm1)
T38LandLAG_lm2 <- lm(crab03$LandingsSum ~ lag(crab03$T38_JuvCPUE))
summary(T38LandLAG_lm2)
T38LandLAG_lm3 <- lm(crab03$LandingsSum ~ lag(crab03$T38_SubadultCPUE))
summary(T38LandLAG_lm3)
T38LandLAG_lm4 <- lm(crab03$LandingsSum ~ lag(crab03$T38_AdultCPUE))
summary(T38LandLAG_lm4)
T38LandLAG_lm5 <- lm(crab03$LandingsSum ~ lag(crab03$T38_ImmatureFemaleCPUE))
summary(T38LandLAG_lm5)
T38LandLAG_lm6 <- lm(crab03$LandingsSum ~ lag(crab03$T38_ImmatureMaleCPUE))
summary(T38LandLAG_lm6)
T38LandLAG_lm7 <- lm(crab03$LandingsSum ~ lag(crab03$T38_MatureFemaleCPUE))
summary(T38LandLAG_lm7)
T38LandLAG_lm8 <- lm(crab03$LandingsSum ~ lag(crab03$T38_MatureMaleCPUE))
summary(T38LandLAG_lm8) 
T38LandLAG_lm9 <- lm(crab03$LandingsSum ~ lag(crab03$T38_SublegalCPUE))
summary(T38LandLAG_lm9)
T38LandLAG_lm10 <- lm(crab03$LandingsSum ~ lag(crab03$T38_LegalCPUE))
summary(T38LandLAG_lm10)

#Significant 


#With LEad Lag

#Insignificant
T38LandLAGLead_lm1 <- lm(lead(crab03$LandingsSum) ~ lag(crab03$T38_CPUE))
summary(T38LandLAGLead_lm1)
T38LandLAGLead_lm2 <- lm(lead(crab03$LandingsSum) ~ lag(crab03$T38_JuvCPUE))
summary(T38LandLAGLead_lm2)
T38LandLAGLead_lm3 <- lm(lead(crab03$LandingsSum) ~ lag(crab03$T38_SubadultCPUE))
summary(T38LandLAGLead_lm3)
T38LandLAGLead_lm4 <- lm(lead(crab03$LandingsSum) ~ lag(crab03$T38_AdultCPUE))
summary(T38LandLAGLead_lm4)
T38LandLAGLead_lm5 <- lm(lead(crab03$LandingsSum) ~ lag(crab03$T38_ImmatureFemaleCPUE))
summary(T38LandLAGLead_lm5)
T38LandLAGLead_lm6 <- lm(lead(crab03$LandingsSum) ~ lag(crab03$T38_ImmatureMaleCPUE))
summary(T38LandLAGLead_lm6)
T38LandLAGLead_lm7 <- lm(lead(crab03$LandingsSum) ~ lag(crab03$T38_MatureFemaleCPUE))
summary(T38LandLAGLead_lm7)
T38LandLAGLead_lm8 <- lm(lead(crab03$LandingsSum) ~ lag(crab03$T38_MatureMaleCPUE))
summary(T38LandLAGLead_lm8) 
T38LandLAGLead_lm9 <- lm(lead(crab03$LandingsSum) ~ lag(crab03$T38_SublegalCPUE))
summary(T38LandLAGLead_lm9)
T38LandLAGLead_lm10 <- lm(lead(crab03$LandingsSum) ~ lag(crab03$T38_LegalCPUE))
summary(T38LandLAGLead_lm10)

#Significant - NONE




# T38 Landings CPUE -------------------------------------------------------


###1980###

#Insignificant
T38CPUE_lm1 <- lm(crab$LandingsCPUEMean ~ crab$T38_CPUE)
summary(T38CPUE_lm1) 
T38CPUE_lm2 <- lm(crab$LandingsCPUEMean ~ crab$T38_JuvCPUE)
summary(T38CPUE_lm2)
T38CPUE_lm3 <- lm(crab$LandingsCPUEMean ~ crab$T38_SubadultCPUE)
summary(T38CPUE_lm3) 
T38CPUE_lm4 <- lm(crab$LandingsCPUEMean ~ crab$T38_AdultCPUE)
summary(T38CPUE_lm4) 
T38CPUE_lm5 <- lm(crab$LandingsCPUEMean ~ crab$T38_ImmatureFemaleCPUE)
summary(T38CPUE_lm5)
T38CPUE_lm6 <- lm(crab$LandingsCPUEMean ~ crab$T38_ImmatureMaleCPUE)
summary(T38CPUE_lm6) 
T38CPUE_lm7 <- lm(crab$LandingsCPUEMean ~ crab$T38_MatureFemaleCPUE)
summary(T38CPUE_lm7) 
T38CPUE_lm8 <- lm(crab$LandingsCPUEMean ~ crab$T38_MatureMaleCPUE)
summary(T38CPUE_lm8) 
T38CPUE_lm9 <- lm(crab$LandingsCPUEMean ~ crab$T38_LegalCPUE)
summary(T38CPUE_lm9) 
T38CPUE_lm10 <- lm(crab$LandingsCPUEMean ~ crab$T38_SublegalCPUE)
summary(T38CPUE_lm10)

#Significant - NONE
 


##With LAG

#Insignificant
T38CPUELAG_lm4 <- lm(crab$LandingsCPUEMean ~ lag(crab$T38_AdultCPUE))
summary(T38CPUELAG_lm4)
T38CPUELAG_lm6 <- lm(crab$LandingsCPUEMean ~ lag(crab$T38_MatureFemaleCPUE))
summary(T38CPUELAG_lm6)
T38CPUELAG_lm8 <- lm(crab$LandingsCPUEMean ~ lag(crab$T38_MatureMaleCPUE))
summary(T38CPUELAG_lm8)
T38CPUELAG_lm9 <- lm(crab$LandingsCPUEMean ~ lag(crab$T38_LegalCPUE))
summary(T38CPUELAG_lm9)

#Significant
T38CPUELAG_lm1 <- lm(crab$LandingsCPUEMean ~ lag(crab$T38_CPUE))
summary(T38CPUELAG_lm1) # 0.02388 - 0.3346
T38CPUELAG_lm2 <- lm(crab$LandingsCPUEMean ~ lag(crab$T38_JuvCPUE))
summary(T38CPUELAG_lm2) # 0.03014 - 0.3129
T38CPUELAG_lm3 <- lm(crab$LandingsCPUEMean ~ lag(crab$T38_SubadultCPUE))
summary(T38CPUELAG_lm3) # 0.01947 - 0.3532
T38CPUELAG_lm5 <- lm(crab$LandingsCPUEMean ~ lag(crab$T38_ImmatureFemaleCPUE))
summary(T38CPUELAG_lm5) # 0.03321 - 0.3038
T38CPUELAG_lm7 <- lm(crab$LandingsCPUEMean ~ lag(crab$T38_ImmatureMaleCPUE))
summary(T38CPUELAG_lm7) # 0.01042 - 0.4076
T38CPUELAG_lm10 <- lm(crab$LandingsCPUEMean ~ lag(crab$T38_SublegalCPUE))
summary(T38CPUELAG_lm10) # 0.01485 - 0.3772


#with Lead LAG

#Insignificant
T38CPUELAGLead_lm1 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$T38_CPUE))
summary(T38CPUELAGLead_lm1) 
T38CPUELAGLead_lm2 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$T38_JuvCPUE))
summary(T38CPUELAGLead_lm2)
T38CPUELAGLead_lm3 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$T38_SubadultCPUE))
summary(T38CPUELAGLead_lm3)
T38CPUELAGLead_lm4 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$T38_AdultCPUE))
summary(T38CPUELAGLead_lm4)
T38CPUELAGLead_lm5 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$T38_ImmatureFemaleCPUE))
summary(T38CPUELAGLead_lm5)
T38CPUELAGLead_lm6 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$T38_MatureFemaleCPUE))
summary(T38CPUELAGLead_lm6)
T38CPUELAGLead_lm7 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$T38_ImmatureMaleCPUE))
summary(T38CPUELAGLead_lm7)
T38CPUELAGLead_lm8 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$T38_MatureMaleCPUE))
summary(T38CPUELAGLead_lm8)
T38CPUELAGLead_lm9 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$T38_LegalCPUE))
summary(T38CPUELAGLead_lm9)
T38CPUELAGLead_lm10 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$T38_SublegalCPUE))
summary(T38CPUELAGLead_lm10)















# 2004 Exploration --------------------------------------------------------

###2004###
#Insignificant
T38Land04_lm1 <- lm(crab04$LandingsSum ~ crab04$T38_CPUE)
summary(T38Land04_lm1)
T38Land04_lm2 <- lm(crab04$LandingsSum ~ crab04$T38_JuvCPUE)
summary(T38Land04_lm2)
T38Land04_lm3 <- lm(crab04$LandingsSum ~ crab04$T38_SubadultCPUE)
summary(T38Land04_lm3)
T38Land04_lm4 <- lm(crab04$LandingsSum ~ crab04$T38_AdultCPUE)
summary(T38Land04_lm4)
T38Land04_lm5 <- lm(crab04$LandingsSum ~ crab04$T38_ImmatureFemaleCPUE)
summary(T38Land04_lm5)
T38Land04_lm6 <- lm(crab04$LandingsSum ~ crab04$T38_ImmatureMaleCPUE)
summary(T38Land04_lm6)
T38Land04_lm7 <- lm(crab04$LandingsSum ~ crab04$T38_MatureFemaleCPUE)
summary(T38Land04_lm7)
T38Land04_lm8 <- lm(crab04$LandingsSum ~ crab04$T38_MatureMaleCPUE)
summary(T38Land04_lm8)
T38Land04_lm9 <- lm(crab04$LandingsSum ~ crab04$T38_SublegalCPUE)
summary(T38Land04_lm9)
T38Land04_lm10 <- lm(crab04$LandingsSum ~ crab04$T38_LegalCPUE)
summary(T38Land04_lm10)

#With Lag

#Insignificant
T38LandLAG04_lm1 <- lm(crab04$LandingsSum ~ lag(crab04$T38_CPUE))
summary(T38LandLAG04_lm1)
T38LandLAG04_lm2 <- lm(crab04$LandingsSum ~ lag(crab04$T38_JuvCPUE))
summary(T38LandLAG04_lm2)
T38LandLAG04_lm3 <- lm(crab04$LandingsSum ~ lag(crab04$T38_SubadultCPUE))
summary(T38LandLAG04_lm3)
T38LandLAG04_lm4 <- lm(crab04$LandingsSum ~ lag(crab04$T38_AdultCPUE))
summary(T38LandLAG04_lm4)
T38LandLAG04_lm5 <- lm(crab04$LandingsSum ~ lag(crab04$T38_ImmatureFemaleCPUE))
summary(T38LandLAG04_lm5)
T38LandLAG04_lm6 <- lm(crab04$LandingsSum ~ lag(crab04$T38_ImmatureMaleCPUE))
summary(T38LandLAG04_lm6)
T38LandLAG04_lm7 <- lm(crab04$LandingsSum ~ lag(crab04$T38_MatureFemaleCPUE))
summary(T38LandLAG04_lm7)
T38LandLAG04_lm8 <- lm(crab04$LandingsSum ~ lag(crab04$T38_MatureMaleCPUE))
summary(T38LandLAG04_lm8)
T38LandLAG04_lm9 <- lm(crab04$LandingsSum ~ lag(crab04$T38_SublegalCPUE))
summary(T38LandLAG04_lm9)
T38LandLAG04_lm10 <- lm(crab04$LandingsSum ~ lag(crab04$T38_LegalCPUE))
summary(T38LandLAG04_lm10)

#Significant - NONE




###2004### - WHY DOES THIS SHOW A BIT DIFFERENT RELATIOSNSHIPS? - The dataset should auto cull to 2004, since that's when the dep var begins

#Insignificant
T38CPUE04_lm1 <- lm(crab04$LandingsCPUEMean ~ crab04$T38_CPUE)
summary(T38CPUE04_lm1) 
T38CPUE04_lm2 <- lm(crab04$LandingsCPUEMean ~ crab04$T38_JuvCPUE)
summary(T38CPUE04_lm2)
T38CPUE04_lm3 <- lm(crab04$LandingsCPUEMean ~ crab04$T38_SubadultCPUE)
summary(T38CPUE04_lm3) 
T38CPUE04_lm4 <- lm(crab04$LandingsCPUEMean ~ crab04$T38_AdultCPUE)
summary(T38CPUE04_lm4) 
T38CPUE04_lm5 <- lm(crab04$LandingsCPUEMean ~ crab04$T38_ImmatureFemaleCPUE)
summary(T38CPUE04_lm5)
T38CPUE04_lm6 <- lm(crab04$LandingsCPUEMean ~ crab04$T38_ImmatureMaleCPUE)
summary(T38CPUE04_lm6) 
T38CPUE04_lm7 <- lm(crab04$LandingsCPUEMean ~ crab04$T38_MatureFemaleCPUE)
summary(T38CPUE04_lm7) 
T38CPUE04_lm8 <- lm(crab04$LandingsCPUEMean ~ crab04$T38_MatureMaleCPUE)
summary(T38CPUE04_lm8) 
T38CPUE04_lm9 <- lm(crab04$LandingsCPUEMean ~ crab04$T38_LegalCPUE)
summary(T38CPUE04_lm9)
T38CPUE04_lm10 <- lm(crab04$LandingsCPUEMean ~ crab04$T38_SublegalCPUE)
summary(T38CPUE04_lm10)

#Significant - NONE


#With LAG

#Insignificant
T38CPUELAG04_lm4 <- lm(crab04$LandingsCPUEMean ~ lag(crab04$T38_AdultCPUE))
summary(T38CPUELAG04_lm4) 
T38CPUELAG04_lm5 <- lm(crab04$LandingsCPUEMean ~ lag(crab04$T38_ImmatureFemaleCPUE))
summary(T38CPUELAG04_lm5) #0.05186, 0.2797
T38CPUELAG04_lm9 <- lm(crab04$LandingsCPUEMean ~ lag(crab04$T38_LegalCPUE))
summary(T38CPUELAG04_lm9) 


#Significant
T38CPUELAG04_lm1 <- lm(crab04$LandingsCPUEMean ~ lag(crab04$T38_CPUE))
summary(T38CPUELAG04_lm1) 
T38CPUELAG04_lm2 <- lm(crab04$LandingsCPUEMean ~ lag(crab04$T38_JuvCPUE))
summary(T38CPUELAG04_lm2) 
T38CPUELAG04_lm3 <- lm(crab04$LandingsCPUEMean ~ lag(crab04$T38_SubadultCPUE))
summary(T38CPUELAG04_lm3) 
T38CPUELAG04_lm6 <- lm(crab04$LandingsCPUEMean ~ lag(crab04$T38_MatureFemaleCPUE))
summary(T38CPUELAG04_lm6) 
T38CPUELAG04_lm7 <- lm(crab04$LandingsCPUEMean ~ lag(crab04$T38_ImmatureMaleCPUE))
summary(T38CPUELAG04_lm7) 
T38CPUELAG04_lm8 <- lm(crab04$LandingsCPUEMean ~ lag(crab04$T38_MatureMaleCPUE))
summary(T38CPUELAG04_lm8) 
T38CPUELAG04_lm10 <- lm(crab04$LandingsCPUEMean ~ lag(crab04$T38_SublegalCPUE))
summary(T38CPUELAG04_lm10) 
