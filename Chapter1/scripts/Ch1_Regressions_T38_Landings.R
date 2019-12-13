library(dplyr)
library(tidyr)
library(ggplot2)
library(PerformanceAnalytics)
library(MuMIn)
library(MASS)

# Read in data ------------------------------------------------------------

crab <- read.csv("./Chapter1/data/CH1data.csv", stringsAsFactors = FALSE)


T38crab <- filter(crab, Year > 1979) %>%
  dplyr::select(34:48, 52, 54, 55, 61)
T38crab04 <- filter(crab, Year > 2003) %>%
  dplyr::select(34:48, 52, 54, 55, 61)




# T38 Landings ------------------------------------------------------------


###1980###

#Insignificant
T38Land_lm1 <- lm(T38crab$SumLandings ~ T38crab$T38_CPUE)
summary(T38Land_lm1)
T38Land_lm2 <- lm(T38crab$SumLandings ~ T38crab$T38_JuvCPUE)
summary(T38Land_lm2)
T38Land_lm3 <- lm(T38crab$SumLandings ~ T38crab$T38_SubadultCPUE)
summary(T38Land_lm3)
T38Land_lm4 <- lm(T38crab$SumLandings ~ T38crab$T38_AdultCPUE)
summary(T38Land_lm4)
T38Land_lm5 <- lm(T38crab$SumLandings ~ T38crab$T38_ImmatureFemaleCPUE)
summary(T38Land_lm5)
T38Land_lm6 <- lm(T38crab$SumLandings ~ T38crab$T38_ImmatureMaleCPUE)
summary(T38Land_lm6)
T38Land_lm7 <- lm(T38crab$SumLandings ~ T38crab$T38_MatureFemaleCPUE)
summary(T38Land_lm7)
T38Land_lm8 <- lm(T38crab$SumLandings ~ T38crab$T38_MatureMaleCPUE)
summary(T38Land_lm8)
T38Land_lm9 <- lm(T38crab$SumLandings ~ T38crab$T38_SublegalCPUE)
summary(T38Land_lm9)
T38Land_lm10 <- lm(T38crab$SumLandings ~ T38crab$T38_LegalCPUE)
summary(T38Land_lm10)


#With Lag

#Insignificant
T38LandLAG_lm1 <- lm(T38crab$SumLandings ~ lag(T38crab$T38_CPUE))
summary(T38LandLAG_lm1)
T38LandLAG_lm2 <- lm(T38crab$SumLandings ~ lag(T38crab$T38_JuvCPUE))
summary(T38LandLAG_lm2)
T38LandLAG_lm3 <- lm(T38crab$SumLandings ~ lag(T38crab$T38_SubadultCPUE))
summary(T38LandLAG_lm3)
T38LandLAG_lm4 <- lm(T38crab$SumLandings ~ lag(T38crab$T38_AdultCPUE))
summary(T38LandLAG_lm4)
T38LandLAG_lm6 <- lm(T38crab$SumLandings ~ lag(T38crab$T38_ImmatureMaleCPUE))
summary(T38LandLAG_lm6)
T38LandLAG_lm7 <- lm(T38crab$SumLandings ~ lag(T38crab$T38_MatureFemaleCPUE))
summary(T38LandLAG_lm7)
T38LandLAG_lm8 <- lm(T38crab$SumLandings ~ lag(T38crab$T38_MatureMaleCPUE))
summary(T38LandLAG_lm8) 
T38LandLAG_lm9 <- lm(T38crab$SumLandings ~ lag(T38crab$T38_SublegalCPUE))
summary(T38LandLAG_lm9)
T38LandLAG_lm10 <- lm(T38crab$SumLandings ~ lag(T38crab$T38_LegalCPUE))
summary(T38LandLAG_lm10)

#Significant 
T38LandLAG_lm5 <- lm(T38crab$SumLandings ~ lag(T38crab$T38_ImmatureFemaleCPUE))
summary(T38LandLAG_lm5)


###2004###
#Insignificant
T38Land04_lm1 <- lm(T38crab04$SumLandings ~ T38crab04$T38_CPUE)
summary(T38Land04_lm1)
T38Land04_lm2 <- lm(T38crab04$SumLandings ~ T38crab04$T38_JuvCPUE)
summary(T38Land04_lm2)
T38Land04_lm3 <- lm(T38crab04$SumLandings ~ T38crab04$T38_SubadultCPUE)
summary(T38Land04_lm3)
T38Land04_lm4 <- lm(T38crab04$SumLandings ~ T38crab04$T38_AdultCPUE)
summary(T38Land04_lm4)
T38Land04_lm5 <- lm(T38crab04$SumLandings ~ T38crab04$T38_ImmatureFemaleCPUE)
summary(T38Land04_lm5)
T38Land04_lm6 <- lm(T38crab04$SumLandings ~ T38crab04$T38_ImmatureMaleCPUE)
summary(T38Land04_lm6)
T38Land04_lm7 <- lm(T38crab04$SumLandings ~ T38crab04$T38_MatureFemaleCPUE)
summary(T38Land04_lm7)
T38Land04_lm8 <- lm(T38crab04$SumLandings ~ T38crab04$T38_MatureMaleCPUE)
summary(T38Land04_lm8)
T38Land04_lm9 <- lm(T38crab04$SumLandings ~ T38crab04$T38_SublegalCPUE)
summary(T38Land04_lm9)
T38Land04_lm10 <- lm(T38crab04$SumLandings ~ T38crab04$T38_LegalCPUE)
summary(T38Land04_lm10)

#With Lag

#Insignificant
T38LandLAG04_lm1 <- lm(T38crab04$SumLandings ~ lag(T38crab04$T38_CPUE))
summary(T38LandLAG04_lm1)
T38LandLAG04_lm2 <- lm(T38crab04$SumLandings ~ lag(T38crab04$T38_JuvCPUE))
summary(T38LandLAG04_lm2)
T38LandLAG04_lm3 <- lm(T38crab04$SumLandings ~ lag(T38crab04$T38_SubadultCPUE))
summary(T38LandLAG04_lm3)
T38LandLAG04_lm4 <- lm(T38crab04$SumLandings ~ lag(T38crab04$T38_AdultCPUE))
summary(T38LandLAG04_lm4)
T38LandLAG04_lm5 <- lm(T38crab04$SumLandings ~ lag(T38crab04$T38_ImmatureFemaleCPUE))
summary(T38LandLAG04_lm5)
T38LandLAG04_lm6 <- lm(T38crab04$SumLandings ~ lag(T38crab04$T38_ImmatureMaleCPUE))
summary(T38LandLAG04_lm6)
T38LandLAG04_lm7 <- lm(T38crab04$SumLandings ~ lag(T38crab04$T38_MatureFemaleCPUE))
summary(T38LandLAG04_lm7)
T38LandLAG04_lm8 <- lm(T38crab04$SumLandings ~ lag(T38crab04$T38_MatureMaleCPUE))
summary(T38LandLAG04_lm8)
T38LandLAG04_lm9 <- lm(T38crab04$SumLandings ~ lag(T38crab04$T38_SublegalCPUE))
summary(T38LandLAG04_lm9)
T38LandLAG04_lm10 <- lm(T38crab04$SumLandings ~ lag(T38crab04$T38_LegalCPUE))
summary(T38LandLAG04_lm10)

#Significant - NONE






# T38 Landings CPUE -------------------------------------------------------


###1980###

#Insignificant
T38CPUE_lm1 <- lm(crab$SumLandingsCPUE ~ crab$T38_CPUE)
summary(T38CPUE_lm1) 
T38CPUE_lm2 <- lm(crab$SumLandingsCPUE ~ crab$T38_JuvCPUE)
summary(T38CPUE_lm2)
T38CPUE_lm3 <- lm(crab$SumLandingsCPUE ~ crab$T38_SubadultCPUE)
summary(T38CPUE_lm3) 
T38CPUE_lm4 <- lm(crab$SumLandingsCPUE ~ crab$T38_AdultCPUE)
summary(T38CPUE_lm4) 
T38CPUE_lm5 <- lm(crab$SumLandingsCPUE ~ crab$T38_ImmatureFemaleCPUE)
summary(T38CPUE_lm5)
T38CPUE_lm6 <- lm(crab$SumLandingsCPUE ~ crab$T38_ImmatureMaleCPUE)
summary(T38CPUE_lm6) 
T38CPUE_lm7 <- lm(crab$SumLandingsCPUE ~ crab$T38_MatureFemaleCPUE)
summary(T38CPUE_lm7) 
T38CPUE_lm8 <- lm(crab$SumLandingsCPUE ~ crab$T38_MatureMaleCPUE)
summary(T38CPUE_lm8) 
T38CPUE_lm9 <- lm(crab$SumLandingsCPUE ~ crab$T38_LegalCPUE)
summary(T38CPUE_lm9) 
T38CPUE_lm10 <- lm(crab$SumLandingsCPUE ~ crab$T38_SublegalCPUE)
summary(T38CPUE_lm10)

#Significant - NONE
 

#with LAG

#Insignificant
T38CPUELAG_lm4 <- lm(crab$SumLandingsCPUE ~ lag(crab$T38_AdultCPUE))
summary(T38CPUELAG_lm4)
T38CPUELAG_lm6 <- lm(crab$SumLandingsCPUE ~ lag(crab$T38_MatureFemaleCPUE))
summary(T38CPUELAG_lm6)
T38CPUELAG_lm9 <- lm(crab$SumLandingsCPUE ~ lag(crab$T38_LegalCPUE))
summary(T38CPUELAG_lm9)

#Significant
T38CPUELAG_lm1 <- lm(crab$SumLandingsCPUE ~ lag(crab$T38_CPUE))
summary(T38CPUELAG_lm1) 
T38CPUELAG_lm2 <- lm(crab$SumLandingsCPUE ~ lag(crab$T38_JuvCPUE))
summary(T38CPUELAG_lm2)
T38CPUELAG_lm3 <- lm(crab$SumLandingsCPUE ~ lag(crab$T38_SubadultCPUE))
summary(T38CPUELAG_lm3)
T38CPUELAG_lm5 <- lm(crab$SumLandingsCPUE ~ lag(crab$T38_ImmatureFemaleCPUE))
summary(T38CPUELAG_lm5)
T38CPUELAG_lm7 <- lm(crab$SumLandingsCPUE ~ lag(crab$T38_ImmatureMaleCPUE))
summary(T38CPUELAG_lm7)
T38CPUELAG_lm8 <- lm(crab$SumLandingsCPUE ~ lag(crab$T38_MatureMaleCPUE))
summary(T38CPUELAG_lm8)
T38CPUELAG_lm10 <- lm(crab$SumLandingsCPUE ~ lag(crab$T38_SublegalCPUE))
summary(T38CPUELAG_lm10)



###2004### - WHY DOES THIS SHOW A BIT DIFFERENT RELATIOSNSHIPS? - The dataset should auto cull to 2004, since that's when the dep var begins

#Insignificant
T38CPUE04_lm1 <- lm(T38crab04$SumLandingsCPUE ~ T38crab04$T38_CPUE)
summary(T38CPUE04_lm1) 
T38CPUE04_lm2 <- lm(T38crab04$SumLandingsCPUE ~ T38crab04$T38_JuvCPUE)
summary(T38CPUE04_lm2)
T38CPUE04_lm3 <- lm(T38crab04$SumLandingsCPUE ~ T38crab04$T38_SubadultCPUE)
summary(T38CPUE04_lm3) 
T38CPUE04_lm4 <- lm(T38crab04$SumLandingsCPUE ~ T38crab04$T38_AdultCPUE)
summary(T38CPUE04_lm4) 
T38CPUE04_lm5 <- lm(T38crab04$SumLandingsCPUE ~ T38crab04$T38_ImmatureFemaleCPUE)
summary(T38CPUE04_lm5)
T38CPUE04_lm6 <- lm(T38crab04$SumLandingsCPUE ~ T38crab04$T38_ImmatureMaleCPUE)
summary(T38CPUE04_lm6) 
T38CPUE04_lm7 <- lm(T38crab04$SumLandingsCPUE ~ T38crab04$T38_MatureFemaleCPUE)
summary(T38CPUE04_lm7) 
T38CPUE04_lm8 <- lm(T38crab04$SumLandingsCPUE ~ T38crab04$T38_MatureMaleCPUE)
summary(T38CPUE04_lm8) 
T38CPUE04_lm9 <- lm(T38crab04$SumLandingsCPUE ~ T38crab04$T38_LegalCPUE)
summary(T38CPUE04_lm9)
T38CPUE04_lm10 <- lm(T38crab04$SumLandingsCPUE ~ T38crab04$T38_SublegalCPUE)
summary(T38CPUE04_lm10)

#Significant - NONE


#With LAG

#Insignificant
T38CPUELAG04_lm4 <- lm(T38crab04$SumLandingsCPUE ~ lag(T38crab04$T38_AdultCPUE))
summary(T38CPUELAG04_lm4) 
T38CPUELAG04_lm5 <- lm(T38crab04$SumLandingsCPUE ~ lag(T38crab04$T38_ImmatureFemaleCPUE))
summary(T38CPUELAG04_lm5) #0.05186, 0.2797
T38CPUELAG04_lm9 <- lm(T38crab04$SumLandingsCPUE ~ lag(T38crab04$T38_LegalCPUE))
summary(T38CPUELAG04_lm9) 


#Significant
T38CPUELAG04_lm1 <- lm(T38crab04$SumLandingsCPUE ~ lag(T38crab04$T38_CPUE))
summary(T38CPUELAG04_lm1) 
T38CPUELAG04_lm2 <- lm(T38crab04$SumLandingsCPUE ~ lag(T38crab04$T38_JuvCPUE))
summary(T38CPUELAG04_lm2) 
T38CPUELAG04_lm3 <- lm(T38crab04$SumLandingsCPUE ~ lag(T38crab04$T38_SubadultCPUE))
summary(T38CPUELAG04_lm3) 
T38CPUELAG04_lm6 <- lm(T38crab04$SumLandingsCPUE ~ lag(T38crab04$T38_MatureFemaleCPUE))
summary(T38CPUELAG04_lm6) 
T38CPUELAG04_lm7 <- lm(T38crab04$SumLandingsCPUE ~ lag(T38crab04$T38_ImmatureMaleCPUE))
summary(T38CPUELAG04_lm7) 
T38CPUELAG04_lm8 <- lm(T38crab04$SumLandingsCPUE ~ lag(T38crab04$T38_MatureMaleCPUE))
summary(T38CPUELAG04_lm8) 
T38CPUELAG04_lm10 <- lm(T38crab04$SumLandingsCPUE ~ lag(T38crab04$T38_SublegalCPUE))
summary(T38CPUELAG04_lm10) 



