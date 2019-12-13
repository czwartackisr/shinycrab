library(dplyr)
library(tidyr)
library(ggplot2)
library(PerformanceAnalytics)
library(MuMIn)
library(MASS)

# Read in data ------------------------------------------------------------

crab <- read.csv("./Chapter1/data/CH1data.csv", stringsAsFactors = FALSE)

crab04 = crab %>%
  filter(Year >2003)
crab06 = crab %>%
  filter(Year >2005)


B90crab <- filter(crab, Year > 1979) %>%
  dplyr::select(2:16, 52, 54, 55, 61)
B90crab04 <- filter(crab, Year > 2003) %>%
  dplyr::select(2:16, 52, 54, 55, 61)
B90crab06 <- filter(crab, Year > 2005) %>%
  dplyr::select(2:16, 52, 54, 55, 61)



# B90 Landings ------------------------------------------------------------


###1980###

#Insignificant
B90Land_lm1 <- lm(B90crab$SumLandings ~ B90crab$B90_CPUE)
summary(B90Land_lm1)
B90Land_lm2 <- lm(B90crab$SumLandings ~ B90crab$B90_JuvCPUE)
summary(B90Land_lm2)
B90Land_lm3 <- lm(B90crab$SumLandings ~ B90crab$B90_SubadultCPUE)
summary(B90Land_lm3)
B90Land_lm5 <- lm(B90crab$SumLandings ~ B90crab$B90_ImmatureFemaleCPUE)
summary(B90Land_lm5)
B90Land_lm6 <- lm(B90crab$SumLandings ~ B90crab$B90_ImmatureMaleCPUE)
summary(B90Land_lm6)
B90Land_lm9 <- lm(B90crab$SumLandings ~ B90crab$B90_SublegalCPUE)
summary(B90Land_lm9)

#Significant
B90Land_lm4 <- lm(B90crab$SumLandings ~ B90crab$B90_AdultCPUE)
summary(B90Land_lm4)
B90Land_lm7 <- lm(B90crab$SumLandings ~ B90crab$B90_MatureFemaleCPUE)
summary(B90Land_lm7)
B90Land_lm8 <- lm(B90crab$SumLandings ~ B90crab$B90_MatureMaleCPUE)
summary(B90Land_lm8)
B90Land_lm10 <- lm(B90crab$SumLandings ~ B90crab$B90_LegalCPUE)
summary(B90Land_lm10)


#With Lag

#Insignificant
B90LandLAG_lm1 <- lm(B90crab$SumLandings ~ lag(B90crab$B90_CPUE))
summary(B90LandLAG_lm1)
B90LandLAG_lm2 <- lm(B90crab$SumLandings ~ lag(B90crab$B90_JuvCPUE))
summary(B90LandLAG_lm2)
B90LandLAG_lm3 <- lm(B90crab$SumLandings ~ lag(B90crab$B90_SubadultCPUE))
summary(B90LandLAG_lm3)
B90LandLAG_lm4 <- lm(B90crab$SumLandings ~ lag(B90crab$B90_AdultCPUE))
summary(B90LandLAG_lm4)
B90LandLAG_lm5 <- lm(B90crab$SumLandings ~ lag(B90crab$B90_ImmatureFemaleCPUE))
summary(B90LandLAG_lm5)
B90LandLAG_lm6 <- lm(B90crab$SumLandings ~ lag(B90crab$B90_ImmatureMaleCPUE))
summary(B90LandLAG_lm6)
B90LandLAG_lm7 <- lm(B90crab$SumLandings ~ lag(B90crab$B90_MatureFemaleCPUE))
summary(B90LandLAG_lm7)
B90LandLAG_lm8 <- lm(B90crab$SumLandings ~ lag(B90crab$B90_MatureMaleCPUE))
summary(B90LandLAG_lm8) #0.05772
B90LandLAG_lm9 <- lm(B90crab$SumLandings ~ lag(B90crab$B90_SublegalCPUE))
summary(B90LandLAG_lm9)
B90LandLAG_lm10 <- lm(B90crab$SumLandings ~ lag(B90crab$B90_LegalCPUE))
summary(B90LandLAG_lm10)

#Significant - NONE
#


###2004###
#Insignificant
B90Land04_lm1 <- lm(B90crab04$SumLandings ~ B90crab04$B90_CPUE)
summary(B90Land04_lm1)
B90Land04_lm2 <- lm(B90crab04$SumLandings ~ B90crab04$B90_JuvCPUE)
summary(B90Land04_lm2)
B90Land04_lm3 <- lm(B90crab04$SumLandings ~ B90crab04$B90_SubadultCPUE)
summary(B90Land04_lm3)
B90Land04_lm4 <- lm(B90crab04$SumLandings ~ B90crab04$B90_AdultCPUE)
summary(B90Land04_lm4)
B90Land04_lm5 <- lm(B90crab04$SumLandings ~ B90crab04$B90_ImmatureFemaleCPUE)
summary(B90Land04_lm5)
B90Land04_lm6 <- lm(B90crab04$SumLandings ~ B90crab04$B90_ImmatureMaleCPUE)
summary(B90Land04_lm6)
B90Land04_lm7 <- lm(B90crab04$SumLandings ~ B90crab04$B90_MatureFemaleCPUE)
summary(B90Land04_lm7)
B90Land04_lm8 <- lm(B90crab04$SumLandings ~ B90crab04$B90_MatureMaleCPUE)
summary(B90Land04_lm8)
B90Land04_lm9 <- lm(B90crab04$SumLandings ~ B90crab04$B90_SublegalCPUE)
summary(B90Land04_lm9)
B90Land04_lm10 <- lm(B90crab04$SumLandings ~ B90crab04$B90_LegalCPUE)
summary(B90Land04_lm10)

#With Lag

#Insignificant
B90LandLAG04_lm1 <- lm(B90crab04$SumLandings ~ lag(B90crab04$B90_CPUE))
summary(B90LandLAG04_lm1)
B90LandLAG04_lm3 <- lm(B90crab04$SumLandings ~ lag(B90crab04$B90_SubadultCPUE))
summary(B90LandLAG04_lm3)
B90LandLAG04_lm4 <- lm(B90crab04$SumLandings ~ lag(B90crab04$B90_AdultCPUE))
summary(B90LandLAG04_lm4)
B90LandLAG04_lm5 <- lm(B90crab04$SumLandings ~ lag(B90crab04$B90_ImmatureFemaleCPUE))
summary(B90LandLAG04_lm5) #0.0537
B90LandLAG04_lm6 <- lm(B90crab04$SumLandings ~ lag(B90crab04$B90_ImmatureMaleCPUE))
summary(B90LandLAG04_lm6) #0.05815
B90LandLAG04_lm7 <- lm(B90crab04$SumLandings ~ lag(B90crab04$B90_MatureFemaleCPUE))
summary(B90LandLAG04_lm7)
B90LandLAG04_lm8 <- lm(B90crab04$SumLandings ~ lag(B90crab04$B90_MatureMaleCPUE))
summary(B90LandLAG04_lm8)
B90LandLAG04_lm9 <- lm(B90crab04$SumLandings ~ lag(B90crab04$B90_SublegalCPUE))
summary(B90LandLAG04_lm9)
B90LandLAG04_lm10 <- lm(B90crab04$SumLandings ~ lag(B90crab04$B90_LegalCPUE))
summary(B90LandLAG04_lm10)

#Significant
B90LandLAG04_lm2 <- lm(B90crab04$SumLandings ~ lag(B90crab04$B90_JuvCPUE))
summary(B90LandLAG04_lm2)





# B90 Landings CPUE -------------------------------------------------------


###1980###

#Insignificant
B90CPUE_lm3 <- lm(crab$SumLandingsCPUE ~ crab$B90_SubadultCPUE)
summary(B90CPUE_lm3) 
B90CPUE_lm4 <- lm(crab$SumLandingsCPUE ~ crab$B90_AdultCPUE)
summary(B90CPUE_lm4) 
B90CPUE_lm7 <- lm(crab$SumLandingsCPUE ~ crab$B90_MatureFemaleCPUE)
summary(B90CPUE_lm7) 
B90CPUE_lm8 <- lm(crab$SumLandingsCPUE ~ crab$B90_MatureMaleCPUE)
summary(B90CPUE_lm8) 
B90CPUE_lm9 <- lm(crab$SumLandingsCPUE ~ crab$B90_LegalCPUE)
summary(B90CPUE_lm9) 
 
#Significant
B90CPUE_lm1 <- lm(crab$SumLandingsCPUE ~ crab$B90_CPUE)
summary(B90CPUE_lm1) 
B90CPUE_lm2 <- lm(crab$SumLandingsCPUE ~ crab$B90_JuvCPUE)
summary(B90CPUE_lm2) 
B90CPUE_lm5 <- lm(crab$SumLandingsCPUE ~ crab$B90_ImmatureFemaleCPUE)
summary(B90CPUE_lm5)
B90CPUE_lm6 <- lm(crab$SumLandingsCPUE ~ crab$B90_ImmatureMaleCPUE)
summary(B90CPUE_lm6) 
B90CPUE_lm10 <- lm(crab$SumLandingsCPUE ~ crab$B90_SublegalCPUE)
summary(B90CPUE_lm10)


#with LAG

#Insignificant
B90CPUELAG_lm2 <- lm(crab$SumLandingsCPUE ~ lag(crab$B90_JuvCPUE))
summary(B90CPUELAG_lm2)
B90CPUELAG_lm4 <- lm(crab$SumLandingsCPUE ~ lag(crab$B90_AdultCPUE))
summary(B90CPUELAG_lm4)
B90CPUELAG_lm5 <- lm(crab$SumLandingsCPUE ~ lag(crab$B90_ImmatureFemaleCPUE))
summary(B90CPUELAG_lm5)
B90CPUELAG_lm6 <- lm(crab$SumLandingsCPUE ~ lag(crab$B90_MatureFemaleCPUE))
summary(B90CPUELAG_lm6)
B90CPUELAG_lm7 <- lm(crab$SumLandingsCPUE ~ lag(crab$B90_ImmatureMaleCPUE))
summary(B90CPUELAG_lm7)
B90CPUELAG_lm9 <- lm(crab$SumLandingsCPUE ~ lag(crab$B90_LegalCPUE))
summary(B90CPUELAG_lm9)
B90CPUELAG_lm10 <- lm(crab$SumLandingsCPUE ~ lag(crab$B90_SublegalCPUE))
summary(B90CPUELAG_lm10)

#Significant
B90CPUELAG_lm1 <- lm(crab$SumLandingsCPUE ~ lag(crab$B90_CPUE))
summary(B90CPUELAG_lm1) 
B90CPUELAG_lm3 <- lm(crab$SumLandingsCPUE ~ lag(crab$B90_SubadultCPUE))
summary(B90CPUELAG_lm3)
B90CPUELAG_lm8 <- lm(crab$SumLandingsCPUE ~ lag(crab$B90_MatureMaleCPUE))
summary(B90CPUELAG_lm8)




###2004### - WHY DOES THIS SHOW A BIT DIFFERENT RELATIOSNSHIPS? - The dataset should auto cull to 2004, since that's when the dep var begins

#Insignificant
B90CPUE04_lm3 <- lm(B90crab04$SumLandingsCPUE ~ B90crab04$B90_SubadultCPUE)
summary(B90CPUE04_lm3) 
B90CPUE04_lm4 <- lm(B90crab04$SumLandingsCPUE ~ B90crab04$B90_AdultCPUE)
summary(B90CPUE04_lm4) 
B90CPUE04_lm7 <- lm(B90crab04$SumLandingsCPUE ~ B90crab04$B90_MatureFemaleCPUE)
summary(B90CPUE04_lm7) 
B90CPUE04_lm8 <- lm(B90crab04$SumLandingsCPUE ~ B90crab04$B90_MatureMaleCPUE)
summary(B90CPUE04_lm8) 
B90CPUE04_lm9 <- lm(B90crab04$SumLandingsCPUE ~ B90crab04$B90_LegalCPUE)
summary(B90CPUE04_lm9)

#Significant
B90CPUE04_lm1 <- lm(B90crab04$SumLandingsCPUE ~ B90crab04$B90_CPUE)
summary(B90CPUE04_lm1) 
B90CPUE04_lm2 <- lm(B90crab04$SumLandingsCPUE ~ B90crab04$B90_JuvCPUE)
summary(B90CPUE04_lm2)
B90CPUE04_lm5 <- lm(B90crab04$SumLandingsCPUE ~ B90crab04$B90_ImmatureFemaleCPUE)
summary(B90CPUE04_lm5)
B90CPUE04_lm6 <- lm(B90crab04$SumLandingsCPUE ~ B90crab04$B90_ImmatureMaleCPUE)
summary(B90CPUE04_lm6) 
B90CPUE04_lm10 <- lm(B90crab04$SumLandingsCPUE ~ B90crab04$B90_SublegalCPUE)
summary(B90CPUE04_lm10)


#With LAG

#Insignificant
B90CPUELAG04_lm2 <- lm(B90crab04$SumLandingsCPUE ~ lag(B90crab04$B90_JuvCPUE))
summary(B90CPUELAG04_lm2) 
B90CPUELAG04_lm4 <- lm(B90crab04$SumLandingsCPUE ~ lag(B90crab04$B90_AdultCPUE))
summary(B90CPUELAG04_lm4) 
B90CPUELAG04_lm5 <- lm(B90crab04$SumLandingsCPUE ~ lag(B90crab04$B90_ImmatureFemaleCPUE))
summary(B90CPUELAG04_lm5) 
B90CPUELAG04_lm7 <- lm(B90crab04$SumLandingsCPUE ~ lag(B90crab04$B90_ImmatureMaleCPUE))
summary(B90CPUELAG04_lm7) 
B90CPUELAG04_lm9 <- lm(B90crab04$SumLandingsCPUE ~ lag(B90crab04$B90_LegalCPUE))
summary(B90CPUELAG04_lm9) 
B90CPUELAG04_lm10 <- lm(B90crab04$SumLandingsCPUE ~ lag(B90crab04$B90_SublegalCPUE))
summary(B90CPUELAG04_lm10) 


#Significant
B90CPUELAG04_lm1 <- lm(B90crab04$SumLandingsCPUE ~ lag(B90crab04$B90_CPUE))
summary(B90CPUELAG04_lm1) 
B90CPUELAG04_lm3 <- lm(B90crab04$SumLandingsCPUE ~ lag(B90crab04$B90_SubadultCPUE))
summary(B90CPUELAG04_lm3) 
B90CPUELAG04_lm6 <- lm(B90crab04$SumLandingsCPUE ~ lag(B90crab04$B90_MatureFemaleCPUE))
summary(B90CPUELAG04_lm6) 
B90CPUELAG04_lm8 <- lm(B90crab04$SumLandingsCPUE ~ lag(B90crab04$B90_MatureMaleCPUE))
summary(B90CPUELAG04_lm8) 





