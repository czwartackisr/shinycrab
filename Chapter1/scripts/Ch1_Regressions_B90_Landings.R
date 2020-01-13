library(dplyr)
library(tidyr)
library(ggplot2)
library(PerformanceAnalytics)
library(MuMIn)
library(MASS)

# Read in data ------------------------------------------------------------

crab <- read.csv("./Chapter1/data/CH1data.csv", stringsAsFactors = FALSE)

crab03 = crab %>%
  filter(Year >2002)



B90crab <- filter(crab, Year > 1979) %>%
  dplyr::select(2:16, 50:52)
B90crab03 <- filter(crab, Year > 2002) %>%
  dplyr::select(2:16, 50:52)



# B90 Landings ------------------------------------------------------------


###1980###

#Insignificant
B90Land_lm1 <- lm(B90crab$LandingsSum ~ B90crab$B90_CPUE)
summary(B90Land_lm1)
B90Land_lm2 <- lm(B90crab$LandingsSum ~ B90crab$B90_JuvCPUE)
summary(B90Land_lm2)
B90Land_lm3 <- lm(B90crab$LandingsSum ~ B90crab$B90_SubadultCPUE)
summary(B90Land_lm3)
B90Land_lm5 <- lm(B90crab$LandingsSum ~ B90crab$B90_ImmatureFemaleCPUE)
summary(B90Land_lm5)
B90Land_lm6 <- lm(B90crab$LandingsSum ~ B90crab$B90_ImmatureMaleCPUE)
summary(B90Land_lm6)
B90Land_lm9 <- lm(B90crab$LandingsSum ~ B90crab$B90_SublegalCPUE)
summary(B90Land_lm9)

#Significant
B90Land_lm4 <- lm(crab$LandingsSum ~ crab$B90_AdultCPUE)
summary(B90Land_lm4)
B90Land_lm7 <- lm(crab$LandingsSum ~ crab$B90_MatureFemaleCPUE)
summary(B90Land_lm7)
B90Land_lm8 <- lm(crab$LandingsSum ~ crab$B90_MatureMaleCPUE)
summary(B90Land_lm8)
B90Land_lm10 <- lm(crab$LandingsSum ~ crab$B90_LegalCPUE)
summary(B90Land_lm10)


#With Lag

#Insignificant
B90LandLAG_lm1 <- lm(B90crab$LandingsSum ~ lag(B90crab$B90_CPUE))
summary(B90LandLAG_lm1)
B90LandLAG_lm2 <- lm(B90crab$LandingsSum ~ lag(B90crab$B90_JuvCPUE))
summary(B90LandLAG_lm2)
B90LandLAG_lm3 <- lm(B90crab$LandingsSum ~ lag(B90crab$B90_SubadultCPUE))
summary(B90LandLAG_lm3)
B90LandLAG_lm4 <- lm(B90crab$LandingsSum ~ lag(B90crab$B90_AdultCPUE))
summary(B90LandLAG_lm4)
B90LandLAG_lm5 <- lm(B90crab$LandingsSum ~ lag(B90crab$B90_ImmatureFemaleCPUE))
summary(B90LandLAG_lm5)
B90LandLAG_lm6 <- lm(B90crab$LandingsSum ~ lag(B90crab$B90_ImmatureMaleCPUE))
summary(B90LandLAG_lm6)
B90LandLAG_lm7 <- lm(B90crab$LandingsSum ~ lag(B90crab$B90_MatureFemaleCPUE))
summary(B90LandLAG_lm7)
B90LandLAG_lm9 <- lm(B90crab$LandingsSum ~ lag(B90crab$B90_SublegalCPUE))
summary(B90LandLAG_lm9)
B90LandLAG_lm10 <- lm(B90crab$LandingsSum ~ lag(B90crab$B90_LegalCPUE))
summary(B90LandLAG_lm10)

#Significant 
B90LandLAG_lm8 <- lm(crab$LandingsSum ~ lag(crab$B90_MatureMaleCPUE))
summary(B90LandLAG_lm8) #0.04897 - 0.1034


## Lead Lag
#With Lag

#Insignificant
B90LandLAGLead_lm1 <- lm(lead(B90crab$LandingsSum) ~ lag(B90crab$B90_CPUE))
summary(B90LandLAGLead_lm1)
B90LandLAGLead_lm2 <- lm(lead(B90crab$LandingsSum) ~ lag(B90crab$B90_JuvCPUE))
summary(B90LandLAGLead_lm2)
B90LandLAGLead_lm3 <- lm(lead(B90crab$LandingsSum) ~ lag(B90crab$B90_SubadultCPUE))
summary(B90LandLAGLead_lm3)
B90LandLAGLead_lm4 <- lm(lead(B90crab$LandingsSum) ~ lag(B90crab$B90_AdultCPUE))
summary(B90LandLAGLead_lm4)
B90LandLAGLead_lm5 <- lm(lead(B90crab$LandingsSum) ~ lag(B90crab$B90_ImmatureFemaleCPUE))
summary(B90LandLAGLead_lm5)
B90LandLAGLead_lm6 <- lm(lead(B90crab$LandingsSum) ~ lag(B90crab$B90_ImmatureMaleCPUE))
summary(B90LandLAGLead_lm6)
B90LandLAGLead_lm7 <- lm(lead(B90crab$LandingsSum) ~ lag(B90crab$B90_MatureFemaleCPUE))
summary(B90LandLAGLead_lm7)
B90LandLAGLead_lm8 <- lm(lead(B90crab$LandingsSum) ~ lag(B90crab$B90_MatureMaleCPUE))
summary(B90LandLAGLead_lm8) 
B90LandLAGLead_lm9 <- lm(lead(B90crab$LandingsSum) ~ lag(B90crab$B90_SublegalCPUE))
summary(B90LandLAGLead_lm9)
B90LandLAGLead_lm10 <- lm(lead(B90crab$LandingsSum) ~ lag(B90crab$B90_LegalCPUE))
summary(B90LandLAGLead_lm10)



###2003###

#Insignificant
B90Land03_lm1 <- lm(B90crab03$LandingsSum ~ B90crab03$B90_CPUE)
summary(B90Land03_lm1)
B90Land03_lm2 <- lm(B90crab03$LandingsSum ~ B90crab03$B90_JuvCPUE)
summary(B90Land03_lm2)
B90Land03_lm3 <- lm(B90crab03$LandingsSum ~ B90crab03$B90_SubadultCPUE)
summary(B90Land03_lm3)
B90Land03_lm4 <- lm(B90crab03$LandingsSum ~ B90crab03$B90_AdultCPUE)
summary(B90Land03_lm4)
B90Land03_lm5 <- lm(B90crab03$LandingsSum ~ B90crab03$B90_ImmatureFemaleCPUE)
summary(B90Land03_lm5)
B90Land03_lm6 <- lm(B90crab03$LandingsSum ~ B90crab03$B90_ImmatureMaleCPUE)
summary(B90Land03_lm6)
B90Land03_lm7 <- lm(B90crab03$LandingsSum ~ B90crab03$B90_MatureFemaleCPUE)
summary(B90Land03_lm7)
B90Land03_lm8 <- lm(B90crab03$LandingsSum ~ B90crab03$B90_MatureMaleCPUE)
summary(B90Land03_lm8)
B90Land03_lm9 <- lm(B90crab03$LandingsSum ~ B90crab03$B90_SublegalCPUE)
summary(B90Land03_lm9)
B90Land03_lm10 <- lm(B90crab03$LandingsSum ~ B90crab03$B90_LegalCPUE)
summary(B90Land03_lm10)


#With Lag

#Insignificant
B90LandLAG03_lm1 <- lm(B90crab03$LandingsSum ~ lag(B90crab03$B90_CPUE))
summary(B90LandLAG03_lm1)
B90LandLAG03_lm2 <- lm(B90crab03$LandingsSum ~ lag(B90crab03$B90_JuvCPUE))
summary(B90LandLAG_lm2)
B90LandLAG03_lm3 <- lm(B90crab03$LandingsSum ~ lag(B90crab03$B90_SubadultCPUE))
summary(B90LandLAG03_lm3)
B90LandLAG03_lm4 <- lm(B90crab03$LandingsSum ~ lag(B90crab03$B90_AdultCPUE))
summary(B90LandLAG03_lm4)
B90LandLAG03_lm5 <- lm(B90crab03$LandingsSum ~ lag(B90crab03$B90_ImmatureFemaleCPUE))
summary(B90LandLAG03_lm5)
B90LandLAG03_lm6 <- lm(B90crab03$LandingsSum ~ lag(B90crab03$B90_ImmatureMaleCPUE))
summary(B90LandLAG03_lm6)
B90LandLAG03_lm7 <- lm(B90crab03$LandingsSum ~ lag(B90crab03$B90_MatureFemaleCPUE))
summary(B90LandLAG03_lm7)
B90LandLAG03_lm8 <- lm(B90crab03$LandingsSum ~ lag(B90crab03$B90_MatureMaleCPUE))
summary(B90LandLAG03_lm8) 
B90LandLAG03_lm9 <- lm(B90crab03$LandingsSum ~ lag(B90crab03$B90_SublegalCPUE))
summary(B90LandLAG03_lm9)
B90LandLAG03_lm10 <- lm(B90crab03$LandingsSum ~ lag(B90crab03$B90_LegalCPUE))
summary(B90LandLAG03_lm10)

#Significant - NONE


## Lead Lag

#Insignificant
B90LandLAGLead03_lm1 <- lm(lead(B90crab$LandingsSum) ~ lag(B90crab$B90_CPUE))
summary(B90LandLAGLead03_lm1)
B90LandLAGLead03_lm2 <- lm(lead(B90crab$LandingsSum) ~ lag(B90crab$B90_JuvCPUE))
summary(B90LandLAGLead03_lm2)
B90LandLAGLead03_lm3 <- lm(lead(B90crab$LandingsSum) ~ lag(B90crab$B90_SubadultCPUE))
summary(B90LandLAGLead03_lm3)
B90LandLAGLead03_lm4 <- lm(lead(B90crab$LandingsSum) ~ lag(B90crab$B90_AdultCPUE))
summary(B90LandLAGLead03_lm4)
B90LandLAGLead03_lm5 <- lm(lead(B90crab$LandingsSum) ~ lag(B90crab$B90_ImmatureFemaleCPUE))
summary(B90LandLAGLead03_lm5)
B90LandLAGLead03_lm6 <- lm(lead(B90crab$LandingsSum) ~ lag(B90crab$B90_ImmatureMaleCPUE))
summary(B90LandLAGLead03_lm6)
B90LandLAGLead03_lm7 <- lm(lead(B90crab$LandingsSum) ~ lag(B90crab$B90_MatureFemaleCPUE))
summary(B90LandLAGLead03_lm7)
B90LandLAGLead03_lm8 <- lm(lead(B90crab$LandingsSum) ~ lag(B90crab$B90_MatureMaleCPUE))
summary(B90LandLAGLead03_lm8) 
B90LandLAGLead03_lm9 <- lm(lead(B90crab$LandingsSum) ~ lag(B90crab$B90_SublegalCPUE))
summary(B90LandLAGLead03_lm9)
B90LandLAGLead03_lm10 <- lm(lead(B90crab$LandingsSum) ~ lag(B90crab$B90_LegalCPUE))
summary(B90LandLAGLead03_lm10)



# B90 Landings CPUE -------------------------------------------------------


###1980###

#Insignificant
B90CPUE_lm1 <- lm(crab$LandingsCPUEMean ~ crab$B90_CPUE)
summary(B90CPUE_lm1) 
B90CPUE_lm3 <- lm(crab$LandingsCPUEMean ~ crab$B90_SubadultCPUE)
summary(B90CPUE_lm3) 
B90CPUE_lm4 <- lm(crab$LandingsCPUEMean ~ crab$B90_AdultCPUE)
summary(B90CPUE_lm4) 
B90CPUE_lm7 <- lm(crab$LandingsCPUEMean ~ crab$B90_MatureFemaleCPUE)
summary(B90CPUE_lm7) 
B90CPUE_lm8 <- lm(crab$LandingsCPUEMean ~ crab$B90_MatureMaleCPUE)
summary(B90CPUE_lm8) 
B90CPUE_lm9 <- lm(crab$LandingsCPUEMean ~ crab$B90_LegalCPUE)
summary(B90CPUE_lm9) 
B90CPUE_lm10 <- lm(crab$LandingsCPUEMean ~ crab$B90_SublegalCPUE)
summary(B90CPUE_lm10)
 
#Significant
B90CPUE_lm2 <- lm(crab$LandingsCPUEMean ~ crab$B90_JuvCPUE)
summary(B90CPUE_lm2) #0.05115 - 0.2619
B90CPUE_lm5 <- lm(crab$LandingsCPUEMean ~ crab$B90_ImmatureFemaleCPUE)
summary(B90CPUE_lm5) #0.03525 - 0.2981
B90CPUE_lm6 <- lm(crab$LandingsCPUEMean ~ crab$B90_ImmatureMaleCPUE)
summary(B90CPUE_lm6) # 0.03943 - 0.2873


#with LAG

#Insignificant
B90CPUELAG_lm2 <- lm(crab$LandingsCPUEMean ~ lag(crab$B90_JuvCPUE))
summary(B90CPUELAG_lm2)
B90CPUELAG_lm4 <- lm(crab$LandingsCPUEMean ~ lag(crab$B90_AdultCPUE))
summary(B90CPUELAG_lm4)
B90CPUELAG_lm5 <- lm(crab$LandingsCPUEMean ~ lag(crab$B90_ImmatureFemaleCPUE))
summary(B90CPUELAG_lm5)
B90CPUELAG_lm6 <- lm(crab$LandingsCPUEMean ~ lag(crab$B90_MatureFemaleCPUE))
summary(B90CPUELAG_lm6)
B90CPUELAG_lm7 <- lm(crab$LandingsCPUEMean ~ lag(crab$B90_ImmatureMaleCPUE))
summary(B90CPUELAG_lm7)
B90CPUELAG_lm9 <- lm(crab$LandingsCPUEMean ~ lag(crab$B90_LegalCPUE))
summary(B90CPUELAG_lm9)
B90CPUELAG_lm10 <- lm(crab$LandingsCPUEMean ~ lag(crab$B90_SublegalCPUE))
summary(B90CPUELAG_lm10)

#Significant
B90CPUELAG_lm1 <- lm(crab$LandingsCPUEMean ~ lag(crab$B90_CPUE))
summary(B90CPUELAG_lm1) # 0.02771 - 0.3208
B90CPUELAG_lm3 <- lm(crab$LandingsCPUEMean ~ lag(crab$B90_SubadultCPUE))
summary(B90CPUELAG_lm3) # 0.01668 - 0.367
B90CPUELAG_lm8 <- lm(crab$LandingsCPUEMean ~ lag(crab$B90_MatureMaleCPUE))
summary(B90CPUELAG_lm8) # 0.007659 - 0.433



#with Lead LAG

#Insignificant

B90CPUELAGLead_lm1 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$B90_CPUE))
summary(B90CPUELAGLead_lm1)
B90CPUELAGLead_lm2 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$B90_JuvCPUE))
summary(B90CPUELAGLead_lm2) 
B90CPUELAGLead_lm3 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$B90_SubadultCPUE))
summary(B90CPUELAGLead_lm3)
B90CPUELAGLead_lm4 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$B90_AdultCPUE))
summary(B90CPUELAGLead_lm4)
B90CPUELAGLead_lm5 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$B90_ImmatureFemaleCPUE))
summary(B90CPUELAGLead_lm5)
B90CPUELAGLead_lm6 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$B90_MatureFemaleCPUE))
summary(B90CPUELAGLead_lm6)
B90CPUELAGLead_lm7 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$B90_ImmatureMaleCPUE))
summary(B90CPUELAGLead_lm7)
B90CPUELAGLead_lm8 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$B90_MatureMaleCPUE))
summary(B90CPUELAGLead_lm8)
B90CPUELAGLead_lm9 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$B90_LegalCPUE))
summary(B90CPUELAGLead_lm9)
B90CPUELAGLead_lm10 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$B90_SublegalCPUE))
summary(B90CPUELAGLead_lm10)
 ###START HERE AFTER LUNCH###



# Multiple Regreesion (suggested by Dredge) -------------------------------

dredge_lm <- lm(crab$LandingsCPUEMean ~ lag(crab$B90_MatureMaleCPUE)*lag(crab$T38_SubadultCPUE))
summary(dredge_lm)

dredgeAdd_lm <- lm(crab$LandingsCPUEMean ~ lag(crab$B90_MatureMaleCPUE)+lag(crab$T38_SubadultCPUE))
summary(dredgeAdd_lm)


# 2004 Exploration --------------------------------------------------------

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




###2004### - WHY DOES THIS SHOW A BIT DIFFERENT RELATIOSNSHIPS? - The dataset should auto cull to 2004, since that's when the dep var begins

#Insignificant
B90CPUE04_lm3 <- lm(B90crab04$LandingsCPUEMean ~ B90crab04$B90_SubadultCPUE)
summary(B90CPUE04_lm3) 
B90CPUE04_lm4 <- lm(B90crab04$LandingsCPUEMean ~ B90crab04$B90_AdultCPUE)
summary(B90CPUE04_lm4) 
B90CPUE04_lm7 <- lm(B90crab04$LandingsCPUEMean ~ B90crab04$B90_MatureFemaleCPUE)
summary(B90CPUE04_lm7) 
B90CPUE04_lm8 <- lm(B90crab04$LandingsCPUEMean ~ B90crab04$B90_MatureMaleCPUE)
summary(B90CPUE04_lm8) 
B90CPUE04_lm9 <- lm(B90crab04$LandingsCPUEMean ~ B90crab04$B90_LegalCPUE)
summary(B90CPUE04_lm9)

#Significant
B90CPUE04_lm1 <- lm(B90crab04$LandingsCPUEMean ~ B90crab04$B90_CPUE)
summary(B90CPUE04_lm1) 
B90CPUE04_lm2 <- lm(B90crab04$LandingsCPUEMean ~ B90crab04$B90_JuvCPUE)
summary(B90CPUE04_lm2)
B90CPUE04_lm5 <- lm(B90crab04$LandingsCPUEMean ~ B90crab04$B90_ImmatureFemaleCPUE)
summary(B90CPUE04_lm5)
B90CPUE04_lm6 <- lm(B90crab04$LandingsCPUEMean ~ B90crab04$B90_ImmatureMaleCPUE)
summary(B90CPUE04_lm6) 
B90CPUE04_lm10 <- lm(B90crab04$LandingsCPUEMean ~ B90crab04$B90_SublegalCPUE)
summary(B90CPUE04_lm10)


#With LAG

#Insignificant
B90CPUELAG04_lm2 <- lm(B90crab04$LandingsCPUEMean ~ lag(B90crab04$B90_JuvCPUE))
summary(B90CPUELAG04_lm2) 
B90CPUELAG04_lm4 <- lm(B90crab04$LandingsCPUEMean ~ lag(B90crab04$B90_AdultCPUE))
summary(B90CPUELAG04_lm4) 
B90CPUELAG04_lm5 <- lm(B90crab04$LandingsCPUEMean ~ lag(B90crab04$B90_ImmatureFemaleCPUE))
summary(B90CPUELAG04_lm5) 
B90CPUELAG04_lm7 <- lm(B90crab04$LandingsCPUEMean ~ lag(B90crab04$B90_ImmatureMaleCPUE))
summary(B90CPUELAG04_lm7) 
B90CPUELAG04_lm9 <- lm(B90crab04$LandingsCPUEMean ~ lag(B90crab04$B90_LegalCPUE))
summary(B90CPUELAG04_lm9) 
B90CPUELAG04_lm10 <- lm(B90crab04$LandingsCPUEMean ~ lag(B90crab04$B90_SublegalCPUE))
summary(B90CPUELAG04_lm10) 


#Significant
B90CPUELAG04_lm1 <- lm(B90crab04$LandingsCPUEMean ~ lag(B90crab04$B90_CPUE))
summary(B90CPUELAG04_lm1) 
B90CPUELAG04_lm3 <- lm(B90crab04$LandingsCPUEMean ~ lag(B90crab04$B90_SubadultCPUE))
summary(B90CPUELAG04_lm3) 
B90CPUELAG04_lm6 <- lm(B90crab04$LandingsCPUEMean ~ lag(B90crab04$B90_MatureFemaleCPUE))
summary(B90CPUELAG04_lm6) 
B90CPUELAG04_lm8 <- lm(B90crab04$LandingsCPUEMean ~ lag(B90crab04$B90_MatureMaleCPUE))
summary(B90CPUELAG04_lm8) 





