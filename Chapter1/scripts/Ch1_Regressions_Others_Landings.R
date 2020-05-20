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


P88crab <- filter(crab, Year > 1987) %>%
  dplyr::select(17:33, 50:52)
P88crab03 <- filter(crab, Year > 2002) %>%
  dplyr::select(17:33, 50:52)



# P88 Landings ------------------------------------------------------------


###1988###

#Insignificant
P88Land_lm1 <- lm(P88crab$LandingsSum ~ P88crab$P88_CPUE)
summary(P88Land_lm1)
P88Land_lm2 <- lm(P88crab$LandingsSum ~ P88crab$P88_SublegalCPUE)
summary(P88Land_lm2)
P88Land_lm3 <- lm(P88crab$LandingsSum ~ P88crab$P88_LegalCPUE)
summary(P88Land_lm3)

#Significant - NONE


#With LAG

#Insignificant
P88LandLAG_lm1 <- lm(P88crab$LandingsSum ~ lag(P88crab$P88_CPUE))
summary(P88LandLAG_lm1)
P88LandLAG_lm2 <- lm(P88crab$LandingsSum ~ lag(P88crab$P88_SublegalCPUE))
summary(P88LandLAG_lm2)
P88LandLAG_lm3 <- lm(P88crab$LandingsSum ~ lag(P88crab$P88_LegalCPUE))
summary(P88LandLAG_lm3)

#Significant - NONE


#With LEad LAG

#Insignificant
P88LandLAGLead_lm1 <- lm(lead(P88crab$LandingsSum) ~ lag(P88crab$P88_CPUE))
summary(P88LandLAGLead_lm1)
P88LandLAGLead_lm2 <- lm(lead(P88crab$LandingsSum) ~ lag(P88crab$P88_SublegalCPUE))
summary(P88LandLAGLead_lm2)


#Significant
P88LandLAGLead_lm3 <- lm(lead(P88crab$LandingsSum) ~ lag(P88crab$P88_LegalCPUE))
summary(P88LandLAGLead_lm3) # 0.0405 - 0.1464


# P88 Landings CPUE -------------------------------------------------------


###1988###

#Insignificant
P88CPUE_lm1 <- lm(P88crab$LandingsCPUEMean ~ P88crab$P88_CPUE)
summary(P88CPUE_lm1)
P88CPUE_lm2 <- lm(P88crab$LandingsCPUEMean ~ P88crab$P88_SublegalCPUE)
summary(P88CPUE_lm2)
P88CPUE_lm3 <- lm(P88crab$LandingsCPUEMean ~ P88crab$P88_LegalCPUE)
summary(P88CPUE_lm3)

#Significant - NONE


#With LAG

#Insignificant
P88CPUELAG_lm1 <- lm(P88crab$LandingsCPUEMean ~ lag(P88crab$P88_CPUE))
summary(P88CPUELAG_lm1)
P88CPUELAG_lm2 <- lm(P88crab$LandingsCPUEMean ~ lag(P88crab$P88_SublegalCPUE))
summary(P88CPUELAG_lm2)
P88CPUELAG_lm3 <- lm(P88crab$LandingsCPUEMean ~ lag(P88crab$P88_LegalCPUE))
summary(P88CPUELAG_lm3)

#Significant - NONE



#With LEad LAG

#Insignificant
P88CPUELAGLead_lm1 <- lm(lead(P88crab$LandingsCPUEMean) ~ lag(P88crab$P88_CPUE))
summary(P88CPUELAGLead_lm1)
P88CPUELAGLead_lm2 <- lm(lead(P88crab$LandingsCPUEMean) ~ lag(P88crab$P88_SublegalCPUE))
summary(P88CPUELAGLead_lm2)
P88CPUELAGLead_lm3 <- lm(lead(P88crab$LandingsCPUEMean) ~ lag(P88crab$P88_LegalCPUE))
summary(P88CPUELAGLead_lm3)

#Significant - NONE



###2003###

#Insignificant
P88CPUE03_lm1 <- lm(P88crab03$LandingsCPUEMean ~ P88crab03$P88_CPUE)
summary(P88CPUE03_lm1)
P88CPUE03_lm2 <- lm(P88crab03$LandingsCPUEMean ~ P88crab03$P88_SublegalCPUE)
summary(P88CPUE03_lm2)
P88CPUE03_lm3 <- lm(P88crab03$LandingsCPUEMean ~ P88crab03$P88_LegalCPUE)
summary(P88CPUE03_lm3)

#Significant - NONE


#With LAG

#Insignificant
P88CPUELAG03_lm1 <- lm(P88crab03$LandingsCPUEMean ~ lag(P88crab03$P88_CPUE))
summary(P88CPUELAG03_lm1)
P88CPUELAG03_lm2 <- lm(P88crab03$LandingsCPUEMean ~ lag(P88crab03$P88_SublegalCPUE))
summary(P88CPUELAG03_lm2)
P88CPUELAG03_lm3 <- lm(P88crab03$LandingsCPUEMean ~ lag(P88crab03$P88_LegalCPUE))
summary(P88CPUELAG03_lm3)

#Significant - NONE



#With LEad LAG

#Insignificant
P88CPUELAGLead03_lm1 <- lm(lead(P88crab03$LandingsCPUEMean) ~ lag(P88crab03$P88_CPUE))
summary(P88CPUELAGLead03_lm1)
P88CPUELAGLead03_lm2 <- lm(lead(P88crab03$LandingsCPUEMean) ~ lag(P88crab03$P88_SublegalCPUE))
summary(P88CPUELAGLead03_lm2)
P88CPUELAGLead03_lm3 <- lm(lead(P88crab03$LandingsCPUEMean) ~ lag(P88crab03$P88_LegalCPUE))
summary(P88CPUELAGLead03_lm3)



# E98 Landings  -----------------------------------------------------------


###1988###

#Insignificant
E98Land_lm1 <- lm(crab$LandingsSum ~ crab$E98_CPUE)
summary(E98Land_lm1)
E98Land_lm2 <- lm(crab$LandingsSum ~ crab$E98_JuvCPUE)
summary(E98Land_lm2)
E98Land_lm3 <- lm(crab$LandingsSum ~ crab$E98_SubadultCPUE)
summary(E98Land_lm3)
E98Land_lm4 <- lm(crab$LandingsSum ~ crab$E98_AdultCPUE)
summary(E98Land_lm4)
E98Land_lm5 <- lm(crab$LandingsSum ~ crab$E98_LegalCPUE)
summary(E98Land_lm5)
E98Land_lm6 <- lm(crab$LandingsSum ~ crab$E98_SublegalCPUE)
summary(E98Land_lm6)

#Significant - NONE


#With LAG

#Insignificant
E98LandLAG_lm1 <- lm(crab$LandingsSum ~ lag(crab$E98_CPUE))
summary(E98LandLAG_lm1)
E98LandLAG_lm2 <- lm(crab$LandingsSum ~ lag(crab$E98_JuvCPUE))
summary(E98LandLAG_lm2)
E98LandLAG_lm3 <- lm(crab$LandingsSum ~ lag(crab$E98_SubadultCPUE))
summary(E98LandLAG_lm3)
E98LandLAG_lm4 <- lm(crab$LandingsSum ~ lag(crab$E98_AdultCPUE))
summary(E98LandLAG_lm4)
E98LandLAG_lm5 <- lm(crab$LandingsSum ~ lag(crab$E98_LegalCPUE))
summary(E98LandLAG_lm5)
E98LandLAG_lm6 <- lm(crab$LandingsSum ~ lag(crab$E98_SublegalCPUE))
summary(E98LandLAG_lm6)

#Significant - NONE


#With Lead LAG

#Insignificant
E98LandLAGLead_lm1 <- lm(lead(crab$LandingsSum) ~ lag(crab$E98_CPUE))
summary(E98LandLAGLead_lm1)
E98LandLAGLead_lm2 <- lm(lead(crab$LandingsSum) ~ lag(crab$E98_JuvCPUE))
summary(E98LandLAGLead_lm2)
E98LandLAGLead_lm3 <- lm(lead(crab$LandingsSum) ~ lag(crab$E98_SubadultCPUE))
summary(E98LandLAGLead_lm3)
E98LandLAGLead_lm4 <- lm(lead(crab$LandingsSum) ~ lag(crab$E98_AdultCPUE))
summary(E98LandLAGLead_lm4)
E98LandLAGLead_lm5 <- lm(lead(crab$LandingsSum) ~ lag(crab$E98_LegalCPUE))
summary(E98LandLAGLead_lm5)
E98LandLAGLead_lm6 <- lm(lead(crab$LandingsSum) ~ lag(crab$E98_SublegalCPUE))
summary(E98LandLAGLead_lm6)

#Significant - NONE


# E98 Landings CPUE -------------------------------------------------------


###1988###

#Insignificant
E98CPUE_lm1 <- lm(crab$LandingsCPUEMean ~ crab$E98_CPUE)
summary(E98CPUE_lm1)
E98CPUE_lm2 <- lm(crab$LandingsCPUEMean ~ crab$E98_JuvCPUE)
summary(E98CPUE_lm2)
E98CPUE_lm3 <- lm(crab$LandingsCPUEMean ~ crab$E98_SubadultCPUE)
summary(E98CPUE_lm3)
E98CPUE_lm4 <- lm(crab$LandingsCPUEMean ~ crab$E98_AdultCPUE)
summary(E98CPUE_lm4)
E98CPUE_lm5 <- lm(crab$LandingsCPUEMean ~ crab$E98_LegalCPUE)
summary(E98CPUE_lm5)
E98CPUE_lm6 <- lm(crab$LandingsCPUEMean ~ crab$E98_SublegalCPUE)
summary(E98CPUE_lm6)

#Significant - NONE


#With LAG

#Insignificant
E98CPUELAG_lm1 <- lm(crab$LandingsCPUEMean ~ lag(crab$E98_CPUE))
summary(E98CPUELAG_lm1)
E98CPUELAG_lm2 <- lm(crab$LandingsCPUEMean ~ lag(crab$E98_JuvCPUE))
summary(E98CPUELAG_lm2)
E98CPUELAG_lm3 <- lm(crab$LandingsCPUEMean ~ lag(crab$E98_SubadultCPUE))
summary(E98CPUELAG_lm3)
E98CPUELAG_lm4 <- lm(crab$LandingsCPUEMean ~ lag(crab$E98_AdultCPUE))
summary(E98CPUELAG_lm4)
E98CPUELAG_lm5 <- lm(crab$LandingsCPUEMean ~ lag(crab$E98_LegalCPUE))
summary(E98CPUELAG_lm5)
E98CPUELAG_lm6 <- lm(crab$LandingsCPUEMean ~ lag(crab$E98_SublegalCPUE))
summary(E98CPUELAG_lm6)

#Significant - NONE



#With Lead LAG

#Insignificant
E98CPUELAGLead_lm1 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$E98_CPUE))
summary(E98CPUELAGLead_lm1)
E98CPUELAGLead_lm2 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$E98_JuvCPUE))
summary(E98CPUELAGLead_lm2)
E98CPUELAGLead_lm3 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$E98_SubadultCPUE))
summary(E98CPUELAGLead_lm3)
E98CPUELAGLead_lm4 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$E98_AdultCPUE))
summary(E98CPUELAGLead_lm4)
E98CPUELAGLead_lm5 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$E98_LegalCPUE))
summary(E98CPUELAGLead_lm5)
E98CPUELAGLead_lm6 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$E98_SublegalCPUE))
summary(E98CPUELAGLead_lm6)

#Significant - NONE


# E99 Landings  -----------------------------------------------------------


###1988###

#Insignificant
E99Land_lm1 <- lm(crab$LandingsSum ~ crab$E99_CPUE)
summary(E99Land_lm1)
E99Land_lm2 <- lm(crab$LandingsSum ~ crab$E99_JuvCPUE)
summary(E99Land_lm2) # 0.05168 - 0.2049
E99Land_lm3 <- lm(crab$LandingsSum ~ crab$E99_SubadultCPUE)
summary(E99Land_lm3)
E99Land_lm4 <- lm(crab$LandingsSum ~ crab$E99_AdultCPUE)
summary(E99Land_lm4)
E99Land_lm5 <- lm(crab$LandingsSum ~ crab$E99_LegalCPUE)
summary(E99Land_lm5)
E99Land_lm6 <- lm(crab$LandingsSum ~ crab$E99_SublegalCPUE)
summary(E99Land_lm6)

#Significant - NONE


#With LAG

#Insignificant
E99LandLAG_lm1 <- lm(crab$LandingsSum ~ lag(crab$E99_CPUE))
summary(E99LandLAG_lm1)
E99LandLAG_lm2 <- lm(crab$LandingsSum ~ lag(crab$E99_JuvCPUE))
summary(E99LandLAG_lm2)
E99LandLAG_lm3 <- lm(crab$LandingsSum ~ lag(crab$E99_SubadultCPUE))
summary(E99LandLAG_lm3) #p=0.05585
E99LandLAG_lm4 <- lm(crab$LandingsSum ~ lag(crab$E99_AdultCPUE))
summary(E99LandLAG_lm4)
E99LandLAG_lm5 <- lm(crab$LandingsSum ~ lag(crab$E99_LegalCPUE))
summary(E99LandLAG_lm5)
E99LandLAG_lm6 <- lm(crab$LandingsSum ~ lag(crab$E99_SublegalCPUE))
summary(E99LandLAG_lm6)

#Significant - NONE

#With LAG

#Insignificant
E99LandLAGLead_lm1 <- lm(lead(crab$LandingsSum) ~ lag(crab$E99_CPUE))
summary(E99LandLAGLead_lm1)
E99LandLAGLead_lm2 <- lm(lead(crab$LandingsSum) ~ lag(crab$E99_JuvCPUE))
summary(E99LandLAGLead_lm2)
E99LandLAGLead_lm3 <- lm(lead(crab$LandingsSum) ~ lag(crab$E99_SubadultCPUE))
summary(E99LandLAGLead_lm3) #p=0.05585
E99LandLAGLead_lm4 <- lm(lead(crab$LandingsSum) ~ lag(crab$E99_AdultCPUE))
summary(E99LandLAGLead_lm4)
E99LandLAGLead_lm5 <- lm(lead(crab$LandingsSum) ~ lag(crab$E99_LegalCPUE))
summary(E99LandLAGLead_lm5)
E99LandLAGLead_lm6 <- lm(lead(crab$LandingsSum) ~ lag(crab$E99_SublegalCPUE))
summary(E99LandLAGLead_lm6)

#Significant - NONE




# E99 Landings CPUE -------------------------------------------------------


###1988###

#Insignificant
E99CPUE_lm1 <- lm(crab$LandingsCPUEMean ~ crab$E99_CPUE)
summary(E99CPUE_lm1)
E99CPUE_lm2 <- lm(crab$LandingsCPUEMean ~ crab$E99_JuvCPUE)
summary(E99CPUE_lm2)
E99CPUE_lm3 <- lm(crab$LandingsCPUEMean ~ crab$E99_SubadultCPUE)
summary(E99CPUE_lm3)
E99CPUE_lm4 <- lm(crab$LandingsCPUEMean ~ crab$E99_AdultCPUE)
summary(E99CPUE_lm4)
E99CPUE_lm5 <- lm(crab$LandingsCPUEMean ~ crab$E99_LegalCPUE)
summary(E99CPUE_lm5)
E99CPUE_lm6 <- lm(crab$LandingsCPUEMean ~ crab$E99_SublegalCPUE)
summary(E99CPUE_lm6)

#Significant - NONE


#With LAG

#Insignificant
E99CPUELAG_lm1 <- lm(crab$LandingsCPUEMean ~ lag(crab$E99_CPUE))
summary(E99CPUELAG_lm1)
E99CPUELAG_lm2 <- lm(crab$LandingsCPUEMean ~ lag(crab$E99_JuvCPUE))
summary(E99CPUELAG_lm2)
E99CPUELAG_lm3 <- lm(crab$LandingsCPUEMean ~ lag(crab$E99_SubadultCPUE))
summary(E99CPUELAG_lm3)
E99CPUELAG_lm4 <- lm(crab$LandingsCPUEMean ~ lag(crab$E99_AdultCPUE))
summary(E99CPUELAG_lm4)
E99CPUELAG_lm5 <- lm(crab$LandingsCPUEMean ~ lag(crab$E99_LegalCPUE))
summary(E99CPUELAG_lm5)
E99CPUELAG_lm6 <- lm(crab$LandingsCPUEMean ~ lag(crab$E99_SublegalCPUE))
summary(E99CPUELAG_lm6)

#Significant - NONE

#With LAG

#Insignificant
E99CPUELAGLead_lm1 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$E99_CPUE))
summary(E99CPUELAGLead_lm1)
E99CPUELAGLead_lm2 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$E99_JuvCPUE))
summary(E99CPUELAGLead_lm2)
E99CPUELAGLead_lm3 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$E99_SubadultCPUE))
summary(E99CPUELAGLead_lm3)
E99CPUELAGLead_lm4 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$E99_AdultCPUE))
summary(E99CPUELAGLead_lm4)
E99CPUELAGLead_lm5 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$E99_LegalCPUE))
summary(E99CPUELAGLead_lm5)
E99CPUELAGLead_lm6 <- lm(lead(crab$LandingsCPUEMean) ~ lag(crab$E99_SublegalCPUE))
summary(E99CPUELAGLead_lm6)

#Significant - NONE


# T06 ---------------------------------------------------------------------

#Insignificant
T06Land_lm1 <- lm(crab$LandingsSum ~ crab$T06_CPUE)
summary(T06Land_lm1)
T06LandLAG_lm1 <- lm(crab$LandingsSum ~ lag(crab$T06_CPUE))
summary(T06LandLAG_lm1)
T06CPUELAG_lm1 <- lm(crab$LandingsCPUEMean ~ lag(crab$T06_CPUE))
summary(T06CPUELAG_lm1)
T06LandLAGLead_lm1 <- lm(lead(crab$LandingsSum) ~ lag(crab$T06_CPUE))
summary(T06LandLAGLead_lm1)
T06CPUELAGLead_lm1 <- lm(lead(crab$LandingsSum) ~ lag(crab$T06_CPUE))
summary(T06CPUELAGLead_lm1)


#Significant
T06CPUE_lm1 <- lm(crab$LandingsCPUEMean ~ crab$T06_CPUE)
summary(T06CPUE_lm1) # 0.01691 - 0.4182








