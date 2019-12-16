library(dplyr)
library(tidyr)
library(ggplot2)
library(PerformanceAnalytics)
library(MuMIn)
library(MASS)

# Read in data ------------------------------------------------------------

crab <- read.csv("./Chapter1/data/CH1data.csv", stringsAsFactors = FALSE)
#crab is the unprepared for dredge original data

# B90 ---------------------------------------------------------------------

#Landings ~ all B90
B90HarborCrab <- filter(crab, Year > 1980) %>%
  dplyr::select(2:16, 52)
B90HarWanCrab <- filter(crab, Year > 1980) %>%
  dplyr::select(2:16, 54)
B90LandingsCrab <- filter(crab, Year > 2003) %>%
  dplyr::select(2:16, 55)


B90Harbor_lm <- lm(ChsHarborLandings ~ ., data = B90HarborCrab, na.action = "na.fail")
B90HarborDredge <- dredge(B90Harbor_lm)
subset(B90HarborDredge, delta < 4)

B90HarWan_lm <- lm(SumWandoHarbor ~ ., data = B90HarWanCrab, na.action = "na.fail")
B90HarWanDredge <- dredge(B90HarWan_lm)
subset(B90HarWanDredge, delta < 4)

B90Landings_lm <- lm(SumLandings ~ ., data = B90LandingsCrab, na.action = "na.fail")
B90LandingsDredge <- dredge(B90Landings_lm)
subset(B90LandingsDredge, delta < 4)



#LandingsCPUE ~ all B90
B90CPUECrab <- filter(crab, Year > 2003) %>%
  dplyr::select(2:16, 61)


B90CPUE_lm <- lm(SumLandingsCPUE ~ ., data = B90CPUECrab, na.action = "na.fail")
B90CPUEDredge <- dredge(B90CPUE_lm)
subset(B90CPUEDredge, delta < 4)





# T38 ---------------------------------------------------------------------

#Landings ~ all T38
T38HarborCrab <- filter(crab, Year > 1980) %>%
  dplyr::select(34:48, 52)
T38HarWanCrab <- filter(crab, Year > 1980) %>%
  dplyr::select(34:48, 54)
T38LandingsCrab <- filter(crab, Year > 2003) %>%
  dplyr::select(34:48, 55)


T38Harbor_lm <- lm(ChsHarborLandings ~ ., data = T38HarborCrab, na.action = "na.fail")
T38HarborDredge <- dredge(T38Harbor_lm)
subset(T38HarborDredge, delta < 4)

T38HarWan_lm <- lm(SumWandoHarbor ~ ., data = T38HarWanCrab, na.action = "na.fail")
T38HarWanDredge <- dredge(T38HarWan_lm)
subset(T38HarWanDredge, delta < 4)

T38Landings_lm <- lm(SumLandings ~ ., data = T38LandingsCrab, na.action = "na.fail")
T38LandingsDredge <- dredge(T38Landings_lm)
subset(T38LandingsDredge, delta < 4)



#LandingsCPUE ~ all T38
T38CPUECrab <- filter(crab, Year > 2003) %>%
  dplyr::select(34:48, 61)


T38CPUE_lm <- lm(SumLandingsCPUE ~ ., data = T38CPUECrab, na.action = "na.fail")
T38CPUEDredge <- dredge(T38CPUE_lm)
subset(T38CPUEDredge, delta < 4)




# T06 ---------------------------------------------------------------------

#Landings ~ T06 - no need to dredge with only one var

T06Landings_lm <- lm(SumLandings ~ T06_CPUE, data = crab)
T06LandingsCPUE_lm <- lm(SumLandingsCPUE ~ T06_CPUE, data = crab)

T06LandingsLAG_lm <- lm(SumLandings ~ lag(T06_CPUE), data = crab)
T06LandingsCPUELAG_lm <- lm(SumLandingsCPUE ~ lag(T06_CPUE), data = crab)

summary(T06Landings_lm)
summary(T06LandingsCPUE_lm)
summary(T06LandingsLAG_lm)
summary(T06LandingsCPUELAG_lm)

##T06 only predicts LandingsCPUE w/out lag P-value=0.0258 r2=0.3194





# P88 ---------------------------------------------------------------------

#Landings ~ all T38
P88HarborCrab <- filter(crab, Year > 1988) %>%
  dplyr::select(29:32, 52)
P88HarWanCrab <- filter(crab, Year > 1988) %>%
  dplyr::select(29:32, 54)
P88LandingsCrab <- filter(crab, Year > 1988) %>%
  dplyr::select(29:32, 55)


P88Harbor_lm <- lm(ChsHarborLandings ~ ., data = P88HarborCrab, na.action = "na.fail")
P88HarborDredge <- dredge(P88Harbor_lm)
subset(P88HarborDredge, delta < 4)

P88HarWan_lm <- lm(SumWandoHarbor ~ ., data = P88HarWanCrab, na.action = "na.fail")
P88HarWanDredge <- dredge(P88HarWan_lm)
subset(P88HarWanDredge, delta < 4)

P88Landings_lm <- lm(SumLandings ~ ., data = P88LandingsCrab, na.action = "na.fail")
P88LandingsDredge <- dredge(P88Landings_lm)
subset(P88LandingsDredge, delta < 4)



#LandingsCPUE ~ all T38
P88CPUECrab <- filter(crab, Year > 2003) %>%
  dplyr::select(29:32, 61)


P88CPUE_lm <- lm(SumLandingsCPUE ~ ., data = P88CPUECrab, na.action = "na.fail")
P88CPUEDredge <- dredge(P88CPUE_lm)
subset(P88CPUEDredge, delta < 4)

P88SubLag_lm <- lm(SumLandingsCPUE ~ P88_SublegalLAG, data = P88CPUECrab)
summary(P88CPUE_lm)

##P88 has no predictive relationship with either landings or landingsCPUE (with or without lag)






# Mulitple Regression -----------------------------------------------------

TotalCPUE = crab %>%
  dplyr::select(1, 2, 17, 23, 29, 33, 34, 49, 52, 54, 61)


# - No LAG
#Multiple regression w/ interaction using all Total CPUEs
TotalHarborINT_lm = lm(TotalCPUE$SumLandings ~ B90_CPUE*E98_CPUE*E99_CPUE*
                      P88_CPUE*T06_CPUE*S16_CPUE, data = TotalCPUE)
summary(TotalHarborINT_lm)

#Multiple Regression (additive) using all Total CPUEs
TotalHarbor_lm = lm(TotalCPUE$SumLandings ~ B90_CPUE+E98_CPUE+E99_CPUE+
                      P88_CPUE+T06_CPUE+S16_CPUE, data = TotalCPUE)
summary(TotalHarbor_lm)

###Only B90 and E98 show relevance in our multiple models


# - LAG

#Multiple regression w/ interaction using all Total CPUEs
TotalHarborLAGINT_lm = lm(TotalCPUE$SumLandings ~ lag(TotalCPUE$B90_CPUE)*lag(TotalCPUE$E98_CPUE)*lag(TotalCPUE$E99_CPUE)*
                      lag(TotalCPUE$P88_CPUE)*lag(TotalCPUE$T06_CPUE)*lag(TotalCPUE$S16_CPUE))
summary(TotalHarborLAGINT_lm)
###Only E98 shows a relationship


#Multiple Regression (additive) using all Total CPUEs
TotalHarborLAG_lm = lm(TotalCPUE$SumLandings ~ lag(B90_CPUE)+lag(E98_CPUE)+lag(E99_CPUE)+
                      lag(P88_CPUE)+lag(T06_CPUE)+lag(S16_CPUE), data = TotalCPUE)
summary(TotalHarborLAG_lm)
###Only B90 Shows a relatioship

B90T38Landings_lm = lm(TotalCPUE$SumLandingsCPUE ~ TotalCPUE$T38_CPUE)
summary(B90T38Landings_lm)










TotalCPUEcrab <-  dplyr::select(dredgeCPUE_T06, 1, 8, 15:19)
TotalDredge <- lm(MeanLandingsCPUE ~ ., data = TotalCPUEcrab, na.action = "na.fail")
dredge(TotalDredge)
subset(dredge(TotalDredge), delta < 5) 

#Landings ~ relevant T38, B90, T06
T06RelevantCrab <- filter(dredgeCPUE, Year > 2005) %>%
  dplyr::select(4, 5:7, 11, 15, 16, 19)
T06RelevantCrabDredge <- lm(MeanLandingsCPUE ~ ., data = T06RelevantCrab, na.action = "na.fail")
subset(dredge(T06RelevantCrabDredge), delta < 5)
##Results = 3 models with <2 D - 1) LandingsCPUE ~ B90 Mature Females, lag(B90 Subadults); 2) ~ B90 Adult, lag(B90 Subadult); 3) ~ B90 Immature Females


#Landings ~ relevant T38, B90, T06
laggedCrab <- dplyr::select(dredgeCPUE_T06, 4, 11, 16, 19)
laggedCrabDredge <- lm(MeanLandingsCPUE ~ ., data = laggedCrab, na.action = "na.fail")
dredge(laggedCrabDredge)



# Additional regressions based-on dredge -----------------------------------

#lag(Subadults) + Mature Female CPUE
B90_lm1 <- lm(MeanLandingsCPUE ~ lag(B90_SubadultCPUE) + B90_MatureFemaleCPUE, data = dredgeCPUE)
summary(B90_lm1)

#lag(Subadults) + Adult
B90_lm2 <- lm(MeanLandingsCPUE ~ lag(B90_SubadultCPUE) + B90_AdultCPUE, data = dredgeCPUE)
summary(B90_lm2)

#Immature Females
B90_lm3 <- lm(MeanLandingsCPUE ~ B90_ImmatureFemaleCPUE, data = dredgeCPUE)
summary(B90_lm3)

#Immature Female CPUE + Mature Female
B90_lm4 <- lm(MeanLandingsCPUE ~ B90_ImmatureFemaleCPUE + B90_MatureFemaleCPUE, data = dredgeCPUE)
summary(B90_lm4)

#Immature Female CPUE + Mature Female
B90_lm5 <- lm(MeanLandingsCPUE ~ B90_AdultCPUE + B90_ImmatureFemaleCPUE, data = dredgeCPUE)
summary(B90_lm5)

#Immature Females
B90_lm6 <- lm(MeanLandingsCPUE ~ B90_SubadultLAG, data = dredgeCPUE)
summary(B90_lm6)

#Creek Trawl - lag(Subadult) 
T38_lm1 <- lm(MeanLandingsCPUE ~ T38_SubadultLAG, data = dredgeCPUE)
summary((T38_lm1))

#Total CPUE - T06/B90 + T38
Total_lm1 <- lm(MeanLandingsCPUE ~ T06B90_CPUE + T38_CPUE, data = dredgeCPUE)
summary(Total_lm1)

#Total CPUE - T06/B90 
Total_lm2 <- lm(MeanLandingsCPUE ~ T06B90_CPUE, data = dredgeCPUE)
summary(Total_lm2)

#Total CPUE - B90 + T06/B90 + T38 
Total_lm3 <- lm(MeanLandingsCPUE ~ B90_CPUE + T06B90_CPUE + T38_CPUE, data = dredgeCPUE)
summary(Total_lm3)

#Total CPUE - T06 + T06/B90 + T38 
Total_lm4 <- lm(MeanLandingsCPUE ~ T06_CPUE + T06B90_CPUE + T38_CPUE, data = dredgeCPUE)
summary(Total_lm4)

#Total CPUE - B90 + T06 + T38 
Total_lm5 <- lm(MeanLandingsCPUE ~ B90_CPUE + T06_CPUE + T38_CPUE, data = dredgeCPUE)
summary(Total_lm5)

#Total CPUE - B90 
Total_lm6 <- lm(MeanLandingsCPUE ~ B90_CPUE, data = dredgeCPUE)
summary(Total_lm6)

#All Relevant Vars - B90_Immature
All_lm1 <- lm(MeanLandingsCPUE ~ B90_ImmatureFemaleCPUE, data = dredgeCPUE)
summary(All_lm1)

#All Relevant Vars - 
All_lm2 <- lm(MeanLandingsCPUE ~ B90_MatureFemaleCPUE + B90_SubadultLAG, data = dredgeCPUE)
summary(All_lm2)

#All Relevant Vars - 
All_lm3 <- lm(MeanLandingsCPUE ~ B90_AdultCPUE * B90_SubadultLAG, data = dredgeCPUE)
summary(All_lm3)

#All Relevant Vars - 
All_lm4 <- lm(MeanLandingsCPUE ~ B90_MatureFemaleCPUE + T38B90_SubadultLAG, data = dredgeCPUE)
summary(All_lm4)

#All Relevant Vars - 
All_lm5 <- lm(MeanLandingsCPUE ~ B90_AdultCPUE + T38B90_SubadultLAG, data = dredgeCPUE)
summary(All_lm5)

#All Relevant Vars - 
All_lm6 <- lm(MeanLandingsCPUE ~ T38B90_SubadultLAG, data = dredgeCPUE)
summary(All_lm6)

#All Relevant Vars - 
All_lm7 <- lm(MeanLandingsCPUE ~ B90_ImmatureFemaleCPUE + B90_MatureFemaleCPUE, data = dredgeCPUE)
summary(All_lm7)

#All Relevant Vars - 
All_lm8 <- lm(MeanLandingsCPUE ~ B90_AdultCPUE + B90_ImmatureFemaleCPUE, data = dredgeCPUE)
summary(All_lm8)

#lag(Subadult)
lag_lm1 <- lm(MeanLandingsCPUE ~ dredgeCPUE$T38B90_SubadultLAG, data = dredgeCPUE)
summary(lag_lm1)

lag_lm2 <- lm(MeanLandingsCPUE ~ T38_SubadultLAG + B90_SubadultLAG, data = dredgeCPUE)
summary(lag_lm2)

lag_lm3 <- lm(MeanLandingsCPUE ~ B90_SubadultLAG, data = dredgeCPUE)
summary(lag_lm3)

lag_lm4 <- lm(MeanLandingsCPUE ~ T38_SubadultLAG, data = dredgeCPUE)
summary(lag_lm4)
