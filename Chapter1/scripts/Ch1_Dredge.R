library(dplyr)
library(tidyr)
library(ggplot2)
library(PerformanceAnalytics)
library(MuMIn)
library(MASS)

# Read in data ------------------------------------------------------------

crab <- read.csv("./Chapter1/data/CH1data.csv", stringsAsFactors = FALSE)
#crab is the unprepared for dredge original data


#dredgecrab = relevant variables lagged and added to original "crab" data file.  Includes lag vars too
dredgeCPUE <- read.csv("./Chapter1/data/CH1Dredgedata_CPUE.csv", stringsAsFactors = FALSE)

#Dredge must have all variables with the same n.  
#T06 is the only variable that needs to be separated due to beginning in 2006
#lag(T06) lhas no explanatory power for Mean Annual CPUE
dredgeCPUE_T06 <- filter(dredgeCPUE, Year > 2005)



# Dredge ------------------------------------------------------------------


#<5 keeps it realistic
#Landings ~ all B90
B90crab <- filter(dredgeCPUE, Year > 2003) %>%
  dplyr::select(1:8, 19)
B90Dredge <- lm(MeanLandingsCPUE ~ ., data = B90crab, na.action = "na.fail")
subset(dredge(B90Dredge), delta < 5) 

#Landings ~ all T38
T38crab <- filter(dredgeCPUE, Year > 2003) %>%
  dplyr::select(1, 9:15, 19)
T38Dredge <- lm(MeanLandingsCPUE ~ ., data = T38crab, na.action = "na.fail")
subset(dredge(T38Dredge), delta < 5)

#Landings ~ T06 - no need to dredge with only one var
T06_lm <- lm(MeanLandingsCPUE ~ T06_CPUE, data = subset(dredgeCPUE, Year>2005), na.action = "na.fail")
summary(T06_lm)

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
