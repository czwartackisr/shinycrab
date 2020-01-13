library(dplyr)
library(tidyr)
library(ggplot2)
library(PerformanceAnalytics)
library(MuMIn)
library(MASS)
library(knitr)
library(kableExtra)

crab <- read.csv("./Chapter1/reports/Dredge/CH1_LandingsDredgeNEW.csv", stringsAsFactors = FALSE)
#crab is the unprepared for dredge original data



# Significant Single Regress Variables ------------------------------------

#Only those variables that have a predictive relationship as per Single LAndings Regressions
B90CrabRegress <- crab %>%
  filter(Year > 2003) %>%
  dplyr::select(9, 4, 2, 62)

T38CrabRegress <- crab %>%
  filter(Year > 2003) %>%
  dplyr::select(45, 47, 41, 39, 40, 43, 62)

CrabRegress <- crab %>%
  filter(Year > 2003) %>%
  dplyr::select(9, 4, 2, 45, 47, 41, 39, 40, 43, 62)



B90CrabRegress_lm <- lm(LandingsCPUEMean ~ ., data = B90CrabRegress, na.action = "na.fail")
B90CrabRegressDredge <- dredge(B90CrabRegress_lm)
B90CrabRegressDredge

T38CrabRegress_lm <- lm(LandingsCPUEMean ~ ., data = T38CrabRegress, na.action = "na.fail")
T38CrabRegressDredge <- dredge(T38CrabRegress_lm)
T38CrabRegressDredge


CrabRegress_lm <- lm(LandingsCPUEMean ~ ., data = CrabRegress, na.action = "na.fail")
CrabRegressDredge <- dredge(CrabRegress_lm)
CrabRegressDredge




# All Var Dredges ---------------------------------------------------------

B90Landingsdredge <- filter(crab, Year > 2003) %>%
  #dplyr::select(14, 7, 3, 46, 49, 40, 36, 38, 43, 54) Original selecting based-on relevant single regression relationships
  dplyr::select(2:21, 62)

B901yr <- dplyr::select(B90Landingsdredge, 1:10, 21)
B902yr <- dplyr::select(B90Landingsdredge, 11:21)

T38Landingsdredge <- filter(crab, Year > 2003) %>%
  dplyr::select(39:58, 62)

T381yr <- dplyr::select(T38Landingsdredge, 1:10, 21)
T382yr <- dplyr::select(T38Landingsdredge, 11:21)

#  B90 1- and 2-yr variables ----------------------------------------------
B90Landings_lm <- lm(LandingsCPUEMean ~ ., data = B90Landingsdredge, na.action = "na.fail")
B90LandingsRelDredge <- dredge(B90Landings_lm)
B90LandingsdredgeTable <- subset(B90LandingsRelDredge, delta < 2.5)

B90LandingsdredgeTable


# B90 1-yr variables ------------------------------------------------------
B90Landings1yr_lm <- lm(LandingsCPUEMean ~ ., data = B901yr, na.action = "na.fail")
B90Landings1yrRelDredge <- dredge(B90Landings1yr_lm)
B90Landings1yrdredgeTable <- subset(B90Landings1yrRelDredge, delta < 2.5)

B90Landings1yrdredgeTable


# B90 2-yr variables ------------------------------------------------------
B90Landings2yr_lm <- lm(LandingsCPUEMean ~ ., data = B902yr, na.action = "na.fail")
B90Landings2yrRelDredge <- dredge(B90Landings2yr_lm)
B90Landings2yrdredgeTable <- subset(B90Landings2yrRelDredge, delta < 2.5)

B90Landings2yrdredgeTable




#  T38 1- and 2-yr variables ----------------------------------------------
T38Landings_lm <- lm(LandingsCPUEMean ~ ., data = T38Landingsdredge, na.action = "na.fail")
T38LandingsRelDredge <- dredge(T38Landings_lm)
T38LandingsdredgeTable <- subset(T38LandingsRelDredge, delta < 2.5)

T38LandingsdredgeTable


# T38 1-yr variables ------------------------------------------------------
T38Landings1yr_lm <- lm(LandingsCPUEMean ~ ., data = T381yr, na.action = "na.fail")
T38Landings1yrRelDredge <- dredge(T38Landings1yr_lm)
T38Landings1yrdredgeTable <- subset(T38Landings1yrRelDredge, delta < 2.5)

T38Landings1yrdredgeTable


# T38 2-yr variables ------------------------------------------------------
T38Landings2yr_lm <- lm(LandingsCPUEMean ~ ., data = T382yr, na.action = "na.fail")
T38Landings2yrRelDredge <- dredge(T38Landings2yr_lm)
T38Landings2yrdredgeTable <- subset(T38Landings2yrRelDredge, delta < 2.5)

T38Landings2yrdredgeTable
