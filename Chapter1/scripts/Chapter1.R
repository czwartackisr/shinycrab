library(shiny)
library(dplyr)
library(ggplot2)
library(DataCombine)
library(PerformanceAnalytics)
library(knitr)
library(rmarkdown)


# Read in data ------------------------------------------------------------

crab <- read.csv("./Chapter1/data/CH1data.csv", stringsAsFactors = FALSE)


# Correlation plots -------------------------------------------------------

B90corr <- select(crab, 1:7)
chart.Correlation(B90corr, histogram = FALSE, pch=19, method = "kendall")

T38corr <- select(crab, 1, 8:13)
chart.Correlation(T38corr, histogram = FALSE, psh=19, method = "kendall")


# Regressions -------------------------------------------------------------

##NO LAG Regressions

#MeanAnnaulLandingsCPUE ~ Harbor trawl 
B90_lm <- lm(MeanLandingsCPUE ~ B90_CPUE, data = crab)
summary(B90_lm)

#MeanAnnaulLandingsCPUE ~ Trammel Net 
T06_lm <- lm(MeanLandingsCPUE ~ T06_CPUE, data = crab)
summary(T06_lm)

#Additive
NoLagAdd_lm <- lm(MeanLandingsCPUE ~ T06_CPUE + B90_CPUE, data = crab)
summary(NoLagAdd_lm)

#Multiple regression
NoLagMulti_lm <- lm(MeanLandingsCPUE ~ T06_CPUE * B90_CPUE, data = crab)
summary(NoLagMulti_lm)


##1-yr LAG Regressions

#MeanAnnaulLandingsCPUE ~ Harbor trawl 
B90sub_lm <- lm(MeanLandingsCPUE ~ lag(B90_SubadultCPUE), data = crab)
summary(B90sub_lm)
B90sub_lm2 <- lm(MeanLandingsCPUE_1 ~ B90_SubadultCPUE, data = crab)
summary(B90sub_lm2)
B90sub_lm3 <- lm(lead(MeanLandingsCPUE) ~ B90_SubadultCPUE, data = crab)
summary(B90sub_lm3)

#MeanAnnaulLandingsCPUE ~ Trammel Net 
T38sub_lm <- lm(MeanLandingsCPUE ~ lag(T38_SubadultCPUE), data = crab)
summary(T38sub_lm)

#Additive
LagAdd_lm <- lm(MeanLandingsCPUE ~ lag(B90_SubadultCPUE) + lag(T38_SubadultCPUE), data = crab)
summary(LagAdd_lm)

#Multiple regression
LagMulti_lm <- lm(MeanLandingsCPUE ~ lag(B90_SubadultCPUE) * lag(T38_SubadultCPUE), data = crab)
summary(LagMulti_lm)



# Ash/Cooper Landings -----------------------------------------------------

