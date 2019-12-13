library(dplyr)
library(ggplot2)
library(PerformanceAnalytics)
library(MuMIn)

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

#B90/T06 combined regression
B90T06_lm <- lm(MeanLandingsCPUE ~ T06B90_CPUE, data = crab)
summary(B90T06_lm)

#B90/T38 lag subadult combined regression
B90T38_lm <- lm(MeanLandingsCPUE ~ lag(T38B90_Subadult), data = crab)
summary(B90T38_lm)

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



# Mixed lag (all iv) variable regression ----------------------------------

#Additive model with 2 lagged subadult Harbor and Creek Trawl variables, and no lagged 
land_lm <- lm(MeanLandingsCPUE ~ lag(B90_SubadultCPUE) + lag(T38_SubadultCPUE) + T06_CPUE + B90_CPUE, data = crab)
summary(land_lm)

landAdd_lm <- lm(MeanLandingsCPUE ~ 
                lag(B90_SubadultCPUE) * lag(T38_SubadultCPUE) * T06_CPUE * B90_CPUE, data = crab)
summary(landAdd_lm)




# Dredge ------------------------------------------------------------------

dredgeCrab <- read.csv("./Chapter1/data/CH1Dredgedata.csv", stringsAsFactors = FALSE)

# To be used for all dredges using T06
crab_T06 <- filter(dredgeCrab, Year > 2005)

#To be used for all dredges without T06
crab2 <- filter(dredgeCrab, Year > 2003) %>%
  select(-c(18, 17, 20))

#LandingsCPUE ~ lag(T38 subadult) + lag(B90 subadult) + B90
subadultB90Total_dredge <- lm(MeanLandingsCPUE ~ T38_SubadultLAG + B90_SubadultLAG + B90_CPUE, 
                data = crab2, na.action = "na.fail")

dredge(subadultB90Total_dredge)

#Landings ~ all B90
B90crab <- filter(dredgeCrab, Year > 2003) %>%
                    select(1:8, 19)
B90Dredge <- lm(MeanLandingsCPUE ~ ., data = B90crab, na.action = "na.fail")
dredge(B90Dredge)

#Landings ~ all T38
T38crab <- filter(dredgeCrab, Year > 2003) %>%
  select(1, 9:15, 19)
T38Dredge <- lm(MeanLandingsCPUE ~ ., data = T38crab, na.action = "na.fail")
dredge(T38Dredge)

#Landings ~ relevant T38, B90, T06
T06crab <- filter(dredgeCrab, Year > 2005) %>%
  select(1, 4, 8, 11, 15:20)

T06Dredge <- lm(MeanLandingsCPUE ~ ., data = T06crab, na.action = "na.fail")
dredge(T06Dredge)


##Results = 3 models with <2 D - 1) LandingsCPUE ~ B90 Mature Females, lag(B90 Subadults); 2) ~ B90 Adult, lag(B90 Subadult); 3) ~ B90 Immature Females



# Additional regressions based-on dredge -----------------------------------

#lag(Subadults) * CPUE
B90_lm1 <- lm(MeanLandingsCPUE ~ lag(B90_SubadultCPUE) * B90_CPUE, data = B90crab)
summary((B90_lm1))

#lag(Subadults) * Adult
B90_lm2 <- lm(MeanLandingsCPUE ~ lag(B90_SubadultCPUE) * B90_AdultCPUE, data = B90crab)
summary((B90_lm2))

#Immature Females
B90_lm3 <- lm(MeanLandingsCPUE ~ B90_ImmatureFemaleCPUE, data = B90crab)
summary((B90_lm3))

#Immature Females
T38_lm1 <- lm(MeanLandingsCPUE ~ T38_SubadultLAG, data = T38crab)
summary((T38_lm1))

T06_lm1 <- lm(MeanLandingsCPUE ~ T38_CPUE, data = T38crab)
summary((T38_lm1))

