library(dplyr)
library(tidyr)
library(ggplot2)
library(PerformanceAnalytics)
library(MuMIn)
library(MASS)

# Read in data ------------------------------------------------------------

crab <- read.csv("./Chapter1/data/CH1data.csv", stringsAsFactors = FALSE)



###Only regressions with a lagged variable were tested, because what is the point of an index of abundance if no lag?
# B90 ---------------------------------------------------------------------


#B90_CPUE~B90
B90Abun_lm1 <- lm(crab$B90_CPUE ~ lag(crab$B90_CPUE))
summary(B90Abun_lm1)
B90Abun_lm2 <- lm(crab$B90_CPUE ~ lag(crab$B90_JuvCPUE))
summary(B90Abun_lm2)
B90Abun_lm3 <- lm(crab$B90_CPUE ~ lag(crab$B90_SubadultCPUE))
summary(B90Abun_lm3)
B90Abun_lm4<- lm(crab$B90_CPUE ~ lag(crab$B90_AdultCPUE))
summary(B90Abun_lm4)
B90Abun_lm5 <- lm(crab$B90_CPUE ~ lag(crab$B90_ImmatureFemaleCPUE))
summary(B90Abun_lm5)
B90Abun_lm6 <- lm(crab$B90_CPUE ~ lag(crab$B90_ImmatureMaleCPUE))
summary(B90Abun_lm6)
B90Abun_lm7 <- lm(crab$B90_CPUE ~ lag(crab$B90_MatureFemaleCPUE))
summary(B90Abun_lm7)
B90Abun_lm8 <- lm(crab$B90_CPUE ~ lag(crab$B90_MatureMaleCPUE))
summary(B90Abun_lm8)
B90Abun_lm9 <- lm(crab$B90_CPUE ~ lag(crab$B90_SublegalCPUE))
summary(B90Abun_lm9)
B90Abun_lm10 <- lm(crab$B90_CPUE ~ lag(crab$B90_LegalCPUE))
summary(B90Abun_lm10)
### All Insignificant


#B90~T38
#B90_CPUE~T38
B90AbunT38_lm1 <- lm(crab$B90_CPUE ~ lag(crab$T38_CPUE))
summary(B90AbunT38_lm1)
B90AbunT38_lm2 <- lm(crab$B90_CPUE ~ lag(crab$T38_JuvCPUE))
summary(B90AbunT38_lm2)
B90AbunT38_lm3 <- lm(crab$B90_CPUE ~ lag(crab$T38_SubadultCPUE))
summary(B90AbunT38_lm3)
B90AbunT38_lm4 <- lm(crab$B90_CPUE ~ lag(crab$T38_AdultCPUE))
summary(B90AbunT38_lm4)
B90AbunT38_lm5 <- lm(crab$B90_CPUE ~ lag(crab$T38_ImmatureFemaleCPUE))
summary(B90AbunT38_lm5)
B90AbunT38_lm6 <- lm(crab$B90_CPUE ~ lag(crab$T38_ImmatureMaleCPUE))
summary(B90AbunT38_lm6)
B90AbunT38_lm7 <- lm(crab$B90_CPUE ~ lag(crab$T38_MatureFemaleCPUE))
summary(B90AbunT38_lm7)
B90AbunT38_lm8 <- lm(crab$B90_CPUE ~ lag(crab$T38_MatureMaleCPUE))
summary(B90AbunT38_lm8)
B90AbunT38_lm9 <- lm(crab$B90_CPUE ~ lag(crab$T38_SublegalCPUE))
summary(B90AbunT38_lm9)
B90AbunT38_lm10 <- lm(crab$B90_CPUE ~ lag(crab$T38_LegalCPUE))
summary(B90AbunT38_lm10)
### All Insignificant


#B90~T06
B90AbunT06_lm1 <- lm(crab$B90_CPUE ~ lag(crab$T06_CPUE))
summary(B90AbunT06_lm1)
### Insignificant


#B90~P88
B90AbunP88_lm1 <- lm(crab$B90_CPUE ~ lag(crab$P88_CPUE))
summary(B90AbunP88_lm1)
B90AbunP88_lm2 <- lm(crab$B90_CPUE ~ lag(crab$P88_SublegalCPUE))
summary(B90AbunP88_lm2)
B90AbunP88_lm3 <- lm(crab$B90_CPUE ~ lag(crab$P88_LegalCPUE))
summary(B90AbunP88_lm3)
### All Insignificant





# B90 Lead Lag ------------------------------------------------------------
#B90_CPUE~B90
B90AbunLead_lm1 <- lm(lead(crab$B90_CPUE) ~ lag(crab$B90_CPUE))
summary(B90AbunLead_lm1)
B90AbunLead_lm2 <- lm(lead(crab$B90_CPUE) ~ lag(crab$B90_JuvCPUE))
summary(B90AbunLead_lm2)
B90AbunLead_lm3 <- lm(lead(crab$B90_CPUE) ~ lag(crab$B90_SubadultCPUE))
summary(B90AbunLead_lm3)
B90AbunLead_lm4<- lm(lead(lead(crab$B90_CPUE)) ~ lag(crab$B90_AdultCPUE))
summary(B90AbunLead_lm4)
B90AbunLead_lm5 <- lm(lead(crab$B90_CPUE) ~ lag(crab$B90_ImmatureFemaleCPUE))
summary(B90AbunLead_lm5)
B90AbunLead_lm6 <- lm(lead(crab$B90_CPUE) ~ lag(crab$B90_ImmatureMaleCPUE))
summary(B90AbunLead_lm6)
B90AbunLead_lm7 <- lm(lead(crab$B90_CPUE) ~ lag(crab$B90_MatureFemaleCPUE))
summary(B90AbunLead_lm7)
B90AbunLead_lm8 <- lm(lead(crab$B90_CPUE) ~ lag(crab$B90_MatureMaleCPUE))
summary(B90AbunLead_lm8)
B90AbunLead_lm9 <- lm(lead(crab$B90_CPUE) ~ lag(crab$B90_SublegalCPUE))
summary(B90AbunLead_lm9)
B90AbunLead_lm10 <- lm(lead(crab$B90_CPUE) ~ lag(crab$B90_LegalCPUE))
summary(B90AbunLead_lm10)
### All Insignificant


#B90_CPUE~T38
#Insignificant
B90AbunT38Lead_lm1 <- lm(lead(crab$B90_CPUE) ~ lag(crab$T38_CPUE))
summary(B90AbunT38Lead_lm1)
B90AbunT38Lead_lm2 <- lm(lead(crab$B90_CPUE) ~ lag(crab$T38_JuvCPUE))
summary(B90AbunT38Lead_lm2)
B90AbunT38Lead_lm4 <- lm(lead(crab$B90_CPUE) ~ lag(crab$T38_AdultCPUE))
summary(B90AbunT38Lead_lm4)
B90AbunT38Lead_lm5 <- lm(lead(crab$B90_CPUE) ~ lag(crab$T38_ImmatureFemaleCPUE))
summary(B90AbunT38Lead_lm5)
B90AbunT38Lead_lm6 <- lm(lead(crab$B90_CPUE) ~ lag(crab$T38_ImmatureMaleCPUE))
summary(B90AbunT38Lead_lm6)
B90AbunT38Lead_lm7 <- lm(lead(crab$B90_CPUE) ~ lag(crab$T38_MatureFemaleCPUE))
summary(B90AbunT38Lead_lm7)
B90AbunT38Lead_lm8 <- lm(lead(crab$B90_CPUE) ~ lag(crab$T38_MatureMaleCPUE))
summary(B90AbunT38Lead_lm8)
B90AbunT38Lead_lm9 <- lm(lead(crab$B90_CPUE) ~ lag(crab$T38_SublegalCPUE))
summary(B90AbunT38Lead_lm9)
B90AbunT38Lead_lm10 <- lm(lead(crab$B90_CPUE) ~ lag(crab$T38_LegalCPUE))
summary(B90AbunT38Lead_lm10)

### Significant
B90AbunT38Lead_lm3 <- lm(lead(crab$B90_CPUE) ~ lag(crab$T38_SubadultCPUE))
summary(B90AbunT38Lead_lm3)

#B90~T06
B90AbunT06Lead_lm1 <- lm(lead(crab$B90_CPUE) ~ lag(crab$T06_CPUE))
summary(B90AbunT06Lead_lm1)
### Insignificant


#B90~P88
B90AbunP88Lead_lm1 <- lm(lead(crab$B90_CPUE) ~ lag(crab$P88_CPUE))
summary(B90AbunP88Lead_lm1)
B90AbunP88Lead_lm2 <- lm(lead(crab$B90_CPUE) ~ lag(crab$P88_SublegalCPUE))
summary(B90AbunP88Lead_lm2)
B90AbunP88Lead_lm3 <- lm(lead(crab$B90_CPUE) ~ lag(crab$P88_LegalCPUE))
summary(B90AbunP88Lead_lm3)
### Insignificant



# T38 ---------------------------------------------------------------------

#T38_CPUE~T38
T38Abun_lm1 <- lm(crab$T38_CPUE ~ lag(crab$T38_JuvCPUE))
summary(T38Abun_lm1) 
T38Abun_lm2 <- lm(crab$T38_CPUE ~ lag(crab$T38_SubadultCPUE))
summary(T38Abun_lm2) # 0.007774 - 0.1809
T38Abun_lm3 <- lm(crab$T38_CPUE ~ lag(crab$T38_AdultCPUE))
summary(T38Abun_lm3) # 0.0312 - 0.1225
T38Abun_lm4 <- lm(crab$T38_CPUE ~ lag(crab$T38_ImmatureFemaleCPUE))
summary(T38Abun_lm4) # 0.05007 - 0.1025
T38Abun_lm5 <- lm(crab$T38_CPUE ~ lag(crab$T38_ImmatureMaleCPUE))
summary(T38Abun_lm5) # 0.04854 - 0.1038
T38Abun_lm6 <- lm(crab$T38_CPUE ~ lag(crab$T38_MatureFemaleCPUE))
summary(T38Abun_lm6)
T38Abun_lm7 <- lm(crab$T38_CPUE ~ lag(crab$T38_MatureMaleCPUE))
summary(T38Abun_lm7) # 0.002197 - 0.2321
T38Abun_lm8 <- lm(crab$T38_CPUE ~ lag(crab$T38_SublegalCPUE))
summary(T38Abun_lm8) # 0.0252 - 0.1314
T38Abun_lm9 <- lm(crab$T38_CPUE ~ lag(crab$T38_LegalCPUE))
summary(T38Abun_lm9) # 0.0312 - 0.1225
T38Abun_lm10 <- lm(crab$T38_CPUE ~ lag(crab$T38_CPUE))
summary(T38Abun_lm10) # 0.01906 - 0.1434


#T38_CPUE~B90
T38AbunB90_lm1 <- lm(crab$T38_CPUE ~ lag(crab$B90_CPUE))
summary(T38AbunB90_lm1)
T38AbunB90_lm2 <- lm(crab$T38_CPUE ~ lag(crab$B90_JuvCPUE))
summary(T38AbunB90_lm2)
T38AbunB90_lm3 <- lm(crab$T38_CPUE ~ lag(crab$B90_SubadultCPUE))
summary(T38AbunB90_lm3)
T38AbunB90_lm4 <- lm(crab$T38_CPUE ~ lag(crab$B90_AdultCPUE))
summary(T38AbunB90_lm4)
T38AbunB90_lm5 <- lm(crab$T38_CPUE ~ lag(crab$B90_ImmatureFemaleCPUE))
summary(T38AbunB90_lm5)
T38AbunB90_lm6 <- lm(crab$T38_CPUE ~ lag(crab$B90_ImmatureMaleCPUE))
summary(T38AbunB90_lm6)
T38AbunB90_lm7 <- lm(crab$T38_CPUE ~ lag(crab$B90_MatureFemaleCPUE))
summary(T38AbunB90_lm7)
T38AbunB90_lm8 <- lm(crab$T38_CPUE ~ lag(crab$B90_MatureMaleCPUE))
summary(T38AbunB90_lm8)
T38AbunB90_lm9 <- lm(crab$T38_CPUE ~ lag(crab$B90_SublegalCPUE))
summary(T38AbunB90_lm9)
T38AbunB90_lm10 <- lm(crab$T38_CPUE ~ lag(crab$B90_LegalCPUE))
summary(T38AbunB90_lm10)
### All Insignificant

#T38~T06
T38AbunT06_lm1 <- lm(crab$T38_CPUE ~ lag(crab$T06_CPUE))
summary(T38AbunT06_lm1)
### All Insignificant

#T38~P88
T38AbunP88_lm1 <- lm(crab$T38_CPUE ~ lag(crab$P88_CPUE))
summary(T38AbunP88_lm1)
T38AbunP88_lm2 <- lm(crab$T38_CPUE ~ lag(crab$P88_SublegalCPUE))
summary(T38AbunP88_lm2)
T38AbunP88_lm3 <- lm(crab$T38_CPUE ~ lag(crab$P88_LegalCPUE))
summary(T38AbunP88_lm3)
###All Insignificant




# T38 Lead Lag ------------------------------------------------------------

#T38_CPUE~T38

### Insignificant
T38AbunLead_lm1 <- lm(lead(lead(crab$T38_CPUE)) ~ lag(crab$T38_JuvCPUE))
summary(T38AbunLead_lm1)
T38AbunLead_lm3 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_AdultCPUE))
summary(T38AbunLead_lm3) 
T38AbunLead_lm9 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_LegalCPUE))
summary(T38AbunLead_lm9) 

### Significant
T38AbunLead_lm2 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_SubadultCPUE))
summary(T38AbunLead_lm2) # 0.001926 - 0.2432
T38AbunLead_lm4 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_ImmatureFemaleCPUE))
summary(T38AbunLead_lm4) # 0.01038 - 0.1733
T38AbunLead_lm5 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_ImmatureMaleCPUE))
summary(T38AbunLead_lm5) # 0.004023 - 0.2131
T38AbunLead_lm6 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_MatureFemaleCPUE))
summary(T38AbunLead_lm6) # 0.019 - 0.1473
T38AbunLead_lm7 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_MatureMaleCPUE))
summary(T38AbunLead_lm7) # 0.03076 - 0.1265
T38AbunLead_lm8 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_SublegalCPUE))
summary(T38AbunLead_lm8) # 0.004317 - 0.2102
T38AbunLead_lm10 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T38_CPUE))
summary(T38AbunLead_lm10) # 0.004898 - 0.205


#T38_CPUE~B90
T38AbunB90Lead_lm1 <- lm(lead(crab$T38_CPUE) ~ lag(crab$B90_CPUE))
summary(T38AbunB90Lead_lm1)
T38AbunB90Lead_lm2 <- lm(lead(crab$T38_CPUE) ~ lag(crab$B90_JuvCPUE))
summary(T38AbunB90Lead_lm2)
T38AbunB90Lead_lm3 <- lm(lead(crab$T38_CPUE) ~ lag(crab$B90_SubadultCPUE))
summary(T38AbunB90Lead_lm3)
T38AbunB90Lead_lm4 <- lm(lead(crab$T38_CPUE) ~ lag(crab$B90_AdultCPUE))
summary(T38AbunB90Lead_lm4)
T38AbunB90Lead_lm5 <- lm(lead(crab$T38_CPUE) ~ lag(crab$B90_ImmatureFemaleCPUE))
summary(T38AbunB90Lead_lm5)
T38AbunB90Lead_lm6 <- lm(lead(crab$T38_CPUE) ~ lag(crab$B90_ImmatureMaleCPUE))
summary(T38AbunB90Lead_lm6)
T38AbunB90Lead_lm7 <- lm(lead(crab$T38_CPUE) ~ lag(crab$B90_MatureFemaleCPUE))
summary(T38AbunB90Lead_lm7)
T38AbunB90Lead_lm8 <- lm(lead(crab$T38_CPUE) ~ lag(crab$B90_MatureMaleCPUE))
summary(T38AbunB90Lead_lm8)
T38AbunB90Lead_lm9 <- lm(lead(crab$T38_CPUE) ~ lag(crab$B90_SublegalCPUE))
summary(T38AbunB90Lead_lm9)
T38AbunB90Lead_lm10 <- lm(lead(crab$T38_CPUE) ~ lag(crab$B90_LegalCPUE))
summary(T38AbunB90Lead_lm10)
### All Insignificant

#T38~T06
T38AbunT06Lead_lm1 <- lm(lead(crab$T38_CPUE) ~ lag(crab$T06_CPUE))
summary(T38AbunT06Lead_lm1)
### All Insignificant

#T38~P88
T38AbunP88Lead_lm1 <- lm(lead(crab$T38_CPUE) ~ lag(crab$P88_CPUE))
summary(T38AbunP88Lead_lm1)
T38AbunP88Lead_lm2 <- lm(lead(crab$T38_CPUE) ~ lag(crab$P88_SublegalCPUE))
summary(T38AbunP88Lead_lm2)
T38AbunP88Lead_lm3 <- lm(lead(crab$T38_CPUE) ~ lag(crab$P88_LegalCPUE))
summary(T38AbunP88Lead_lm3)
###All Insignificant

# T06 ---------------------------------------------------------------------

#T06~B90

#Insignificant
T06AbunB90_lm1 <- lm(crab$T06_CPUE ~ lag(crab$B90_CPUE))
summary(T06AbunB90_lm1)
T06AbunB90_lm2 <- lm(crab$T06_CPUE ~ lag(crab$B90_JuvCPUE))
summary(T06AbunB90_lm2)
T06AbunB90_lm3 <- lm(crab$T06_CPUE ~ lag(crab$B90_SubadultCPUE))
summary(T06AbunB90_lm3) 
T06AbunB90_lm4 <- lm(crab$T06_CPUE ~ lag(crab$B90_AdultCPUE))
summary(T06AbunB90_lm4) 
T06AbunB90_lm5 <- lm(crab$T06_CPUE ~ lag(crab$B90_ImmatureFemaleCPUE))
summary(T06AbunB90_lm5) 
T06AbunB90_lm6 <- lm(crab$T06_CPUE ~ lag(crab$B90_ImmatureMaleCPUE))
summary(T06AbunB90_lm6) 
T06AbunB90_lm8<- lm(crab$T06_CPUE ~ lag(crab$B90_MatureMaleCPUE))
summary(T06AbunB90_lm8) 
T06AbunB90_lm9 <- lm(crab$T06_CPUE ~ lag(crab$B90_SublegalCPUE))
summary(T06AbunB90_lm9) 
T06AbunB90_lm10 <- lm(crab$T06_CPUE ~ lag(crab$B90_LegalCPUE))
summary(T06AbunB90_lm10) 

#Significant
T06AbunB90_lm7 <- lm(crab$T06_CPUE ~ lag(crab$B90_MatureFemaleCPUE))
summary(T06AbunB90_lm7)


#T06~T38
T06AbunT38_lm1 <- lm(crab$T06_CPUE ~ lag(crab$T38_CPUE))
summary(T06AbunT38_lm1)
T06AbunT38_lm2 <- lm(crab$T06_CPUE ~ lag(crab$T38_JuvCPUE))
summary(T06AbunT38_lm2)
T06AbunT38_lm3 <- lm(crab$T06_CPUE ~ lag(crab$T38_SubadultCPUE))
summary(T06AbunT38_lm3) 
T06AbunT38_lm4 <- lm(crab$T06_CPUE ~ lag(crab$T38_AdultCPUE))
summary(T06AbunT38_lm4) 
T06AbunT38_lm5 <- lm(crab$T06_CPUE ~ lag(crab$T38_ImmatureFemaleCPUE))
summary(T06AbunT38_lm5) 
T06AbunT38_lm6 <- lm(crab$T06_CPUE ~ lag(crab$T38_ImmatureMaleCPUE))
summary(T06AbunT38_lm6) 
T06AbunT38_lm7 <- lm(crab$T06_CPUE ~ lag(crab$T38_MatureFemaleCPUE))
summary(T06AbunT38_lm7)
T06AbunT38_lm8<- lm(crab$T06_CPUE ~ lag(crab$T38_MatureMaleCPUE))
summary(T06AbunT38_lm8) 
T06AbunT38_lm9 <- lm(crab$T06_CPUE ~ lag(crab$T38_SublegalCPUE))
summary(T06AbunT38_lm9) 
T06AbunT38_lm10 <- lm(crab$T06_CPUE ~ lag(crab$T38_LegalCPUE))
summary(T06AbunT38_lm10) 
# All Insignificant


T06AbunT06_lm1 <- lm(crab$T06_CPUE ~ lag(crab$T06_CPUE))
summary(T06AbunT06_lm1)
# All Insignificant

# T06 Lead Lag ------------------------------------------------------------

#T06~B90
T06AbunB90_lm1 <- lm(lead(crab$T06_CPUE) ~ lag(crab$B90_CPUE))
summary(T06AbunB90_lm1)
T06AbunB90_lm2 <- lm(lead(crab$T06_CPUE) ~ lag(crab$B90_JuvCPUE))
summary(T06AbunB90_lm2)
T06AbunB90_lm3 <- lm(lead(crab$T06_CPUE) ~ lag(crab$B90_SubadultCPUE))
summary(T06AbunB90_lm3) 
T06AbunB90_lm4 <- lm(lead(crab$T06_CPUE) ~ lag(crab$B90_AdultCPUE))
summary(T06AbunB90_lm4) 
T06AbunB90_lm5 <- lm(lead(crab$T06_CPUE) ~ lag(crab$B90_ImmatureFemaleCPUE))
summary(T06AbunB90_lm5) 
T06AbunB90_lm6 <- lm(lead(crab$T06_CPUE) ~ lag(crab$B90_ImmatureMaleCPUE))
summary(T06AbunB90_lm6) 
T06AbunB90_lm7 <- lm(lead(crab$T06_CPUE) ~ lag(crab$B90_MatureFemaleCPUE))
summary(T06AbunB90_lm7)
T06AbunB90_lm8<- lm(lead(crab$T06_CPUE) ~ lag(crab$B90_MatureMaleCPUE))
summary(T06AbunB90_lm8) 
T06AbunB90_lm9 <- lm(lead(crab$T06_CPUE) ~ lag(crab$B90_SublegalCPUE))
summary(T06AbunB90_lm9) 
T06AbunB90_lm10 <- lm(lead(crab$T06_CPUE) ~ lag(crab$B90_LegalCPUE))
summary(T06AbunB90_lm10) 
# All Insignificant


#T06~T38
T06AbunT38_lm1 <- lm(lead(crab$T06_CPUE) ~ lag(crab$T38_CPUE))
summary(T06AbunT38_lm1)
T06AbunT38_lm2 <- lm(lead(crab$T06_CPUE) ~ lag(crab$T38_JuvCPUE))
summary(T06AbunT38_lm2)
T06AbunT38_lm3 <- lm(lead(crab$T06_CPUE) ~ lag(crab$T38_SubadultCPUE))
summary(T06AbunT38_lm3) 
T06AbunT38_lm4 <- lm(lead(crab$T06_CPUE) ~ lag(crab$T38_AdultCPUE))
summary(T06AbunT38_lm4) 
T06AbunT38_lm5 <- lm(lead(crab$T06_CPUE) ~ lag(crab$T38_ImmatureFemaleCPUE))
summary(T06AbunT38_lm5) 
T06AbunT38_lm6 <- lm(lead(crab$T06_CPUE) ~ lag(crab$T38_ImmatureMaleCPUE))
summary(T06AbunT38_lm6) 
T06AbunT38_lm7 <- lm(lead(crab$T06_CPUE) ~ lag(crab$T38_MatureFemaleCPUE))
summary(T06AbunT38_lm7)
T06AbunT38_lm8<- lm(lead(crab$T06_CPUE) ~ lag(crab$T38_MatureMaleCPUE))
summary(T06AbunT38_lm8) 
T06AbunT38_lm9 <- lm(lead(crab$T06_CPUE) ~ lag(crab$T38_SublegalCPUE))
summary(T06AbunT38_lm9) 
T06AbunT38_lm10 <- lm(lead(crab$T06_CPUE) ~ lag(crab$T38_LegalCPUE))
summary(T06AbunT38_lm10) 
# All Insignificant


T06AbunT06_lm1 <- lm(lead(crab$T06_CPUE) ~ lag(crab$T06_CPUE))
summary(T06AbunT06_lm1)
# All Insignificant

#T38~P88
T06AbunP88Lead_lm1 <- lm(lead(crab$T06_CPUE) ~ lag(crab$P88_CPUE))
summary(T06AbunP88Lead_lm1)
T06AbunP88Lead_lm2 <- lm(lead(crab$T06_CPUE) ~ lag(crab$P88_SublegalCPUE))
summary(T06AbunP88Lead_lm2)
T06AbunP88Lead_lm3 <- lm(lead(crab$T06_CPUE) ~ lag(crab$P88_LegalCPUE))
summary(T06AbunP88Lead_lm3)
###All Insignificant

# P88 ---------------------------------------------------------------------

#P88~B90
P88AbunB90_lm1 <- lm(crab$P88_CPUE ~ lag(crab$B90_CPUE))
summary(P88AbunB90_lm1)
P88AbunB90_lm2 <- lm(crab$P88_CPUE ~ lag(crab$B90_JuvCPUE))
summary(P88AbunB90_lm2)
P88AbunB90_lm3 <- lm(crab$P88_CPUE ~ lag(crab$B90_SubadultCPUE))
summary(P88AbunB90_lm3) 
P88AbunB90_lm4 <- lm(crab$P88_CPUE ~ lag(crab$B90_AdultCPUE))
summary(P88AbunB90_lm4) 
P88AbunB90_lm5 <- lm(crab$P88_CPUE ~ lag(crab$B90_ImmatureFemaleCPUE))
summary(P88AbunB90_lm5) 
P88AbunB90_lm6 <- lm(crab$P88_CPUE ~ lag(crab$B90_ImmatureMaleCPUE))
summary(P88AbunB90_lm6) 
P88AbunB90_lm7 <- lm(crab$P88_CPUE ~ lag(crab$B90_MatureFemaleCPUE))
summary(P88AbunB90_lm7)
P88AbunB90_lm8<- lm(crab$P88_CPUE ~ lag(crab$B90_MatureMaleCPUE))
summary(P88AbunB90_lm8) 
P88AbunB90_lm9 <- lm(crab$P88_CPUE ~ lag(crab$B90_SublegalCPUE))
summary(P88AbunB90_lm9) 
P88AbunB90_lm10 <- lm(crab$P88_CPUE ~ lag(crab$B90_LegalCPUE))
summary(P88AbunB90_lm10) 
# All Insignificant


#P88~T38
P88AbunT38_lm1 <- lm(crab$P88_CPUE ~ lag(crab$T38_CPUE))
summary(P88AbunT38_lm1)
P88AbunT38_lm2 <- lm(crab$P88_CPUE ~ lag(crab$T38_JuvCPUE))
summary(P88AbunT38_lm2)
P88AbunT38_lm3 <- lm(crab$P88_CPUE ~ lag(crab$T38_SubadultCPUE))
summary(P88AbunT38_lm3) 
P88AbunT38_lm4 <- lm(crab$P88_CPUE ~ lag(crab$T38_AdultCPUE))
summary(P88AbunT38_lm4) 
P88AbunT38_lm5 <- lm(crab$P88_CPUE ~ lag(crab$T38_ImmatureFemaleCPUE))
summary(P88AbunT38_lm5) 
P88AbunT38_lm6 <- lm(crab$P88_CPUE ~ lag(crab$T38_ImmatureMaleCPUE))
summary(P88AbunT38_lm6) 
P88AbunT38_lm7 <- lm(crab$P88_CPUE ~ lag(crab$T38_MatureFemaleCPUE))
summary(P88AbunT38_lm7)
P88AbunT38_lm8<- lm(crab$P88_CPUE ~ lag(crab$T38_MatureMaleCPUE))
summary(P88AbunT38_lm8) 
P88AbunT38_lm9 <- lm(crab$P88_CPUE ~ lag(crab$T38_SublegalCPUE))
summary(P88AbunT38_lm9) 
P88AbunT38_lm10 <- lm(crab$P88_CPUE ~ lag(crab$T38_LegalCPUE))
summary(P88AbunT38_lm10) 
# All Insignificant


P88AbunP88_lm1 <- lm(crab$P88_CPUE ~ lag(crab$P88_CPUE))
summary(P88AbunP88_lm1)
# All Insignificant


# P88 Lead Lag ------------------------------------------------------------

#P88~B90
P88AbunB90_lm1 <- lm(lead(crab$P88_CPUE) ~ lag(crab$B90_CPUE))
summary(P88AbunB90_lm1)
P88AbunB90_lm2 <- lm(lead(crab$P88_CPUE) ~ lag(crab$B90_JuvCPUE))
summary(P88AbunB90_lm2)
P88AbunB90_lm3 <- lm(lead(crab$P88_CPUE) ~ lag(crab$B90_SubadultCPUE))
summary(P88AbunB90_lm3) 
P88AbunB90_lm4 <- lm(lead(crab$P88_CPUE) ~ lag(crab$B90_AdultCPUE))
summary(P88AbunB90_lm4) 
P88AbunB90_lm5 <- lm(lead(crab$P88_CPUE) ~ lag(crab$B90_ImmatureFemaleCPUE))
summary(P88AbunB90_lm5) 
P88AbunB90_lm6 <- lm(lead(crab$P88_CPUE) ~ lag(crab$B90_ImmatureMaleCPUE))
summary(P88AbunB90_lm6) 
P88AbunB90_lm7 <- lm(lead(crab$P88_CPUE) ~ lag(crab$B90_MatureFemaleCPUE))
summary(P88AbunB90_lm7)
P88AbunB90_lm8<- lm(lead(crab$P88_CPUE) ~ lag(crab$B90_MatureMaleCPUE))
summary(P88AbunB90_lm8) 
P88AbunB90_lm9 <- lm(lead(crab$P88_CPUE) ~ lag(crab$B90_SublegalCPUE))
summary(P88AbunB90_lm9) 
P88AbunB90_lm10 <- lm(lead(crab$P88_CPUE) ~ lag(crab$B90_LegalCPUE))
summary(P88AbunB90_lm10) 
# All Insignificant


#P88~T38
P88AbunT38_lm1 <- lm(lead(crab$P88_CPUE) ~ lag(crab$T38_CPUE))
summary(P88AbunT38_lm1)
P88AbunT38_lm2 <- lm(lead(crab$P88_CPUE) ~ lag(crab$T38_JuvCPUE))
summary(P88AbunT38_lm2)
P88AbunT38_lm3 <- lm(lead(crab$P88_CPUE) ~ lag(crab$T38_SubadultCPUE))
summary(P88AbunT38_lm3) 
P88AbunT38_lm4 <- lm(lead(crab$P88_CPUE) ~ lag(crab$T38_AdultCPUE))
summary(P88AbunT38_lm4) 
P88AbunT38_lm5 <- lm(lead(crab$P88_CPUE) ~ lag(crab$T38_ImmatureFemaleCPUE))
summary(P88AbunT38_lm5) 
P88AbunT38_lm6 <- lm(lead(crab$P88_CPUE) ~ lag(crab$T38_ImmatureMaleCPUE))
summary(P88AbunT38_lm6) 
P88AbunT38_lm7 <- lm(lead(crab$P88_CPUE) ~ lag(crab$T38_MatureFemaleCPUE))
summary(P88AbunT38_lm7)
P88AbunT38_lm8<- lm(lead(crab$P88_CPUE) ~ lag(crab$T38_MatureMaleCPUE))
summary(P88AbunT38_lm8) 
P88AbunT38_lm9 <- lm(lead(crab$P88_CPUE) ~ lag(crab$T38_SublegalCPUE))
summary(P88AbunT38_lm9) 
P88AbunT38_lm10 <- lm(lead(crab$P88_CPUE) ~ lag(crab$T38_LegalCPUE))
summary(P88AbunT38_lm10) 
# All Insignificant


P88AbunP88_lm1 <- lm(lead(crab$P88_CPUE) ~ lag(crab$P88_CPUE))
summary(P88AbunP88_lm1)
# All Insignificant






