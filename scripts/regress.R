library(tidyverse)
library(caret)
library(leaps)
library(PerformanceAnalytics)

crab = read.csv("./shinycrab/data.csv")

Abundance = select(crab, 28:33, 35:38, 40:43, 46:51, 54, 57, 60, 61:72)
Abun_Sal = select(crab, 28:33, 11:26)
Salinity = select(crab, 11:26, 34, 39, 44, 52, 55, 58)
Temperature = select(crab, 9, 10, 27, 45, 53, 56, 59)    
Precipitation = select(crab, 2:8)
Climate = select(crab, 73:96)

chart.Correlation(Abun_Sal, histogram = FALSE, method = "kendall")
