library(dplyr)
library(tidyr)
library(ggplot2)

scatterCrab <- read.csv("./Chapter1/data/Ch1_Scatter.csv", stringsAsFactors = FALSE)


B90Sub = ggplot(scatterCrab, aes(B90_SubadultLAG,MeanLandingsCPUE)) +
  ggtitle("Charleston Harbor Landings CPUE by\nLagged (1 yr.) Harbor Trawl Subadult Abundance") +
  geom_point() +
  stat_summary(fun.data = mean_cl_normal) +
  geom_smooth(method = 'lm', formula = y~x, se=FALSE, color='grey20') +
  labs(x="Harbor Trawl Subadults (1 yr. lag)", y="Landings CPUE") 
B90Sub


B90Imm = ggplot(scatterCrab, aes(B90_ImmatureFemaleCPUE,MeanLandingsCPUE)) +
  ggtitle("Charleston Harbor Landings CPUE by\nHarbor Trawl Immature Female Abundance") +
  geom_point() +
  stat_summary(fun.data = mean_cl_normal) +
  geom_smooth(method = 'lm', formula = y~x, se=FALSE, color='grey20') +
  labs(x="Harbor Trawl Immature Females", y="Landings CPUE")
B90Imm

B90Ma = ggplot(scatterCrab, aes(B90_MatureFemaleCPUE,MeanLandingsCPUE)) +
  ggtitle("Charleston Harbor Landings CPUE by\nHarbor Trawl Mature Female Abundance") +
  geom_point() +
  stat_summary(fun.data = mean_cl_normal) +
  geom_smooth(method = 'lm', formula = y~x, se=FALSE, color='grey20') +
  labs(x="Harbor Trawl Mature Females", y="Landings CPUE") +
  coord_cartesian(xlim = c(0.0, 5.0))
B90Ma

B90Tot = ggplot(scatterCrab, aes(B90_CPUE,MeanLandingsCPUE)) +
  ggtitle("Charleston Harbor Landings CPUE by\nHarbor Trawl Abundance") +
  geom_point() +
  stat_summary(fun.data = mean_cl_normal) +
  geom_smooth(method = 'lm', formula = y~x, se=FALSE, color='grey20') +
  labs(x="Harbor Trawl Total Abundance", y="Landings CPUE") +
  xlim(0,21)
B90Tot

T38Sub = ggplot(scatterCrab, aes(T38_SubadultLAG,MeanLandingsCPUE)) +
  ggtitle("Charleston Harbor Landings CPUE by\nLagged (1 yr.) Creek Trawl Subadult Abundance") +
  geom_point() +
  stat_summary(fun.data = mean_cl_normal) +
  geom_smooth(method = 'lm', formula = y~x, se=FALSE, color='grey20') +
  labs(x="Creek Trawl Subadults (1 yr. lag)", y="Landings CPUE") +
  xlim(1,4.05)
T38Sub

T38Tot = ggplot(scatterCrab, aes(T38_CPUE,MeanLandingsCPUE)) +
  ggtitle("Charleston Harbor Landings CPUE by\nCreek Trawl Abundance") +
  geom_point() +
  stat_summary(fun.data = mean_cl_normal) +
  geom_smooth(method = 'lm', formula = y~x, se=FALSE, color='grey20') +
  labs(x="Creek Trawl Total Abundance", y="Landings CPUE") +
  xlim(0,10)
T38Tot

B90T38Sub = ggplot(scatterCrab, aes(T38B90_SubadultLAG,MeanLandingsCPUE)) +
  ggtitle("Charleston Harbor Landings CPUE by\nLagged (1 yr.) Creek Trawl + Harbor Trawl Subadult Abundance") +
  geom_point() +
  stat_summary(fun.data = mean_cl_normal) +
  geom_smooth(method = 'lm', formula = y~x, se=FALSE, color='grey20') +
  labs(x="Lagged Creek + Harbor Trawl Subadult Abundance", y="Landings CPUE") +
  xlim(0,31)
B90T38Sub

T06Tot = ggplot(scatterCrab, aes(T06_CPUE,MeanLandingsCPUE)) +
  ggtitle("Charleston Harbor Landings CPUE by\nTrammel Net Abundance") +
  geom_point() +
  stat_summary(fun.data = mean_cl_normal) +
  geom_smooth(method = 'lm', formula = y~x, se=FALSE, color='grey20') +
  labs(x="Trammel Net Total Abundance", y="Landings CPUE") 
T06Tot




