library(dplyr)
library(tidyr)
library(ggplot2)


# Read in data ------------------------------------------------------------

crab <- read.csv("./Chapter1/data/CH1data.csv", stringsAsFactors = FALSE)
#crab is the unprepared for dredge original data





# Data Reshaping ----------------------------------------------------------

crab = crab %>%
  filter(Year > 1979) %>%
  select(1:14, 17) 

B90 = crab %>%
  select(1:7) %>%
  rename(Total = B90_CPUE,
         Juvenile = B90_JuvCPUE,
         Subadult = B90_SubadultCPUE,
         Adult = B90_AdultCPUE,
         "Immature Female" = B90_ImmatureFemaleCPUE,
         "Mature Female" = B90_MatureFemaleCPUE) %>%
  gather(Lifestage, CPUE, 2:7) %>%
  mutate(ProjID = "Harbor Trawl") %>%
  select(1, 4, 2, 3)

T38 = select(crab, 1, 8:13)%>%
  rename(Total = T38_CPUE,
         Juvenile = T38_JuvCPUE,
         Subadult = T38_SubadultCPUE,
         Adult = T38_AdultCPUE,
         "Immature Female" = T38_ImmatureFemaleCPUE,
         "Mature Female" = T38_MatureFemaleCPUE) %>%
  gather(Lifestage, CPUE, 2:7) %>%
  mutate(ProjID = "Harbor Trawl") %>%
  select(1, 4, 2, 3)

T06 = select(crab, 1, 14) %>%
  filter(Year > 2005) %>%
  rename(Total = T06_CPUE) %>%
  gather(Lifestage, CPUE, 2) %>%
  mutate(ProjID = "Total") %>%
  select(1, 4, 2, 3)


LandingsCPUE = select(crab, 1, 15) %>%
  filter(Year > 2003) %>%
#######STOPPED HERE#######  
  

B90 %>%
  rename(Total = B90_CPUE,
         Juvenile = B90_JuvCPUE,
         Subadult = B90_SubadultCPUE,
         Adult = B90_AdultCPUE,
         "Immature Female" = B90_ImmatureFemaleCPUE,
         "Mature Female" = B90_MatureFemaleCPUE)

crab2 = cbind(B90, T38, T06, )


Fig_plot<-plot_grid(T38_YSI, T38_Raw, T38_CSI, nrow = 1)
ggsave("Fig.pdf", width = 7, height = 3, dpi=600)