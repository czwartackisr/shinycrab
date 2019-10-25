library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)
library(Hmisc)
library(nlme)
library(cowplot)
library(knitr)
library(readxl)
library(rmarkdown)

theme_set(theme_classic(base_size = 12)+
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_rect(colour = "black", size=0.5))+
            theme(text=element_text(size=12,  family="serif", colour = "black"))+
            theme(axis.text = element_text(colour = "black"))+
            theme(axis.ticks = element_line(colour = "black"))
)
update_geom_defaults("point",   list(colour = "black"))
update_geom_defaults("line",   list(colour = "black"))
theme_update(plot.title = element_text(hjust = 0.5))





# Data Read-In ------------------------------------------------------------
crab <- read.csv("./Chapter1/data/SizeClassAbun.csv", stringsAsFactors = FALSE) 
#Source = "R.Project$crab - size - classSize - cleandata"
#This data is cleandata from the crab project with abundance information for both size and class of crab

crab1 = crab %>%
  select(1:3, 5, 6, 12, 13, 22:24) %>%
  filter(ProjID == c("B90", "T38")) %>%
  rename(Total = CPUE,
         Juvenile = JuvCPUE,
         Subadult = SubadultCPUE,
         Adult = AdultCPUE,
         "Immature Female" = ImmatureFemaleCPUE,
         "Mature Female" = MatureFemaleCPUE) %>%
  gather("Lifestage", "CPUE", 5:10) %>%
  mutate(Year = year(StartTime)) %>%
  mutate(Month = month(StartTime)) %>%
  select(1,2,4,3,7,8,5,6)

#Source = "R.Project$crab - flatfile - cleandata - crab.csv"
#Wrangled in Excel due to time constraints - Chas only sites, date/time variable, Lifestage added
T06_1 = read.csv("./Chapter1/data/T06.csv", stringsAsFactors = FALSE) 
T06_1 = T06_1 %>%
  mutate(Year = year(StartTime)) %>%
  mutate(Month = month(StartTime)) %>%
  select(1,2,3,4,7,8,5,6)

#combining T06 with B90 and T38
crab2 = rbind(crab1, T06_1)

#Source - Eric Hiltz OFM Fisheries Statistics
#Wrangled in Excel due to confidentiality
depend <- read.csv("./Chapter1/data/hardCrabsByArea_04252019_CH.csv")

#Joining Independent and Dependent 
crab3 = full_join(crab2, depend,
                  by = c("ProjID", "StationCode", "Year", "Month", "Lifestage", "CPUE")) %>%
  mutate(ProjID = as.factor(ProjID)) %>%
  mutate(Coll = as.factor(Coll)) %>%
  mutate(StationCode = as.factor(StationCode)) %>%
  mutate(StartTime = ymd_hms(StartTime)) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(Month = as.factor(Month)) %>%
  mutate(Lifestage = as.factor(Lifestage)) %>%
  group_by(ProjID, StationCode, Month) %>%
  mutate(Means = mean(CPUE, na.rm=TRUE),
         SDs = sd(CPUE, na.rm=TRUE),
         Zs = (CPUE-Means)/ifelse(SDs>0,SDs, 0.00000001)) %>%
  ungroup() 

crab = crab3 %>%
  mutate(ProjID = recode(ProjID, 'B90' = "Harbor Trawl",
                       'T38' = "Creek Trawl",
                       'T06' = "Trammel Net"))



# Time Series Plots -------------------------------------------------------
TotalCrab = crab %>%
  filter(Lifestage == "Total")
TotalCrab$ProjID <- factor(TotalCrab$ProjID, levels = c("Harbor Trawl",
                                                        "Creek Trawl",
                                                        "Trammel Net"))

TotalCPUE = ggplot(aes(Year, CPUE), data = TotalCrab) +
  ggtitle("All Surveys Mean Annual Abundance (Total Catch)") +
  facet_wrap(~ProjID, scales = 'free') +
  stat_summary(fun.data = "mean_se", size = .7) +
  geom_smooth(method = 'loess', se=FALSE, color="gray20") +
  geom_hline(aes(yintercept=mean(CPUE)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow~or~set^{-1})) +
  expand_limits(x=2022)
TotalCPUE



B90crab = crab %>%
  filter(ProjID == "Harbor Trawl") %>%
  filter(Lifestage == c("Juvenile", "Subadult", "Adult",
                          "Immature Female", "Mature Female"))
B90crab$Lifestage <- factor(B90crab$Lifestage, levels = c("Juvenile", "Subadult", "Adult",
                                                          "Immature Female", "Mature Female"))
B90CPUEs = ggplot(aes(Year, CPUE), data = B90crab) +
  ggtitle("Harbor Trawl Mean Annual Abundance by Life Stage") +
  facet_wrap(~Lifestage, scales = 'free') +
  stat_summary(fun.data = "mean_se", size = .7) +
  geom_smooth(method = 'loess', se=FALSE, color="gray20") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow~or~set^{-1})) +
  expand_limits(x=2022)
B90CPUEs



T38crab = crab %>%
  filter(ProjID == "Creek Trawl") %>%
  filter(Lifestage == c("Juvenile", "Subadult", "Adult",
                        "Immature Female", "Mature Female"))
T38crab$Lifestage <- factor(T38crab$Lifestage, levels = c("Juvenile", "Subadult", "Adult",
                                                          "Immature Female", "Mature Female"))
T38CPUEs = ggplot(aes(Year, CPUE), data = T38crab) +
  ggtitle("Creek Trawl Mean Annual Abundance by Life Stage") +
  facet_wrap(~Lifestage, scales = 'free') +
  stat_summary(fun.data = "mean_se", size = .7) +
  geom_smooth(method = 'loess', se=FALSE, color="gray20") +
  geom_hline(aes(yintercept=mean(CPUE)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow~or~set^{-1})) +
  expand_limits(x=2022)
T38CPUEs



LandingsCrab = crab %>%
  filter(ProjID == c("Landings", "LandingsCPUE")) %>%
  group_by(ProjID) %>%
  mutate(Means = mean(CPUE, na.rm=TRUE),
         SDs = sd(CPUE, na.rm=TRUE),
         Zs = (CPUE-Means)/ifelse(SDs>0,SDs, 0.00000001)) %>%
  ungroup() 

LandingsCPUEs = ggplot(aes(Year, CPUE), data = LandingsCrab) +
  ggtitle("Mean Annual Landings\nCharleston Harbor (Combined Watersheds)") +
  #geom_hline(aes(yintercept=mean(CPUE)), linetype="dashed") +
  facet_wrap(~ProjID, scales = 'free') +
  stat_summary(fun.data = "mean_se", size = .7) +
  geom_smooth(method = 'loess', se=FALSE, color="gray20") +
  labs(y=expression(Blue~crab~per~tow~or~set^{-1})) +
  expand_limits(x=2022)
LandingsCPUEs
