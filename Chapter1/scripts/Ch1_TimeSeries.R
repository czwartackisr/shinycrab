library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)
library(Hmisc)
library(nlme)
library(cowplot)
library(gridExtra)

theme_set(theme_classic(base_size = 9)+
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_rect(colour = "black", size=0.5))+
            theme(text=element_text(size=9,  family="serif", colour = "black"))+
            theme(axis.text = element_text(colour = "black"))+
            theme(axis.ticks = element_line(colour = "black"))
)
update_geom_defaults("point",   list(colour = "black"))
update_geom_defaults("line",   list(colour = "black"))
theme_update(plot.title = element_text(hjust = 0.5))





# Data Read-In ------------------------------------------------------------
crabdata <- read.csv("./Chapter1/data/ClassSizeAbunMANUAL2.csv", stringsAsFactors = FALSE) 
crabdata = distinct(crabdata, Coll, .keep_all = TRUE)
#Source = "R.Project$crab - size - classSize - cleandata"
#This data is cleandata from the crab project with abundance information for both size and class of crab

crab1 = crabdata %>%
  select(1:4, 7:16) %>%
  rename(Total = CPUE,
         Juvenile = JuvCPUE,
         Subadult = SubadultCPUE,
         Adult = AdultCPUE,
         "Immature Female" = ImmatureFemaleCPUE,
         "Mature Female" = MatureFemaleCPUE,
         "Immature Male" = ImmatureMaleCPUE,
         "Mature Male" = MatureMaleCPUE,
         Sublegal = SublegalCPUE,
         Legal = SublegalCPUE) %>%
  gather("Lifestage", "CPUE", 5:14) %>%
  mutate(Year = year(StartTime)) %>%
  mutate(Month = month(StartTime)) %>%
  select(1,2,4,3,7,8,5,6)
#############################################################START HERE
#Cull Lifestage info for Vars with NAs in certain lifestages **hint: Look below***

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
                  by = c("ProjID", "StationCode", "Year", 
                         "Month", "Lifestage", "CPUE")) %>%
  mutate(ProjID = as.factor(ProjID)) %>%
  mutate(Coll = as.factor(Coll)) %>%
  mutate(StationCode = as.factor(StationCode)) %>%
  mutate(StartTime = ymd_hms(StartTime)) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(Month = as.factor(Month)) %>%
  mutate(Lifestage = as.factor(Lifestage)) %>%
  group_by(ProjID, StationCode, Lifestage, Month) %>%
  mutate(Means = mean(CPUE, na.rm=TRUE),
         SDs = sd(CPUE, na.rm=TRUE),
         Zs = (CPUE-Means)/ifelse(SDs>0,SDs, 0.00000001)) %>%
  ungroup() 

crab = crab3 %>%
  mutate(ProjID = recode(ProjID, 'B90' = "Harbor Trawl",
                       'T38' = "Creek Trawl",
                       'T06' = "Trammel Net"))



# Total Abundance Time Series ---------------------------------------------

TotalCrab = crab %>%
  filter(Lifestage == "Total")
TotalCrab$ProjID <- factor(TotalCrab$ProjID, levels = c("Harbor Trawl",
                                                        "Creek Trawl",
                                                        "Trammel Net"))

#TotalCPUE = ggplot(aes(Year, CPUE), data = TotalCrab) +
  #ggtitle("All Surveys Mean Annual Abundance (Total Catch)") +
  #facet_wrap(~ProjID, scales = 'free') +
  #stat_summary(fun.data = "mean_se", size = .7) +
  #geom_smooth(method = 'loess', se=FALSE, color="gray20") +
  #geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  #labs(y=expression(Blue~crab~per~tow~or~set^{-1})) +
  #expand_limits(x=2022)
#TotalCPUE

B90TotalCPUE = ggplot(aes(Year, CPUE), data = subset(TotalCrab,
                                                     ProjID == "Harbor Trawl")) +
  ggtitle("Harbor Trawl") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  geom_smooth(method = 'loess', se=FALSE, color="gray20") +
  geom_hline(aes(yintercept=mean(CPUE)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow~or~set^{-1})) +
  expand_limits(x=2020)
#B90TotalCPUE

T38TotalCPUE = ggplot(aes(Year, CPUE), data = subset(TotalCrab,
                                                     ProjID == "Creek Trawl")) +
  ggtitle("Creek Trawl") +
  stat_summary(fun.data = "mean_se", size = .7) +
  geom_smooth(method = 'loess', se=FALSE, color="gray20") +
  geom_hline(aes(yintercept=mean(CPUE)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow~or~set^{-1})) +
  expand_limits(x=2020)
#T38TotalCPUE

T06TotalCPUE = ggplot(aes(Year, CPUE), data = subset(TotalCrab,
                                                     ProjID == "Trammel Net")) +
  ggtitle("Trammel Net") +
  stat_summary(fun.data = "mean_se", size = .7) +
  geom_smooth(method = 'loess', se=FALSE, color="gray20") +
  geom_hline(aes(yintercept=mean(CPUE)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow~or~set^{-1})) +
  expand_limits(x=c(2005,2020))
#T06TotalCPUE


grid.arrange(B90TotalCPUE, T38TotalCPUE, T06TotalCPUE, nrow = 1,
             top = "All Surveys Mean Annual Abundance (Total Catch)") 




# Harbor Trawl Size/Class Time Series -------------------------------------

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

B90JuvCPUEs = ggplot(aes(Year, CPUE), data = subset(B90crab, 
                                                    Lifestage == "Juvenile")) +
  ggtitle("Juveniles (<61mm)") +
 stat_summary(fun.data = "mean_se", size = .7) +
  geom_smooth(method = 'loess', se=FALSE, color="gray20") +
  geom_hline(aes(yintercept=mean(CPUE)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow~or~set^{-1})) +
  expand_limits(x=2020)
B90JuvCPUEs

B90sub = subset(B90crab, 
                Lifestage == "Subadult")
B90SubCPUEs = ggplot(aes(Year, CPUE), data = subset(B90crab, 
                                                    Lifestage == "Subadult")) +
  ggtitle("Subadults (>60mm & <127mm)") +
  stat_summary(fun.data = "mean_se", size = .7) +
  geom_smooth(method = 'loess', se=FALSE, color="gray20") +
  geom_hline(aes(yintercept=mean(CPUE)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow~or~set^{-1})) +
  expand_limits(x=2020)
B90SubCPUEs

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
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow~or~set^{-1})) +
  expand_limits(x=2022)
T38CPUEs

#####FIX NAS in SIZE CLASS DATA 

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
