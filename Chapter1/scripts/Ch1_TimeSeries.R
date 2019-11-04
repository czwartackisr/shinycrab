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
crabdata <- read.csv("./Chapter1/data/ClassSizeAbunMANUAL.csv", stringsAsFactors = FALSE) 
crabdata = distinct(crabdata, Coll, .keep_all = TRUE) %>%
  mutate(StartTime = as.character(StartTime)) %>%
  mutate(StartTime = mdy(StartTime))
#Source = "R.Project$crab - size - classSize - ClassSizeAbunMANUAL2" - file was manually lubridated in Excel
#This data is cleandata from the crab project with abundance information for both size and class of crab

B90T38 = crabdata %>% #Formerly crab1
  select(1:4, 7:16) %>%
  filter(ProjID %in% c("B90", "T38")) %>%
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

T06 = crabdata %>% #Formerly crab1
  select(1:4, 7:16) %>%
  filter(ProjID %in% "T06") %>%
  rename(Total = CPUE) %>%
  select(1:5) %>%
  gather("Lifestage", "CPUE", 5) %>%
  mutate(Year = year(StartTime)) %>%
  mutate(Month = month(StartTime)) %>%
  select(1,2,4,3,7,8,5,6) 


P88 = crabdata %>% #Formerly crab1
  select(1:4, 7:16) %>%
  filter(ProjID %in% "P88") %>%
  rename(Total = CPUE,
         Sublegal = SublegalCPUE,
         Legal = LegalCPUE) %>%
  distinct(Coll, .keep_all = TRUE) %>%
  select(1:5, 13, 14) %>%
  gather("Lifestage", "CPUE", 5:7) %>%
  mutate(Year = year(StartTime)) %>%
  mutate(Month = month(StartTime)) %>%
  select(1,2,4,3,7,8,5,6) 

p88Crab = crabdata %>% #Formerly crab1
  select(1:4, 7:16) %>%
  filter(ProjID %in% "P88") 


SCECAP = crabdata %>% #Formerly crab1
  select(1:4, 7:16) %>%
  filter(ProjID %in% c("E98", "E99")) %>%
  rename(Total = CPUE,
         Sublegal = SublegalCPUE,
         Legal = SublegalCPUE,
         Juvenile = JuvCPUE,
         Subadult = SubadultCPUE,
         Adult = AdultCPUE) %>%
  select(1:8, 13, 14) %>%
  gather("Lifestage", "CPUE", 5:10) %>%
  mutate(Year = year(StartTime)) %>%
  mutate(Month = month(StartTime)) %>%
  select(1,2,4,3,7,8,5,6) 
  
crab1 = rbind(B90T38, P88) %>%
  rbind(., T06) %>%
  rbind(., SCECAP)



#Source - Eric Hiltz OFM Fisheries Statistics
#Wrangled in Excel due to confidentiality
depend <- read.csv("./Chapter1/data/hardCrabsByArea_04252019_CH.csv")

###############################################################################DO UP THERE

#Joining Independent and Dependent 
crab3 = full_join(crab1, depend,
                  by = c("ProjID", "StationCode", "Year", 
                         "Month", "Lifestage", "CPUE")) %>%
  mutate(ProjID = as.factor(ProjID)) %>%
  mutate(Coll = as.factor(Coll)) %>%
  mutate(StationCode = as.factor(StationCode)) %>%
  mutate(StartTime = ymd(StartTime)) %>%
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
                       'T06' = "Trammel Net",
                       'P88' = "Ashley Potting",
                       'E98' = "SCECAP Creek Trawl",
                       'E99' = "SCECAP Harbor Trawl"))



# Total Abundance Time Series ---------------------------------------------

TotalCrab = crab %>%
  filter(Lifestage == "Total")
TotalCrab$ProjID <- factor(TotalCrab$ProjID, levels = c("Harbor Trawl",
                                                        "SCECAP Harbor Trawl",
                                                        "Creek Trawl",
                                                        "SCECAP Creek Trawl",
                                                        "Trammel Net",
                                                        "Ashley Potting"))


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
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)
#B90TotalCPUE

T38TotalCPUE = ggplot(aes(Year, CPUE), data = subset(TotalCrab,
                                                     ProjID == "Creek Trawl")) +
  ggtitle("Creek Trawl") +
  stat_summary(fun.data = "mean_se", size = .7) +
  geom_smooth(method = 'loess', se=FALSE, color="gray20") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)
#T38TotalCPUE

T06TotalCPUE = ggplot(aes(Year, CPUE), data = subset(TotalCrab,
                                                     ProjID == "Trammel Net")) +
  ggtitle("Trammel Net") +
  stat_summary(fun.data = "mean_se", size = .7) +
  geom_smooth(method = 'loess', se=FALSE, color="gray20") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~set^{-1})) +
  expand_limits(x=c(1980,2020))
#T06TotalCPUE

E98TotalCPUE = ggplot(aes(Year, log(CPUE)), data = subset(TotalCrab,
                                                     ProjID == "SCECAP Creek Trawl")) +
  ggtitle("SCECAP Tidal Creek Trawl") +
  stat_summary(fun.data = "mean_se", size = .7) +
  geom_smooth(method = 'loess', se=FALSE, color="gray20") +
  geom_hline(aes(yintercept=(log(mean(Means)))), linetype="dashed") +
  labs(y=expression((log)~Blue~crab~per~tow^{-1})) +
  expand_limits(x=c(1980,2020))


E99TotalCPUE = ggplot(aes(Year, CPUE), data = subset(TotalCrab,
                                                          ProjID == "SCECAP Harbor Trawl")) +
  ggtitle("SCECAP Open Water Trawl") +
  stat_summary(fun.data = "mean_se", size = .7) +
  geom_smooth(method = 'loess', se=FALSE, color="gray20") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=c(1980,2020))

P88TotalCPUE = ggplot(aes(Year, CPUE), data = subset(TotalCrab,
                                                     ProjID == "Ashley Potting" &
                                                       Year > 2002)) +
  ggtitle("Ashley River Potting Survey (bi-monthly)") +
  stat_summary(fun.data = "mean_se", size = .7) +
  geom_smooth(method = 'loess', se=FALSE, color="gray20", na.rm = TRUE) +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~soak^{-1})) +
  expand_limits(x=c(1980,2020))

P88fall = TotalCrab %>%
  filter(ProjID == "Ashley Potting") %>%
  filter(Month %in% c("10", "11"))
P88FallTotalCPUE = ggplot(aes(Year, CPUE), data = subset(TotalCrab,
                                                     ProjID == "Ashley Potting")) +
  ggtitle("Ashley River Potting Survey (fall)") +
  stat_summary(fun.data = "mean_se", size = .7) +
  geom_smooth(method = 'loess', se=FALSE, color="gray20", na.rm = TRUE) +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~soak^{-1})) +
  expand_limits(x=c(1980,2020))
#grid.arrange(B90TotalCPUE, T38TotalCPUE, T06TotalCPUE, ncol = 1,
             #top = "All Surveys Mean Annual Abundance (Total Catch)") 

plot_grid(B90TotalCPUE, E99TotalCPUE, T38TotalCPUE, E98TotalCPUE, P88FallTotalCPUE, T06TotalCPUE, 
          ncol = 1, align = 'hv')
plot_grid(B90TotalCPUE, T38TotalCPUE, P88FallTotalCPUE, ncol = 1, align = 'hv')
plot_grid(E99TotalCPUE, E98TotalCPUE, P88FallTotalCPUE, T06TotalCPUE, ncol = 1, align = 'hv')


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
