library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)
library(Hmisc)
library(nlme)
library(cowplot)
library(gridExtra)
library(ggpmisc)

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




# Data Read-In and Wrangling ---------------------------------------------
crabdata <- read.csv("./Chapter1/report/ClassSizeAbunMANUAL.csv", stringsAsFactors = FALSE) 
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
         Legal = LegalCPUE) %>%
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
         Legal = LegalCPUE,
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
depend <- read.csv("./Chapter1/report/hardCrabsByArea_04252019_CH.csv")


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

#### IS.FINITE NEEDS TO GO HERE FOR mATURE fEMALE STAT_SUMMS AND GEOM_HLINES

crab = crab3 %>%
  mutate(ProjID = recode(ProjID, 'B90' = "Harbor Trawl",
                         'T38' = "Creek Trawl",
                         'T06' = "Trammel Net",
                         'P88' = "Ashley Potting",
                         'E98' = "SCECAP Creek Trawl",
                         'E99' = "SCECAP Harbor Trawl"))





# Landings ----------------------------------------------------------------

LandingsCPUEFormTotal = subset(crab,
                           ProjID == "LandingsCPUE")
formula1 <- lm(LandingsCPUEFormTotal$Year ~ LandingsCPUEFormTotal$CPUE)
LandingsCrab = crab %>%
  filter(ProjID == c("Landings", "LandingsCPUE")) %>%
  group_by(ProjID) %>%
  mutate(Means = mean(CPUE, na.rm=TRUE),
         SDs = sd(CPUE, na.rm=TRUE),
         Zs = (CPUE-Means)/ifelse(SDs>0,SDs, 0.00000001)) %>%
  ungroup() 

LandingsCPUE = ggplot(aes(Year, CPUE), data = subset(LandingsCrab,
                                                     ProjID == "LandingsCPUE")) +
  ggtitle("Mean Annual Landings CPUE\nCharleston Harbor (Combined Watersheds)") +
  geom_hline(aes(yintercept=mean(CPUE)), linetype="dashed") +
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula1, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  labs(y=expression(Blue~crab~per~tow~or~set^{-1})) +
  expand_limits(x=c(1980, 2022))


LandingsFormTotal = subset(crab,
                      ProjID == "Landings")
formula2 <- lm(LandingsFormTotal$Year ~ LandingsFormTotal$CPUE)
Landings = ggplot(aes(Year, CPUE), data = subset(LandingsCrab,
                                                 ProjID == "Landings")) +
  ggtitle("Mean Annual Landings\nCharleston Harbor (Combined Watersheds)") +
  geom_hline(aes(yintercept=mean(CPUE)), linetype="dashed") +
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula2, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  labs(y=expression(Blue~crab~per~tow~or~set^{-1})) +
  expand_limits(x=c(1980, 2022))


LandingsTimeSeries <- plot_grid(Landings, LandingsCPUE, ncol = 1, align = 'hv')
LandingsTimeSeries







# Total Abundance Plots ---------------------------------------------------

TotalCrab = crab %>%
  filter(Lifestage == "Total")
TotalCrab$ProjID <- factor(TotalCrab$ProjID, levels = c("Harbor Trawl",
                                                        "SCECAP Harbor Trawl",
                                                        "Creek Trawl",
                                                        "SCECAP Creek Trawl",
                                                        "Trammel Net",
                                                        "Ashley Potting"))

B90FormTotal = subset(TotalCrab,
                     ProjID == "Harbor Trawl")
formula3 <- lm(B90FormTotal$Year ~ B90FormTotal$CPUE)
B90TotalCPUE = ggplot(aes(Year, CPUE), data = subset(TotalCrab,
                                                     ProjID == "Harbor Trawl")) +
  ggtitle("Harbor Trawl") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula3, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)
#B90TotalCPUE

T38FormTotal = subset(TotalCrab,
                     ProjID == "Creek Trawl")
formula4 <- lm(T38FormTotal$Year ~ T38FormTotal$CPUE)
T38TotalCPUE = ggplot(aes(Year, CPUE), data = subset(TotalCrab,
                                                     ProjID == "Creek Trawl")) +
  ggtitle("Creek Trawl") +
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula4, parse = TRUE, label.x = "right") +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)
#T38TotalCPUE

T06FormTotal = subset(TotalCrab,
                     ProjID == "Trammel Net")
formula5 <- lm(T06FormTotal$Year ~ T06FormTotal$CPUE)
T06TotalCPUE = ggplot(aes(Year, CPUE), data = subset(TotalCrab,
                                                     ProjID == "Trammel Net")) +
  ggtitle("Trammel Net") +
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula5, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~set^{-1})) +
  expand_limits(x=c(1980,2020))
#T06TotalCPUE

E98FormTotal = subset(TotalCrab,
                     ProjID == "SCECAP Creek Trawl")
formula6 <- lm(E98FormTotal$Year ~ E98FormTotal$CPUE)
E98TotalCPUE = ggplot(aes(Year, log(CPUE)), data = subset(TotalCrab,
                                                          ProjID == "SCECAP Creek Trawl")) +
  ggtitle("SCECAP Tidal Creek Trawl") +
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula6, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=(log(mean(Means)))), linetype="dashed") +
  labs(y=expression((log)~Blue~crab~per~tow^{-1})) +
  expand_limits(x=c(1980,2020)) +
  coord_cartesian(ylim = c(0,8))

E99FormTotal = subset(TotalCrab,
                     ProjID == "SCECAP Harbor Trawl")
formula7 <- lm(E99FormTotal$Year ~ E99FormTotal$CPUE)
E99TotalCPUE = ggplot(aes(Year, CPUE), data = subset(TotalCrab,
                                                     ProjID == "SCECAP Harbor Trawl")) +
  ggtitle("SCECAP Open Water Trawl") +
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula7, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=c(1980,2020)) 

P88FormTotal = subset(TotalCrab,
                     ProjID == "Ashley Potting")
formula8<- lm(P88FormTotal$Year ~ P88FormTotal$CPUE)
P88FallTotalCPUE = TotalCrab %>%
  filter(ProjID == "Ashley Potting") %>%
  filter(Month %in% c("10", "11"))
P88FallTotalCPUE = ggplot(aes(Year, CPUE), data = subset(TotalCrab,
                                                         ProjID == "Ashley Potting")) +
  ggtitle("Ashley River Potting Survey (fall)") +
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula8, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black", na.rm = TRUE) +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~soak^{-1})) +
  expand_limits(x=c(1980,2020))


##Only going to use this if we need a year-round look, but only goes to 2003
P88FormTotal2 = subset(TotalCrab,
                      ProjID == "Ashley Potting" &
                        Year > 2002)
formula9 <- lm(P88FormTotal$Year ~ P88FormTotal$CPUE)
P88TotalAnnualCPUE = ggplot(aes(Year, CPUE), data = subset(TotalCrab,
                                                           ProjID == "Ashley Potting" &
                                                             Year > 2002)) +
  ggtitle("Ashley River Potting Survey (bi-monthly)") +
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula9, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black", na.rm = TRUE) +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~soak^{-1})) +
  expand_limits(x=c(1980,2020))

CRMSCPUETimeSeries <- plot_grid(B90TotalCPUE, T38TotalCPUE, P88FallTotalCPUE, ncol = 1, align = 'hv')

CRMSCPUETimeSeries

OtherCPUETimeSeries <- plot_grid(E99TotalCPUE, E98TotalCPUE, T06TotalCPUE, ncol = 1, align = 'hv')

OtherCPUETimeSeries






# B90 ---------------------------------------------------------------------

B90Crab = crab %>%
  filter(ProjID == "Harbor Trawl") %>%
  filter(Lifestage == c("Juvenile", "Subadult", "Adult", "Immature Female",
                        "Immature Male", "Mature Female", "Mature Male",
                        "Sublegal", "Legal"))

B90Crab$Lifestage <- factor(B90Crab$Lifestage, levels = 
                              c("Juvenile", "Subadult", "Adult", 
                                "Immature Female", "Immature Male", 
                                "Mature Female", "Mature Male",
                                "Sublegal", "Legal"))

B90JuvForm = subset(B90Crab,
                           Lifestage == "Juvenile")
formula10 <- lm(B90JuvForm$Year ~ B90JuvForm$CPUE)
B90Juv = ggplot(aes(Year, CPUE), data = subset(B90Crab,
                                               Lifestage == "Juvenile")) +
  ggtitle("Juvenile CPUE (<61mm Carapace Width)") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula10, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)



B90SubadultForm = subset(B90Crab,
                    Lifestage == "Subadult")
formula11 <- lm(B90SubadultForm$Year ~ B90SubadultForm$CPUE)
B90Subadult = ggplot(aes(Year, CPUE), data = subset(B90Crab,
                                                    Lifestage == "Subadult")) +
  ggtitle("Subadult CPUE (>61mm - <127mm Carapace Width)") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula11, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)



B90AdultForm = subset(B90Crab,
                         Lifestage == "Adult")
formula12 <- lm(B90AdultForm$Year ~ B90AdultForm$CPUE)
B90Adult = ggplot(aes(Year, log(CPUE)), data = subset(B90Crab,
                                                      Lifestage == "Adult")) +
  ggtitle("Adult CPUE (>126mm Carapace Width)") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula12, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=log(mean(Means))), linetype="dashed") +
  labs(y=expression((log)~Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)



B90IFForm = subset(B90Crab,
                      Lifestage == "Immature Female")
formula13 <- lm(B90IFForm$Year ~ B90IFForm$CPUE)
B90ImmatureFemale = ggplot(aes(Year, CPUE), data = subset(B90Crab,
                                                          Lifestage == "Immature Female")) +
  ggtitle("Immature Female") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula13, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)



#Inf, NA and NaNs in Mature Female Crab 
B90MatureFemaleCrab <- as.data.frame(B90Crab) %>%
  filter(Lifestage == "Mature Female") 
B90MatureFemaleCrab$Means[is.infinite(B90MatureFemaleCrab$Means)] <- NA
B90MatureFemaleCrab$SDs[is.infinite(B90MatureFemaleCrab$SDs)] <- NA
B90MatureFemaleCrab$Zs[is.infinite(B90MatureFemaleCrab$Zs)] <- NA
B90MatureFemaleCrab$Means[is.nan(B90MatureFemaleCrab$Means)] <- NA
B90MatureFemaleCrab$SDs[is.nan(B90MatureFemaleCrab$SDs)] <- NA
B90MatureFemaleCrab$Zs[is.nan(B90MatureFemaleCrab$Zs)] <- NA
B90MFForm = subset(B90Crab,
                   Lifestage == "Mature Female")
formula14 <- lm(B90MFForm$CPUE ~ B90MFForm$Year)
B90MatureFemale = ggplot(aes(Year, CPUE), data = subset(B90MatureFemaleCrab,
                                                        Lifestage == "Mature Female")) +
  ggtitle("Mature Female") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula14, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(na.exclude(Means))), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)



B90IMForm = subset(B90Crab,
                   Lifestage == "Immature Male")
formula15 <- lm(B90IMForm$Year ~ B90IMForm$CPUE)
B90ImmatureMale = ggplot(aes(Year, CPUE), data = subset(B90Crab,
                                                        Lifestage == "Immature Male")) +
  ggtitle("Immature Male CPUE") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula15, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)



B90MMForm = subset(B90Crab,
                   Lifestage == "Mature Male")
formula16 <- lm(B90MMForm$Year ~ B90MMForm$CPUE)
B90MatureMale = ggplot(aes(Year, CPUE), data = subset(B90Crab,
                                                      Lifestage == "Mature Male")) +
  ggtitle("Mature Male CPUE") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula16, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)




B90SublegalForm = subset(B90Crab,
                   Lifestage == "Sublegal")
formula17 <- lm(B90SublegalForm$Year ~ B90SublegalForm$CPUE)
B90Sublegal = ggplot(aes(Year, CPUE), data = subset(B90Crab,
                                                    Lifestage == "Sublegal")) +
  ggtitle("Sublegal CPUE (<127mm Carapace Width)") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula17, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)


B90LegalForm = subset(B90Crab,
                         Lifestage == "Legal")
formula18 <- lm(B90LegalForm$Year ~ B90LegalForm$CPUE)
B90Legal = ggplot(aes(Year, log(CPUE)), data = subset(B90Crab,
                                                      Lifestage == "Legal")) +
  ggtitle("Legal CPUE (>126mm Carapace Width)") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula18, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=log(mean(Means))), linetype="dashed") +
  labs(y=expression((log)~Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)





B90SizeTimeSeries <- plot_grid(B90Juv, B90Subadult, B90Adult, ncol = 1, align = 'hv')
B90SizeTimeSeries

B90ClassTimeSeries <- plot_grid(B90ImmatureFemale, B90ImmatureMale, B90MatureFemale, B90MatureMale, ncol = 1, align = 'hv')
B90ClassTimeSeries

B90LegalTimeSeries <- plot_grid(B90Sublegal, B90Legal, ncol = 1, align = 'hv')
B90LegalTimeSeries





# T38 ---------------------------------------------------------------------

T38Crab = crab %>%
  filter(ProjID == "Creek Trawl") %>%
  filter(Lifestage == c("Juvenile", "Subadult", "Adult", "Immature Female",
                        "Immature Male", "Mature Female", "Mature Male",
                        "Sublegal", "Legal"))

T38Crab$Lifestage <- factor(T38Crab$Lifestage, levels = 
                              c("Juvenile", "Subadult", "Adult", 
                                "Immature Female", "Immature Male", 
                                "Mature Female", "Mature Male",
                                "Sublegal", "Legal"))



T38JuvForm = subset(T38Crab,
                      Lifestage == "Juvenile")
formula19 <- lm(T38JuvForm$Year ~ T38JuvForm$CPUE)
T38Juv = ggplot(aes(Year, CPUE), data = subset(T38Crab,
                                               Lifestage == "Juvenile")) +
  ggtitle("Juvenile CPUE (<61mm Carapace Width)") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula19, parse = TRUE, label.x = "right") +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)


T38SubadultForm = subset(T38Crab,
                    Lifestage == "Subadult")
formula20 <- lm(T38SubadultForm$Year ~ T38SubadultForm$CPUE)
T38Subadult = ggplot(aes(Year, CPUE), data = subset(T38Crab,
                                                    Lifestage == "Subadult")) +
  ggtitle("Subadult CPUE (>61mm - <127mm Carapace Width)") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula20, parse = TRUE, label.x = "right") +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)


T38AdultForm = subset(T38Crab,
                         Lifestage == "Adult")
formula21 <- lm(T38AdultForm$Year ~ T38AdultForm$CPUE)
T38Adult = ggplot(aes(Year, log(CPUE)), data = subset(T38Crab,
                                                      Lifestage == "Adult")) +
  ggtitle("Adult CPUE (>126mm Carapace Width)") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula21, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=log(mean(Means))), linetype="dashed") +
  labs(y=expression((log)~Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)





T38IFForm = subset(T38Crab,
                      Lifestage == "Immature Female")
formula22 <- lm(T38IFForm$Year ~ T38IFForm$CPUE)
T38ImmatureFemale = ggplot(aes(Year, CPUE), data = subset(T38Crab,
                                                          Lifestage == "Immature Female")) +
  ggtitle("Immature Female") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula22, parse = TRUE, label.x = "right") +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)

T38MFForm = subset(T38Crab,
                   Lifestage == "Mature Female")
formula23 <- lm(T38MFForm$Year ~ T38MFForm$CPUE)
T38MatureFemale = ggplot(aes(Year, CPUE), data = subset(T38Crab,
                                                        Lifestage == "Mature Female")) +
  ggtitle("Mature Female") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula23, parse = TRUE, label.x = "right") +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)


T38IMForm = subset(T38Crab,
                   Lifestage == "Immature Male")
formula24 <- lm(T38IMForm$Year ~ T38IMForm$CPUE)
T38ImmatureMale = ggplot(aes(Year, CPUE), data = subset(T38Crab,
                                                        Lifestage == "Immature Male")) +
  ggtitle("Immature Male CPUE") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula24, parse = TRUE, label.x = "right") +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)

T38MMForm = subset(T38Crab,
                   Lifestage == "Mature Male")
formula25 <- lm(T38MMForm$Year ~ T38MMForm$CPUE)
T38MatureMale = ggplot(aes(Year, CPUE), data = subset(T38Crab,
                                                      Lifestage == "Mature Male")) +
  ggtitle("Mature Male CPUE") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula25, parse = TRUE, label.x = "right") +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)




T38SublegalForm = subset(T38Crab,
                   Lifestage == "Sublegal")
formula26 <- lm(T38SublegalForm$Year ~ T38SublegalForm$CPUE)
T38Sublegal = ggplot(aes(Year, CPUE), data = subset(T38Crab,
                                                    Lifestage == "Sublegal")) +
  ggtitle("Sublegal CPUE (<127mm Carapace Width)") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula26, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)


T38LegalForm = subset(T38Crab,
                         Lifestage == "Legal")
formula27 <- lm(T38LegalForm$Year ~ T38LegalForm$CPUE)
T38Legal = ggplot(aes(Year, log(CPUE)), data = subset(T38Crab,
                                                      Lifestage == "Legal")) +
  ggtitle("Legal CPUE (>126mm Carapace Width)") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula27, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=log(mean(Means))), linetype="dashed") +
  labs(y=expression((log)~Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)





T38SizeTimeSeries <- plot_grid(T38Juv, T38Subadult, T38Adult, ncol = 1, align = 'hv')
T38SizeTimeSeries

T38ClassTimeSeries <- plot_grid(T38ImmatureFemale, T38ImmatureMale, T38MatureFemale, T38MatureMale, ncol = 1, align = 'hv')
T38ClassTimeSeries

T38LegalTimeSeries <- plot_grid(T38Sublegal, T38Legal, ncol = 1, align = 'hv')
T38LegalTimeSeries





# P88 ---------------------------------------------------------------------

P88Crab = crab %>%
  filter(ProjID == "Ashley Potting") %>%
  filter(Lifestage == c("Sublegal", "Legal"))

P88Crab$Lifestage <- factor(P88Crab$Lifestage, levels = 
                              c("Sublegal", "Legal"))



P88SublegalForm = subset(P88Crab,
                      Lifestage == "Sublegal")
formula28 <- lm(P88SublegalForm$Year ~ P88SublegalForm$CPUE)
P88SublegalCrab = ggplot(aes(Year, CPUE), data = subset(P88Crab,
                                                        Lifestage == "Sublegal")) +
  ggtitle("Sublegal (<127mm Carapace Width)") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula28, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~soak^{-1})) +
  expand_limits(x=2020)

P88LegalForm = subset(P88Crab,
                         Lifestage == "Legal")
formula29 <- lm(P88SublegalForm$Year ~ P88SublegalForm$CPUE)
P88LegalCrab = ggplot(aes(Year, CPUE), data = subset(P88Crab,
                                                     Lifestage == "Legal")) +
  ggtitle("Legal (>126mm Carapace Width)") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula29, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~soak^{-1})) +
  expand_limits(x=2020)


P88SizeTimeSeries <- plot_grid(P88SublegalCrab, P88LegalCrab, ncol = 1, align = 'hv')
P88SizeTimeSeries






# E98 ---------------------------------------------------------------------

E98Crab = crab %>%
  filter(ProjID == "SCECAP Creek Trawl") %>%
  filter(Lifestage == c("Juvenile", "Subadult", "Adult",
                        "Sublegal", "Legal"))

E98Crab$Lifestage <- factor(E98Crab$Lifestage, levels = 
                              c("Juvenile", "Subadult", "Adult",
                                "Sublegal", "Legal"))



E98JuvForm = subset(E98Crab,
                      Lifestage == "Juvenile")
formula30 <- lm(E98JuvForm$Year ~ E98JuvForm$CPUE)
E98JuvenileCrab = ggplot(aes(Year, CPUE), data = subset(E98Crab,
                                                        Lifestage == "Juvenile")) +
  ggtitle("Juvenile (<61mm Carapace Width)") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula30, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~trawl^{-1})) +
  expand_limits(x=2020)


E98SubadultForm = subset(E98Crab,
                    Lifestage == "Subadult")
formula31 <- lm(E98SubadultForm$Year ~ E98SubadultForm$CPUE)
E98SubadultCrab = ggplot(aes(Year, CPUE), data = subset(E98Crab,
                                                        Lifestage == "Subadult")) +
  ggtitle("Subadult (>60mm & <127mm Carapace Width)") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula31, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~trawl^{-1})) +
  expand_limits(x=2020)


E98AdultForm = subset(E98Crab,
                         Lifestage == "Adult")
formula32 <- lm(E98AdultForm$Year ~ E98AdultForm$CPUE)
E98AdultCrab = ggplot(aes(Year, CPUE), data = subset(E98Crab,
                                                     Lifestage == "Adult")) +
  ggtitle("Subadult (>126mm Carapace Width)") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula32, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~trawl^{-1})) +
  expand_limits(x=2020)




E98SublegalForm = subset(E98Crab,
                      Lifestage == "Sublegal")
formula33 <- lm(E98SublegalForm$Year ~ E98SublegalForm$CPUE)
E98Sublegal = ggplot(aes(Year, CPUE), data = subset(E98Crab,
                                                    Lifestage == "Sublegal")) +
  ggtitle("Sublegal CPUE (<127mm Carapace Width)") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula33, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)

E98LegalForm = subset(E98Crab,
                         Lifestage == "Legal")
formula34 <- lm(E98LegalForm$Year ~ E98LegalForm$CPUE)
E98Legal = ggplot(aes(Year, CPUE), data = subset(E98Crab,
                                                      Lifestage == "Legal")) +
  ggtitle("Legal sized CPUE (>126mm Carapace Width)") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula34, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)






E98SizeTimeSeries <- plot_grid(E98JuvenileCrab, E98SubadultCrab, E98AdultCrab, ncol = 1, align = 'hv')
E98SizeTimeSeries

E98LegalTimeSeries <- plot_grid(E98Sublegal, E98Legal, ncol = 1, align = 'hv')
E98LegalTimeSeries





# E99 ---------------------------------------------------------------------

E99Crab = crab %>%
  filter(ProjID == "SCECAP Harbor Trawl") %>%
  filter(Lifestage == c("Juvenile", "Subadult", "Adult",
                        "Sublegal", "Legal"))

E99Crab$Lifestage <- factor(E99Crab$Lifestage, levels = 
                              c("Juvenile", "Subadult", "Adult",
                                "Sublegal", "Legal"))



E99JuvForm = subset(E99Crab,
                      Lifestage == "Juvenile")
formula35 <- lm(E99JuvForm$Year ~ E99JuvForm$CPUE)
E99JuvenileCrab = ggplot(aes(Year, CPUE), data = subset(E99Crab,
                                                        Lifestage == "Juvenile")) +
  ggtitle("Juvenile (<61mm Carapace Width)") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula35, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~trawl^{-1})) +
  expand_limits(x=2020)


E99SubadultForm = subset(E99Crab,
                    Lifestage == "Subadult")
formula36 <- lm(E99SubadultForm$Year ~ E99SubadultForm$CPUE)
E99SubadultCrab = ggplot(aes(Year, CPUE), data = subset(E99Crab,
                                                        Lifestage == "Subadult")) +
  ggtitle("Subadult (>60mm & <127mm Carapace Width)") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula36, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~trawl^{-1})) +
  expand_limits(x=2020)

E99AdultForm = subset(E99Crab,
                         Lifestage == "Adult")
formula37 <- lm(E99AdultForm$Year ~ E99AdultForm$CPUE)
E99AdultCrab = ggplot(aes(Year, CPUE), data = subset(E99Crab,
                                                     Lifestage == "Adult")) +
  ggtitle("Subadult (>126mm Carapace Width)") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula37, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~trawl^{-1})) +
  expand_limits(x=2020)





E99SublegalForm = subset(E99Crab,
                      Lifestage == "Sublegal")
formula38 <- lm(E99SublegalForm$Year ~ E99SublegalForm$CPUE)
E99Sublegal = ggplot(aes(Year, CPUE), data = subset(E99Crab,
                                                    Lifestage == "Sublegal")) +
  ggtitle("Sublegal CPUE (<127mm Carapace Width)") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula38, parse = TRUE, label.x = "right") +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)


E99LegalForm = subset(E99Crab,
                         Lifestage == "Legal")
formula39 <- lm(E99LegalForm$Year ~ E99LegalForm$CPUE)
E99Legal = ggplot(aes(Year, CPUE), data = subset(E99Crab,
                                                      Lifestage == "Legal")) +
  ggtitle("Legal sized CPUE (>126mm Carapace Width)") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula39, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(Means)), linetype="dashed") +
  labs(y=expression((log)~Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)






E99SizeTimeSeries <- plot_grid(E99JuvenileCrab, E99SubadultCrab, E99AdultCrab, ncol = 1, align = 'hv')
E99SizeTimeSeries

E99LegalTimeSeries <- plot_grid(E99Sublegal, E99Legal, ncol = 1, align = 'hv')
E99LegalTimeSeries
####THESE ARE MISSING DATA DUE TO inf'S TOO!

# Equations ---------------------------------------------------------------

lm(formula = Year ~ CPUE, data = subset(TotalCrab, ProjID == 
                                          "Harbor Trawl"))
lm(formula = Year ~ CPUE, data = subset(TotalCrab, ProjID == 
                                          "Creek Trawl"))
lm(formula = Year ~ CPUE, data = subset(TotalCrab, ProjID == 
                                          "Ashley Potting"))
lm(formula = Year ~ CPUE, data = subset(TotalCrab, ProjID == 
                                          "SCECAP Harbor Trawl"))
lm(formula = Year ~ CPUE, data = subset(TotalCrab, ProjID == 
                                          "SCECAP Creek Trawl"))
lm(formula = Year ~ CPUE, data = subset(TotalCrab, ProjID == 
                                          "Trammel Net"))



lm(formula = Year ~ CPUE, data = subset(LandingsCrab, ProjID == 
                                          "Landings"))
lm(formula = Year ~ CPUE, data = subset(LandingsCrab, ProjID == 
                                          "LandingsCPUE"))


lm(formula = Year ~ CPUE, data = subset(B90Crab, Lifestage == "Juvenile"))
lm(formula = Year ~ CPUE, data = subset(B90Crab, Lifestage == "Subadult"))
lm(formula = Year ~ CPUE, data = subset(B90Crab, Lifestage == "Adult"))
lm(formula = Year ~ CPUE, data = subset(B90Crab, Lifestage == "Immature Female"))
lm(formula = Year ~ CPUE, data = subset(B90Crab, Lifestage == "Mature Female"))
lm(formula = Year ~ CPUE, data = subset(B90Crab, Lifestage == "Immature Male"))
lm(formula = Year ~ CPUE, data = subset(B90Crab, Lifestage == "Mature Male"))
lm(formula = Year ~ CPUE, data = subset(B90Crab, Lifestage == "Sublegal"))
lm(formula = Year ~ CPUE, data = subset(B90Crab, Lifestage == "Legal"))


lm(formula = Year ~ CPUE, data = subset(T38Crab, Lifestage == "Juvenile"))
lm(formula = Year ~ CPUE, data = subset(T38Crab, Lifestage == "Subadult"))
lm(formula = Year ~ CPUE, data = subset(T38Crab, Lifestage == "Adult"))
lm(formula = Year ~ CPUE, data = subset(T38Crab, Lifestage == "Immature Female"))
lm(formula = Year ~ CPUE, data = subset(T38Crab, Lifestage == "Mature Female"))
lm(formula = Year ~ CPUE, data = subset(T38Crab, Lifestage == "Immature Male"))
lm(formula = Year ~ CPUE, data = subset(T38Crab, Lifestage == "Mature Male"))
lm(formula = Year ~ CPUE, data = subset(T38Crab, Lifestage == "Sublegal"))
lm(formula = Year ~ CPUE, data = subset(T38Crab, Lifestage == "Legal"))

lm(formula = Year ~ CPUE, data = subset(P88Crab, Lifestage == "Sublegal"))
lm(formula = Year ~ CPUE, data = subset(P88Crab, Lifestage == "Legal"))

lm(formula = Year ~ CPUE, data = subset(E98Crab, Lifestage == "Juvenile"))
lm(formula = Year ~ CPUE, data = subset(E98Crab, Lifestage == "Subadult"))
lm(formula = Year ~ CPUE, data = subset(E98Crab, Lifestage == "Adult"))

lm(formula = Year ~ CPUE, data = subset(E99Crab, Lifestage == "Juvenile"))
lm(formula = Year ~ CPUE, data = subset(E99Crab, Lifestage == "Subadult"))
lm(formula = Year ~ CPUE, data = subset(E99Crab, Lifestage == "Adult"))

