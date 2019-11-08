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

B90Landings = crab %>%
  filter(ProjID == c("Harbor Trawl", "Landings")) %>%
  filter((Lifestage == "Total" & ProjID == "Harbor Trawl") | 
           (Lifestage == "Legal" & ProjID == "Landings"))

B90Landings$Means[is.infinite(B90Landings$Means)] <- NA
B90Landings$SDs[is.infinite(B90Landings$SDs)] <- NA
B90Landings$Zs[is.infinite(B90Landings$Zs)] <- NA
B90Landings$Means[is.nan(B90Landings$Means)] <- NA
B90Landings$SDs[is.nan(B90Landings$SDs)] <- NA
B90Landings$Zs[is.nan(B90Landings$Zs)] <- NA

B90LandingsSpread = B90Landings %>%
  spread(Lifestage, CPUE)

#############################SEPARATE

B90LandingsPlot = ggplot(aes(Year, CPUE), data = B90Landings) +
  ggtitle("Mature Female") + 
  stat_summary(fun.data = "mean_se", size = .7) +
  stat_poly_eq(aes(label = ..eq.label..), formula = formula14, parse = TRUE) +
  geom_smooth(method = 'loess', se=FALSE, color="black") +
  geom_hline(aes(yintercept=mean(na.exclude(Means))), linetype="dashed") +
  labs(y=expression(Blue~crab~per~tow^{-1})) +
  expand_limits(x=2020)