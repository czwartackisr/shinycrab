
# Libraries and theme -----------------------------------------------------


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

# Wrangling ---------------------------------------------------------------

B90T38 = crabdata %>% #Formerly crab1
  select(1:4, 7:16) %>%
  filter(ProjID %in% c("B90", "T38")) %>%
  rename(Total = CPUE,
         Juvenile = JuvCPUE,
         Subadult = SubadultCPUE,
         Adult = AdultCPUE,
         ImmatureFemale = ImmatureFemaleCPUE,
         MatureFemale = MatureFemaleCPUE,
         ImmatureMale = ImmatureMaleCPUE,
         MatureMale = MatureMaleCPUE,
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
crab$CPUE[is.infinite(crab$CPUE)] <- NA


Landings = crab %>%
  filter(ProjID == c("Landings", "LandingsCPUE")) %>%
  group_by(ProjID, Lifestage, Month, Year) %>%
  summarise_at(vars(CPUE), funs(mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  group_by(ProjID, Lifestage, Year) %>%
  summarise_at(vars(CPUE), funs(mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  select(1,3,4) %>%
  spread(ProjID, CPUE) %>%
  filter(Year > 1979)


B90Abun = crab %>%
  filter(ProjID %in% "Harbor Trawl") %>%
  group_by(ProjID, Lifestage, Month, Year) %>%
  summarise_at(vars(CPUE), funs(mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  group_by(ProjID, Lifestage, Year) %>%
  summarise_at(vars(CPUE), funs(mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  spread(Lifestage, CPUE) %>%
  select(2:12)


T38Abun = crab %>%
  filter(ProjID %in% "Creek Trawl") %>%
  group_by(ProjID, Lifestage, Month, Year) %>%
  summarise_at(vars(CPUE), funs(mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  group_by(ProjID, Lifestage, Year) %>%
  summarise_at(vars(CPUE), funs(mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  spread(Lifestage, CPUE) %>%
  select(2:12) 


P88Abun = crab %>%
  filter(ProjID %in% "Ashley Potting") %>%
  group_by(ProjID, Lifestage, Month, Year) %>%
  summarise_at(vars(CPUE), funs(mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  group_by(ProjID, Lifestage, Year) %>%
  summarise_at(vars(CPUE), funs(mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  spread(Lifestage, CPUE) %>%
  select(2:4) 

E98Abun = crab %>%
  filter(ProjID %in% "SCECAP Creek Trawl") %>%
  group_by(ProjID, Lifestage, Month, Year) %>%
  summarise_at(vars(CPUE), funs(mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  group_by(ProjID, Lifestage, Year) %>%
  summarise_at(vars(CPUE), funs(mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  spread(Lifestage, CPUE) %>%
  select(2:8) 


E99Abun = crab %>%
  filter(ProjID %in% "SCECAP Harbor Trawl") %>%
  group_by(ProjID, Lifestage, Month, Year) %>%
  summarise_at(vars(CPUE), funs(mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  group_by(ProjID, Lifestage, Year) %>%
  summarise_at(vars(CPUE), funs(mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  spread(Lifestage, CPUE) %>%
  select(2:8) 


T06Abun = crab %>%
  filter(ProjID %in% "Trammel Net") %>%
  group_by(ProjID, Lifestage, Month, Year) %>%
  summarise_at(vars(CPUE), funs(mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  group_by(ProjID, Lifestage, Year) %>%
  summarise_at(vars(CPUE), funs(mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  spread(Lifestage, CPUE) %>%
  select(2, 3) 


B90crabAbun = full_join(B90Abun, Landings, by = "Year")
T38crabAbun = full_join(T38Abun, Landings, by = "Year")
P88crabAbun = full_join(P88Abun, Landings, by = "Year")
E98crabAbun = full_join(E98Abun, Landings, by = "Year")
E99crabAbun = full_join(E99Abun, Landings, by = "Year")


# Landings ~ B90 ----------------------------------------------------------

#Landings ~ Total
formulaB90_T = lm(B90crabAbun$Landings ~ B90crabAbun$Total)
B90TotalLandingsPlot = ggplot(aes(Total, Landings), data = B90crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby Harbor Trawl CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaB90_T),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formulaB90_T,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1}))

#Landings ~ Adult
formulaB90_A = lm(B90crabAbun$Landings ~ B90crabAbun$Adult)
B90AdultLandingsPlot = ggplot(aes(Adult, Landings), data = B90crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby Harbor Trawl Adult (>126mm) Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaB90_A),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formulaB90_A,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1}))


#Landings ~ lag(Subadult)
formulaB90_SA = lm(B90crabAbun$Landings ~ lag(B90crabAbun$Subadult))
B90SubadultLandingsPlot = ggplot(aes(lag(Subadult), Landings), data = B90crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby lagged (1 yr.) Harbor Trawl Subadult (>60mm & <127mm Carapace Width) Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaB90_SA),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  #stat_poly_eq(aes(label = paste(..rr.label..)), 
               #label.x.npc = "right", label.y.npc = 0.11,
               #formula = formulaB90_SA,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1}))

#Landings ~ lag(Juvenile)
formulaB90_J = lm(lead(B90crabAbun$Landings) ~ lag(B90crabAbun$Juvenile))
B90JuvenilLandingsPlot = ggplot(aes(lag(Juvenile), Landings), data = B90crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby lagged (2 yr.) Harbor Trawl Juvenile (<61mm Carapace Width) Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaB90_J),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  #stat_poly_eq(aes(label = paste(..rr.label..)), 
               #label.x.npc = "right", label.y.npc = 0.11,
               #formula = formulaB90_J,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1}))

#Landings ~ lag(Immature Female)
formulaB90_IF = lm(B90crabAbun$Landings ~ lag(B90crabAbun$ImmatureFemale))
B90IFLandingsPlot = ggplot(aes(lag(ImmatureFemale), Landings), data = B90crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby lagged (1 yr.) Harbor Trawl Immature Female Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaB90_IF),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.11,
               formula = formulaB90_IF,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1}))

#Landings ~ lag(Immature Male)
formulaB90_IM = lm(B90crabAbun$Landings ~ lag(B90crabAbun$ImmatureMale))
B90IMLandingsPlot = ggplot(aes(lag(ImmatureMale), Landings), data = B90crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby lagged (1 yr.) Harbor Trawl Immature Male Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaB90_IM),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.11,
               formula = formulaB90_IM,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1}))


#Landings ~ Mature Female
formulaB90_MF = lm(B90crabAbun$Landings ~ B90crabAbun$MatureFemale)
B90MFLandingsPlot = ggplot(aes(MatureFemale, Landings), data = B90crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby Harbor Trawl Mature Female Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaB90_MF),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.11,
               formula = formulaB90_MF,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1}))


#Landings ~ Mature Male
formulaB90_MM = lm(B90crabAbun$Landings ~ B90crabAbun$MatureMale)
B90MMLandingsPlot = ggplot(aes(MatureMale, Landings), data = B90crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby Harbor Trawl Mature Male Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaB90_MM),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.11,
               formula = formulaB90_MM,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1}))


#Landings ~ Legal
formulaB90_L = lm(B90crabAbun$Landings ~ B90crabAbun$Legal)
B90LegalLandingsPlot = ggplot(aes(Legal, Landings), data = B90crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby Harbor Trawl Legal-Sized (>126mm) Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaB90_L),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formulaB90_L,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1}))


#Landings ~ lag(Sublegal)
formulaB90_SL = lm(B90crabAbun$Landings ~ lag(B90crabAbun$Sublegal))
B90SublegalLandingsPlot = ggplot(aes(lag(Sublegal), Landings), data = B90crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby Harbor Trawl Sublegal-Sized (<127mm) Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaB90_SL),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formulaB90_SL,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1}))



B90SizeLandingsRegress <- plot_grid(B90TotalLandingsPlot, B90AdultLandingsPlot, B90SubadultLandingsPlot, B90JuvenilLandingsPlot, 
                                    ncol = 1, align = 'hv')
B90SizeLandingsRegress

B90ClassLandingsRegress <- plot_grid(B90IFLandingsPlot, B90IMLandingsPlot, B90MFLandingsPlot, B90MMLandingsPlot, 
                                    ncol = 1, align = 'hv')
B90ClassLandingsRegress

B90LegalLandingsRegress <- plot_grid(B90LegalLandingsPlot, B90SublegalLandingsPlot, 
                                     ncol = 1, align = 'hv')
B90LegalLandingsRegress


# Landings ~ T38 ----------------------------------------------------------

#Landings ~ Total
formulaT38_T = lm(T38crabAbun$Landings ~ T38crabAbun$Total)
T38TotalLandingsPlot = ggplot(aes(Total, Landings), data = T38crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby Creek Trawl CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaT38_T),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formulaT38_T,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1}))


#Landings ~ Adult
formulaT38_A = lm(T38crabAbun$Landings ~ T38crabAbun$Adult)
T38AdultLandingsPlot = ggplot(aes(Adult, Landings), data = T38crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby Creek Trawl Adult (>126mm) Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaT38_A),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formulaT38_A,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1}))


#Landings ~ lag(Subadult)
formulaT38_SA = lm(T38crabAbun$Landings ~ lag(T38crabAbun$Subadult))
T38SubadultLandingsPlot = ggplot(aes(lag(Subadult), Landings), data = T38crabAbun) +
  ggtitle("Charleston Creek Watershed Landings\nby lagged (1 yr.) Creek Trawl Subadult (>60mm & <127mm Carapace Width) Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaT38_SA),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.11,
               formula = formulaT38_SA,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1}))

#Landings ~ lag(Juvenile)
formulaT38_J = lm(lead(T38crabAbun$Landings) ~ lag(T38crabAbun$Juvenile))
T38JuvenilLandingsPlot = ggplot(aes(lag(Juvenile), Landings), data = T38crabAbun) +
  ggtitle("Charleston Creek Watershed Landings\nby lagged (2 yr.) Creek Trawl Juvenile (<61mm Carapace Width) Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaT38_J),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.11,
               formula = formulaT38_J,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1}))

#Landings ~ lag(Immature Female)
formulaT38_IF = lm(T38crabAbun$Landings ~ lag(T38crabAbun$ImmatureFemale))
T38IFLandingsPlot = ggplot(aes(lag(ImmatureFemale), Landings), data = T38crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby lagged (1 yr.) Creek Trawl Immature Female Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaT38_IF),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.11,
               formula = formulaT38_IF,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1})) 

#Landings ~ lag(Immature Male)
formulaT38_IM = lm(B90crabAbun$Landings ~ lag(T38crabAbun$ImmatureMale))
T38IMLandingsPlot = ggplot(aes(lag(ImmatureMale), Landings), data = T38crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby lagged (1 yr.) Creek Trawl Immature Male Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaT38_IM),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.11,
               formula = formulaT38_IM,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1}))


#Landings ~ Mature Female
formulaT38_MF = lm(T38crabAbun$Landings ~ T38crabAbun$MatureFemale)
T38MFLandingsPlot = ggplot(aes(MatureFemale, Landings), data = T38crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby Creek Trawl Mature Female Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaT38_MF),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.11,
               formula = formulaT38_MF,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1}))


#Landings ~ Mature Male
formulaT38_MM = lm(B90crabAbun$Landings ~ T38crabAbun$MatureMale)
T38MMLandingsPlot = ggplot(aes(MatureMale, Landings), data = T38crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby Creek Trawl Mature Male Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaT38_MM),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.11,
               formula = formulaT38_MM,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1}))


#Landings ~ Legal
formulaT38_L = lm(T38crabAbun$Landings ~ T38crabAbun$Legal)
T38LegalLandingsPlot = ggplot(aes(Legal, Landings), data = T38crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby Creek Trawl Legal-Sized (>126mm) Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaT38_L),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formulaT38_L,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1}))


#Landings ~ lag(Sublegal)
formulaT38_SL = lm(T38crabAbun$Landings ~ lag(T38crabAbun$Sublegal))
T38SublegalLandingsPlot = ggplot(aes(lag(Sublegal), Landings), data = T38crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby Creek Trawl Sublegal-Sized (<127mm) Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaT38_SL),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formulaT38_SL,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1}))






T38SizeLandingsRegress <- plot_grid(T38TotalLandingsPlot, T38AdultLandingsPlot, T38SubadultLandingsPlot, T38JuvenilLandingsPlot, 
                                    ncol = 1, align = 'hv')
T38SizeLandingsRegress

T38ClassLandingsRegress <- plot_grid(T38IFLandingsPlot, T38IMLandingsPlot, T38MFLandingsPlot, T38MMLandingsPlot, 
                                     ncol = 1, align = 'hv')
T38ClassLandingsRegress

T38LegalLandingsRegress <- plot_grid(T38LegalLandingsPlot, T38SublegalLandingsPlot, 
                                     ncol = 1, align = 'hv')
T38LegalLandingsRegress



# Landings ~ P88 ----------------------------------------------------------

#Landings ~ Legal
formulaP88_L = lm(P88crabAbun$Landings ~ P88crabAbun$Legal)
P88LegalLandingsPlot = ggplot(aes(Legal, Landings), data = P88crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby Ashley Potting Survey Legal-Sized (>126mm) Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaP88_L),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formulaP88_L,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~per~soak^{-1}))


#Landings ~ lag(Sublegal)
formulaP88_SL = lm(P88crabAbun$Landings ~ lag(P88crabAbun$Sublegal))
P88SublegalLandingsPlot = ggplot(aes(lag(Sublegal), Landings), data = P88crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby Ashley Potting Survey Sublegal-Sized (<127mm) Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaP88_SL),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formulaP88_SL,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~per~soak^{-1}))

P88SizeLandingsRegress <- plot_grid(P88LegalLandingsPlot, P88SublegalLandingsPlot, 
                                    ncol = 1, align = 'hv')
P88SizeLandingsRegress




# Landings ~ E98 ----------------------------------------------------------

#Landings ~ Total
formulaE98_T = lm(E98crabAbun$Landings ~ E98crabAbun$Total)
E98TotalLandingsPlot = ggplot(aes(log(Total), Landings), data = E98crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby SCECAP Open Water Trawl CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaE98_T),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formulaE98_T,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression((log)~blue~crab~abundance~per~trawl^{-1}))


#Landings ~ Adult
formulaE98_A = lm(E98crabAbun$Landings ~ E98crabAbun$Adult)
E98AdultLandingsPlot = ggplot(aes(log(Adult), Landings), data = E98crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby SCECAP Creek Trawl Adult (>126mm) Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaE98_A),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formulaE98_A,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression((log)~blue~crab~abundance~per~trawl^{-1}))


#Landings ~ lag(Subadult)
formulaE98_SA = lm(E98crabAbun$Landings ~ lag(E98crabAbun$Subadult))
E98SubadultLandingsPlot = ggplot(aes(lag(log(Subadult)), Landings), data = E98crabAbun) +
  ggtitle("Charleston Creek Watershed Landings\nby lagged (1 yr.) Creek Trawl Subadult (>60mm & <127mm Carapace Width) Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaE98_SA),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.11,
               formula = formulaE98_SA,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression((log)~blue~crab~abundance~per~trawl^{-1})) 

#Landings ~ lag(Juvenile)
formulaE98_J = lm(lead(E98crabAbun$Landings) ~ lag(E98crabAbun$Juvenile))
E98JuvenilLandingsPlot = ggplot(aes(lag(log(Juvenile)), Landings), data = E98crabAbun) +
  ggtitle("Charleston Creek Watershed Landings\nby lagged (2 yr.) SCECAPCreek Trawl Juvenile (<61mm Carapace Width) Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaE98_J),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.11,
               formula = formulaE98_J,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression((log)~blue~crab~abundance~per~trawl^{-1})) 




#Landings ~ Legal
formulaE98_L = lm(E98crabAbun$Landings ~ E98crabAbun$Legal)
E98LegalLandingsPlot = ggplot(aes(log(Legal), Landings), data = E98crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby SCECAP Tidal Creek Trawl Legal-Sized (>126mm) Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaE98_L),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formulaE98_L,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression((log)~blue~crab~abundance~per~trawl^{-1}))


#Landings ~ lag(Sublegal)
formulaE98_SL = lm(E98crabAbun$Landings ~ lag(E98crabAbun$Sublegal))
E98SublegalLandingsPlot = ggplot(aes(lag(log(Sublegal)), Landings), data = E98crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby SCECAP Tidal Creek Trawl Sublegal-Sized (<127mm) Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaE98_SL),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formulaE98_SL,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression((log)~blue~crab~abundance~per~trawl^{-1}))




E98SizeLandingsRegress <- plot_grid(E98TotalLandingsPlot, E98AdultLandingsPlot, E98SubadultLandingsPlot, E98JuvenilLandingsPlot, 
                                    ncol = 1, align = 'hv')
E98SizeLandingsRegress

E98LegalLandingsRegress <- plot_grid(E98LegalLandingsPlot, E98SublegalLandingsPlot, 
                                     ncol = 1, align = 'hv')
E98LegalLandingsRegress



# Landings ~ E99 ----------------------------------------------------------

#Landings ~ Total
formulaE99_T = lm(E99crabAbun$Landings ~ E99crabAbun$Total)
E99TotalLandingsPlot = ggplot(aes(Total, Landings), data = E99crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby Harbor Trawl CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaE99_T),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formulaE99_T,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1}))


#Landings ~ Adult
formulaE99_A = lm(E99crabAbun$Landings ~ E99crabAbun$Adult)
E98AdultLandingsPlot = ggplot(aes(Adult, Landings), data = E99crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby SCECAP Open Water Trawl Adult (>126mm) Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaE99_A),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formulaE99_A,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1}))


#Landings ~ lag(Subadult)
formulaE99_SA = lm(E99crabAbun$Landings ~ lag(E99crabAbun$Subadult))
E99SubadultLandingsPlot = ggplot(aes(lag(Subadult), Landings), data = E99crabAbun) +
  ggtitle("Charleston Creek Watershed Landings\nby lagged (1 yr.) Open Water Trawl Subadult (>60mm & <127mm Carapace Width) Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaE99_SA),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.11,
               formula = formulaE99_SA,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1})) 

#Landings ~ lag(Juvenile)
formulaE99_J = lm(lead(E99crabAbun$Landings) ~ lag(E99crabAbun$Juvenile))
E99JuvenilLandingsPlot = ggplot(aes(lag(Juvenile), Landings), data = E99crabAbun) +
  ggtitle("Charleston Creek Watershed Landings\nby lagged (2 yr.) SCECAPCreek Open Water Juvenile (<61mm Carapace Width) Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaE99_J),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.11,
               formula = formulaE99_J,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1})) 



#Landings ~ Legal
formulaE99_L = lm(E99crabAbun$Landings ~ E99crabAbun$Legal)
E99LegalLandingsPlot = ggplot(aes(Legal, Landings), data = E99crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby SCECAP Open Water Trawl Legal-Sized (>126mm) Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaE99_L),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formulaE99_L,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1}))


#Landings ~ lag(Sublegal)
formulaE99_SL = lm(E99crabAbun$Landings ~ lag(E99crabAbun$Sublegal))
E99SublegalLandingsPlot = ggplot(aes(lag(Sublegal), Landings), data = E99crabAbun) +
  ggtitle("Charleston Harbor Watershed Landings\nby SCECAP Open Water Trawl Sublegal-Sized (<127mm) Abundance CPUE") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = formulaE99_SL),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 4), sep = "")),
                  parse = TRUE) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.15,
               formula = formulaE99_SL,rr.digits = 4,  parse = TRUE, size = 4) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(blue~crab~landings~(bu.)~per~year^{-1}),
       x=expression(blue~crab~abundance~per~trawl^{-1}))



E99SizeLandingsRegress <- plot_grid(E99AdultLandingsPlot, E99SubadultLandingsPlot, E99JuvenilLandingsPlot, 
                                    ncol = 1, align = 'hv')
E99SizeLandingsRegress

E99LegalLandingsRegress <- plot_grid(E99LegalLandingsPlot, E99SublegalLandingsPlot, 
                                     ncol = 1, align = 'hv')
E99LegalLandingsRegress
