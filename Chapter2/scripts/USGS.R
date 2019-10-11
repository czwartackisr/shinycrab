library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)
library(Hmisc)
library(nlme)
library(cowplot)
library(gridExtra)
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




crab = read.csv("./Chapter2/data/dataGather2.csv")
crab = crab %>%
  mutate(ProjID = recode(ProjID,
                         'B90' = "Harbor Trawl",
                         'T38' = "CreekTrawl",
                         'T06' = "Trammel Net",
                         'P88' = "Ashley Potting")) 
crab$Salinity <- factor(crab$Salinity, levels = 
                          c("Spot", "CSI_Raw", "CSI_12", "CSI_24"))





# Cooper River ------------------------------------------------------------

TotalCooperplot <- ggplot(aes(PSU, CPUE), data = subset(crab, Lifestage == "Total" &
                                                        Watershed == "Cooper")) +
  ggtitle("SCDNR Blue Crab Survey Total CPUEs \nby Cooper River Salinity") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="gray45") 

TotalCooperplot + facet_grid(ProjID ~ Salinity, scales = "free")



JuvenileCooperplot <- ggplot(aes(PSU, CPUE), data = subset(crab, Lifestage == "Juvenile"&
                                                           Watershed == "Cooper")) +
  ggtitle("SCDNR Blue Crab Survey Juvenile (<61mm) CPUEs \nby Cooper River Salinity") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="gray45") 

JuvenileCooperplot + facet_grid(ProjID ~ Salinity, scales = "free")



SubadultCooperplot <- ggplot(aes(PSU, CPUE), data = subset(crab, Lifestage == "Subadult"&
                                                           Watershed == "Cooper")) +
  ggtitle("SCDNR Blue Crab Survey Subadult (61-126mm carapace width) CPUEs \nby Cooper River Salinity") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="gray45") 

SubadultCooperplot + facet_grid(ProjID ~ Salinity, scales = "free")



AdultCooperplot <- ggplot(aes(PSU, CPUE), data = subset(crab, Lifestage == "Adult"&
                                                        Watershed == "Cooper")) +
  ggtitle("SCDNR Blue Crab Survey Adult (>126mm carapace width) CPUEs \nby Cooper River Salinity") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="gray45") 

AdultCooperplot + facet_grid(ProjID ~ Salinity, scales = "free")



ImmatureFemaleCooperplot <- ggplot(aes(PSU, CPUE), data = subset(crab, Lifestage == "ImmatureFemale"&
                                                                 Watershed == "Cooper")) +
  ggtitle("SCDNR Blue Crab Survey Immature Female CPUEs \nby Cooper River Salinity") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="gray45") 

ImmatureFemaleCooperplot + facet_grid(ProjID ~ Salinity, scales = "free")



MatureFemaleCooperplot <- ggplot(aes(PSU, CPUE), data = subset(crab, Lifestage == "MatureFemale"&
                                                               Watershed == "Cooper")) +
  ggtitle("SCDNR Blue Crab Survey Mature Female CPUEs \nby Cooper River Salinity") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="gray45") 

MatureFemaleCooperplot + facet_grid(ProjID ~ Salinity, scales = "free")





# Wando River -------------------------------------------------------------

TotalWandoplot <- ggplot(aes(PSU, CPUE), data = subset(crab, Lifestage == "Total" &
                                                          Watershed == "Wando")) +
  ggtitle("SCDNR Blue Crab Survey Total CPUEs \nby Wando River Salinity") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="gray45") 

TotalWandoplot + facet_grid(ProjID ~ Salinity, scales = "free")



JuvenileWandoplot <- ggplot(aes(PSU, CPUE), data = subset(crab, Lifestage == "Juvenile"&
                                                             Watershed == "Wando")) +
  ggtitle("SCDNR Blue Crab Survey Juvenile (<61mm) CPUEs \nby Wando River Salinity") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="gray45") 

JuvenileWandoplot + facet_grid(ProjID ~ Salinity, scales = "free")



SubadultWandoplot <- ggplot(aes(PSU, CPUE), data = subset(crab, Lifestage == "Subadult"&
                                                             Watershed == "Wando")) +
  ggtitle("SCDNR Blue Crab Survey Subadult (61-126mm carapace width) CPUEs \nby Wando River Salinity") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="gray45") 

SubadultWandoplot + facet_grid(ProjID ~ Salinity, scales = "free")



AdultWandoplot <- ggplot(aes(PSU, CPUE), data = subset(crab, Lifestage == "Adult"&
                                                          Watershed == "Wando")) +
  ggtitle("SCDNR Blue Crab Survey Adult (>126mm carapace width) CPUEs \nby Wando River Salinity") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="gray45") 

AdultWandoplot + facet_grid(ProjID ~ Salinity, scales = "free")



ImmatureFemaleWandoplot <- ggplot(aes(PSU, CPUE), data = subset(crab, Lifestage == "ImmatureFemale"&
                                                                   Watershed == "Wando")) +
  ggtitle("SCDNR Blue Crab Survey Immature Female CPUEs \nby Wando River Salinity") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="gray45") 

ImmatureFemaleWandoplot + facet_grid(ProjID ~ Salinity, scales = "free")



MatureFemaleWandoplot <- ggplot(aes(PSU, CPUE), data = subset(crab, Lifestage == "MatureFemale"&
                                                                 Watershed == "Wando")) +
  ggtitle("SCDNR Blue Crab Survey Mature Female CPUEs \nby Wando River Salinity") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="gray45") 

MatureFemaleCooperplot + facet_grid(ProjID ~ Salinity, scales = "free")





# Ashley River ------------------------------------------------------------

TotalAshleyplot <- ggplot(aes(PSU, CPUE), data = subset(crab, Lifestage == "Total" &
                                                         Watershed == "Ashley")) +
  ggtitle("SCDNR Blue Crab Survey Total CPUEs \nby Ashley River Salinity") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="gray45") 

TotalAshleyplot + facet_grid(ProjID ~ Salinity, scales = "free")



JuvenileAshleyplot <- ggplot(aes(PSU, CPUE), data = subset(crab, Lifestage == "Juvenile"&
                                                             Watershed == "Ashley")) +
  ggtitle("SCDNR Blue Crab Survey Juvenile (<61mm) CPUEs \nby Ashley River Salinity") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="gray45") 

JuvenileAshleyplot + facet_grid(ProjID ~ Salinity, scales = "free")



SubadultAshleyplot <- ggplot(aes(PSU, CPUE), data = subset(crab, Lifestage == "Subadult"&
                                                             Watershed == "Ashley")) +
  ggtitle("SCDNR Blue Crab Survey Subadult (61-126mm carapace width) CPUEs \nby Ashley River Salinity") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="gray45") 

SubadultAshleyplot + facet_grid(ProjID ~ Salinity, scales = "free")



AdultAshleyplot <- ggplot(aes(PSU, CPUE), data = subset(crab, Lifestage == "Adult"&
                                                          Watershed == "Ashley")) +
  ggtitle("SCDNR Blue Crab Survey Adult (>126mm carapace width) CPUEs \nby Ashley River Salinity") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="gray45") 

AdultAshleyplot + facet_grid(ProjID ~ Salinity, scales = "free")



ImmatureFemaleAshleyplot <- ggplot(aes(PSU, CPUE), data = subset(crab, Lifestage == "ImmatureFemale"&
                                                                   Watershed == "Ashley")) +
  ggtitle("SCDNR Blue Crab Survey Immature Female CPUEs \nby Ashley River Salinity") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="gray45") 

ImmatureFemaleAshleyplot + facet_grid(ProjID ~ Salinity, scales = "free")



MatureFemaleAshleyplot <- ggplot(aes(PSU, CPUE), data = subset(crab, Lifestage == "MatureFemale"&
                                                                 Watershed == "Ashley")) +
  ggtitle("SCDNR Blue Crab Survey Mature Female CPUEs \nby Ashley River Salinity") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="gray45") 

MatureFemaleAshleyplot + facet_grid(ProjID ~ Salinity, scales = "free")







