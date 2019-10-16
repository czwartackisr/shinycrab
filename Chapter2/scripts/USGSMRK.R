library(ggplot2)
library(ggpubr)
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



# Data read-in ------------------------------------------------------------

crab = read.csv("./Chapter2/data/dataGather2.csv")
crab = crab %>%
  mutate(ProjID = recode(ProjID,
                         'B90' = "Harbor Trawl",
                         'T38' = "Creek Trawl",
                         'T06' = "Trammel Net",
                         'P88' = "Ashley Potting")) %>%
  mutate(Salinity = recode(Salinity,
                           'Spot' = "YSI",
                           'CSI_Raw' = "Raw CSI",
                           'CSI_12' = "12mo CSI",
                           'CSI_24' = "24mo CSI")) 

crab$Salinity <- factor(crab$Salinity, levels = 
                          c("YSI", "Raw CSI", "12mo CSI", "24mo CSI"))

crab2 = crab %>%
  filter(Salinity != "12mo CSI") 

# Summary Statistics ------------------------------------------------------

T38 = subset(crab2,
             ProjID == "Creek Trawl" &
               Lifestage == "Total" &
               Watershed == "Cooper")

T38YSI = subset(T38, Salinity == "YSI")
T38Raw = subset(T38, Salinity == "Raw CSI")
T38CSI = subset(T38, Salinity == "24mo CSI")

#P-value for T38 YSI
T38YSI_lm = lm(T38YSI$CPUE~T38YSI$PSU)
summary(T38YSI_lm)

#P-value for T38 Raw CSI
T38Raw_lm = lm(T38Raw$CPUE~T38Raw$PSU)
summary(T38Raw_lm)

#P-value for 24 mo CSI
T38CSI_lm = lm(T38CSI$CPUE~T38CSI$PSU)
summary(T38CSI_lm)
# Plots -------------------------------------------------------------------

T38_YSI <- ggplot(aes(PSU, CPUE), data = subset(crab2, 
                                                ProjID == "Creek Trawl" &
                                                  Lifestage == "Total" &
                                                  Watershed == "Cooper" &
                                                  Salinity == "YSI")) +
  ggtitle("YSI") +
  geom_point() 
T38_YSI

T38_Raw <- ggplot(aes(PSU, CPUE), data = subset(crab2, 
                                                ProjID == "Creek Trawl" &
                                                  Lifestage == "Total" &
                                                  Watershed == "Cooper" &
                                                  Salinity == "Raw CSI")) +
  ggtitle("CSI Raw Data (Apr-Mar)") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="gray45") 
T38_Raw

T38_CSI <- ggplot(aes(PSU, CPUE), data = subset(crab2, 
                                                ProjID == "Creek Trawl" &
                                                  Lifestage == "Total" &
                                                  Watershed == "Cooper" &
                                                  Salinity == "24mo CSI")) +
  ggtitle("24 mo CSI (April-Mar)") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="gray45") 
T38_CSI


Fig_plot <- plot_grid(T38_YSI, T38_Raw, T38_CSI, nrow = 1)

Fig.plot2 <- grid.arrange(T38_YSI, T38_Raw, T38_CSI, nrow = 1)
#Fig_plot

#ggsave("Fig.pdf", width = 7, height = 3, dpi=600)
