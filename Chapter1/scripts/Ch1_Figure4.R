library(dplyr)
library(tidyr)
library(ggplot2)
library(PerformanceAnalytics)
library(MuMIn)
library(MASS)
library(ggpmisc)
library(Hmisc)
library(nlme)
library(cowplot)

theme_set(theme_classic(base_size = 11)+
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_rect(colour = "black", size=0.5))+
            theme(text=element_text(size=11,  family="serif", colour = "black"))+
            theme(axis.text = element_text(colour = "black"))+
            theme(axis.ticks = element_line(colour = "black"))
)
update_geom_defaults("point",   list(colour = "black"))
update_geom_defaults("line",   list(colour = "black"))
theme_update(plot.title = element_text(hjust = 0.5))

# Read in data ------------------------------------------------------------

crab <- read.csv("./Chapter1/data/CH1data.csv", stringsAsFactors = FALSE)%>%
  mutate(E98_CPUE = log(E98_CPUE)) %>%
  mutate(E98_CPUE = if_else(is.infinite(E98_CPUE), 0.00000001, E98_CPUE)) %>%
  mutate(E98_JuvCPUE = log(E98_JuvCPUE)) %>%
  mutate(E98_JuvCPUE = if_else(is.infinite(E98_JuvCPUE), 0.00000001, E98_JuvCPUE)) %>%
  mutate(E98_SubadultCPUE = log(E98_SubadultCPUE)) %>%
  mutate(E98_SubadultCPUE = if_else(is.infinite(E98_SubadultCPUE), 0.00000001, E98_SubadultCPUE)) %>%
  mutate(E98_AdultCPUE = log(E98_AdultCPUE)) %>%
  mutate(E98_AdultCPUE = if_else(is.infinite(E98_AdultCPUE), 0.00000001, E98_AdultCPUE)) %>%
  mutate(E98_SublegalCPUE = log(E98_SublegalCPUE)) %>%
  mutate(E98_SublegalCPUE = if_else(is.infinite(E98_SublegalCPUE), 0.00000001, E98_SublegalCPUE))


P88Abun_lm9 <- lm(crab$P88_LegalCPUE ~ lag(crab$P88_LegalCPUE, 1))
E98AbunLead_lm8 <- lm(crab$E98_AdultCPUE ~ lag(crab$E98_SublegalCPUE, 2))
T38AbunLead_lm2 = lm(crab$T38_CPUE ~ lag(crab$T38_SubadultCPUE, 2))
summary(E98AbunLead_lm8)


E98AbunLeadPlot = ggplot(aes(y = E98_LegalCPUE, x = lag(crab$E98_SublegalCPUE, 2)), data = crab) +
  ggtitle(" ") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = E98AbunLead_lm8),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 1), sep = "")),
                  parse = TRUE, size = 3.2) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.12,
               formula = E98AbunLead_lm8,rr.digits = 2,  parse = TRUE, size = 3.2) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  xlim(0, 4) +
  labs(y=expression(Adult~CPUE~year^{-1}),
       x=expression(Sublegal~CPUE~"(2-yr. lag)"~year^{-1}))


T38AbunLeadPlot = ggplot(aes(y = T38_CPUE, x = lag(crab$T38_SubadultCPUE, 2)), data = crab) +
  ggtitle(" ") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = T38AbunLead_lm2),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 1), sep = "")),
                  parse = TRUE, size = 3.2) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.12,
               formula = T38AbunLead_lm2,rr.digits = 2,  parse = TRUE, size = 3.2) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(Total~CPUE~year^{-1}),
       x=expression(Subadult~CPUE~"(2-yr. lag)"~year^{-1}))



P88AbunLagPlot = ggplot(aes(y = P88_LegalCPUE, x = lag(crab$P88_LegalCPUE, 1)), data = crab) +
  ggtitle(" ") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = P88Abun_lm9),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 1), sep = "")),
                  parse = TRUE, size = 3.2) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.12,
               formula = P88Abun_lm9,rr.digits = 2,  parse = TRUE, size = 3.2) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  labs(y=expression(Adult~CPUE~year^{-1}),
       x=expression(Adult~CPUE~"(1-yr. lag)"~year^{-1}))


SingleRegressPlot <- plot_grid(E98AbunLeadPlot, T38AbunLeadPlot, P88AbunLagPlot, ncol = 1, align = 'hv', labels = 
                                 c("A", "B", "C")) 
SingleRegressPlot 




B90_lm <- lm(crab$LandingsCPUEMean ~ lag(crab$B90_MatureMaleCPUE, 1)) 
B90AbunPlot = ggplot(aes(y = LandingsCPUEMean, x = lag(crab$B90_MatureMaleCPUE, 1)), data = crab) +
  ggtitle(" ") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = B90_lm),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 1), sep = "")),
                  parse = TRUE, size = 3.2) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.12,
               formula = B90_lm,rr.digits = 2,  parse = TRUE, size = 3.2) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  xlim(0, 3) +
  ylim(2, 4) +
  labs(y=expression(Effort~corrected~landings~year^{-1}),
       x=expression(Mature~male~CPUE~"(1-yr. lag)"~year^{-1}))


T38_lm <- lm(crab$LandingsCPUEMean ~ lag(crab$T38_ImmatureMaleCPUE, 1)) 
T38AbunPlot = ggplot(aes(y = LandingsCPUEMean, x = lag(crab$T38_ImmatureMaleCPUE, 1)), data = crab) +
  ggtitle(" ") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = T38_lm),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 1), sep = "")),
                  parse = TRUE, size = 3.2) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.12,
               formula = T38_lm,rr.digits = 2,  parse = TRUE, size = 3.2) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  xlim(0, 3) +
  ylim(2, 4) +
  labs(y=expression(Effort~corrected~landings~year^{-1}),
       x=expression(Immature~male~CPUE~"(1-yr. lag)"~year^{-1}))


LandingsCPUERegressPlot <- plot_grid(B90AbunPlot, T38AbunPlot, ncol = 1, align = 'hv', labels = 
                                 c("A", "B")) 
LandingsCPUERegressPlot





P88Land_lm <- lm(crab$LandingsSum ~ lag(crab$P88_LegalCPUE, 2)) 
P88LandPlot = ggplot(aes(y = LandingsSum, x = lag(crab$P88_LegalCPUE, 2)), data = crab) +
  ggtitle(" ") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = P88Land_lm),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 1), sep = "")),
                  parse = TRUE, size = 3.2) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.12,
               formula = P88Land_lm,rr.digits = 2,  parse = TRUE, size = 3.2) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(y=expression(Total~lbs.~landed~year^{-1}),
       x=expression(Adult~CPUE~"(2-yr. lag)"~year^{-1}))


B90Land_lm <- lm(crab$LandingsSum ~ lag(crab$B90_MatureMaleCPUE, 1)) 
B90LandPlot = ggplot(aes(y = LandingsSum, x = lag(crab$B90_MatureMaleCPUE, 1)), data = crab) +
  ggtitle(" ") +
  geom_point() +
  stat_fit_glance(method = "lm", method.args = list(formula = B90Land_lm),
                  label.x = "right",
                  label.y = "bottom",
                  aes(label = paste("italic(P)*\"-value = \"*", 
                                    signif(..p.value.., digits = 1), sep = "")),
                  parse = TRUE, size = 3.2) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "right", label.y.npc = 0.12,
               formula = B90Land_lm,rr.digits = 2,  parse = TRUE, size = 3.2) +
  geom_smooth(method = 'lm', se=FALSE, color="black") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(y=expression(Effort~corrected~landings~year^{-1}),
       x=expression(Immature~male~CPUE~"(1-yr. lag)"~year^{-1}))


LandingsRegressPlot <- plot_grid(P88LandPlot, B90LandPlot, ncol = 1, align = 'hv', labels = 
                                       c("A", "B")) 
LandingsRegressPlot

