##########################################################################
#####                                                               ######
##### Figure for site~SVL and follicle number compared to site ######
#####                                                               ######
##########################################################################

library(patchwork)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggplot2)
library(viridis)
library(viridisLite)
# Load data-------------------------------------------------------
data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithBothBCI.csv")

data2 <- data %>%
  select("phys_ID", "sex", "SVL_mm", "month_caught", "site", 'mass_g') %>%
  mutate(sexName = sex)
  na.omit
data2
data2$sex <- as.factor(data2$sex)
str(data2)

# SVL v Sites---------------------------------------------

p1 <- ggplot(data = data2, aes(x=site, y=SVL_mm, color = sex)) +
  geom_boxplot() +  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  labs(title="FIG.XX SVL of female and male iguanas at each site",x="Site", y = "Snout Vent Length (mm)")+
  theme_classic() +
  coord_cartesian(ylim=c(0,400)) +
  scale_fill_discrete(labels = c("Male", "Female")) +
  annotate("text", x = 1, y = 400, label = "a", color = "black", size = 5) +
  annotate("text", x = 2, y = 365, label = "b", color = "black", size = 5) +
  annotate("text", x = 3, y = 322, label = "c", color = "black", size = 5) 
p1

# Mass v Sites---------------------------------------------

p2 <- ggplot(data = data2, aes(x=site, y=mass_g, color = sex)) +
  geom_boxplot() +  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  labs(title="FIG.XX Mass of female and male iguanas at each site",x="Site", y = "Mass (g)")+
  theme_classic() +
  coord_cartesian(ylim=c(0,2000)) +
  scale_fill_discrete(labels = c("Male", "Female")) +
  annotate("text", x = 1, y = 2000, label = "a", color = "black", size = 5) +
  annotate("text", x = 2, y = 1700, label = "b", color = "black", size = 5) +
  annotate("text", x = 3, y = 1400, label = "c", color = "black", size = 5) 
p2

# Put figures together-----------------------------------------------
nested <- (p1/p2)+
  plot_annotation(tag_levels = 'A')
nested
