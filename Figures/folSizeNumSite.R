##########################################################################
#####                                                               ######
##### Figure for follicle size and follicle number compared to site ######
#####                                                               ######
##########################################################################

library(patchwork)
library(ggplot2)
library(tidyverse)
library(ggpubr)

# Load data-------------------------------------------------------
data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithFemBCI.csv")

fem <- data %>%
  mutate(avgfolsize = rowMeans(select(., size_left_cm ,size_right_cm))) %>%
  select("phys_ID", "site", "avgfolsize", "size_left_cm", "size_right_cm", "month_caught", "BCI_fem", "SVL_mm", "mass_g", "total_follicles") %>%
  filter(month_caught == "april") %>%
  filter(!phys_ID %in% c("15","23"))
  # remove 15 and 23 because they were juveniles and remove may iguanas (because we did in the analysis)
View(fem)

# Follicle number v Sites---------------------------------------------

p1 <- ggplot(fem, aes(x=site, y=total_follicles, fill = site)) + 
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  labs(title="FIG.XX Follicle number of iguanas at each site",x="Site", y = "Total Follicle Number")+
  theme_classic() +
  coord_cartesian(ylim=c(0,20)) +
  annotate("text", x = 1, y = 20, label = "a", color = "red", size = 5)+
  annotate("text", x = 2, y = 15, label = "b", color = "red", size = 5)+
  annotate("text", x = 3, y = 10, label = "c", color = "red", size = 5)
p1
# Follicle size v Sites---------------------------------------------

p2 <- ggplot(fem, aes(x=site, y=avgfolsize, fill = site)) + 
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  labs(title="FIG.XX Average follicle size of iguanas at each site",x="Site", y = "Average Follicle Size (cm)")+
  theme_classic() +
  coord_cartesian(ylim=c(0,2.5)) +
  annotate("text", x = 1, y = 2.5, label = "a", color = "red", size = 5)+
  annotate("text", x = 2, y = 2.5, label = "a", color = "red", size = 5)+
  annotate("text", x = 3, y = 2, label = "b", color = "red", size = 5)
p2  
# Put figures together-----------------------------------------------
nested <- (p1/p2)+
  plot_annotation(tag_levels = 'A')
nested
