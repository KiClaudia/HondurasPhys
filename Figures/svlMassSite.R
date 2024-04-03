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

p1 <- ggplot(data = data2, aes(x=site, y=SVL_mm, fill = sex)) +
  geom_boxplot() +  
  labs(x="Study Locations", y = "Snout Vent Length (mm)", fill = "Sex")+
  scale_x_discrete(labels=c("Site 1", "Site 2", "Site 3")) +
  theme_classic() +
  coord_cartesian(ylim=c(0,400)) +
  annotate("text", x = 1, y = 400, label = "a", color = "red", size = 5) +
  annotate("text", x = 2, y = 365, label = "b", color = "red", size = 5) +
  annotate("text", x = 3, y = 322, label = "c", color = "red", size = 5) +
  scale_fill_discrete(name = "Sex", labels = c("Male", "Female"), 
  type = c("royalblue1", "salmon")) 
p1

# Mass v Sites---------------------------------------------

p2 <- ggplot(data = data2, aes(x=site, y=mass_g, fill = sex)) +
  geom_boxplot() +  
  labs(x="Study Locations", y = "Mass (g)", fill = "Sex") +
  scale_x_discrete(labels=c("Site 1", "Site 2", "Site 3")) +
  theme_classic() +
  coord_cartesian(ylim=c(0,2000)) +
  annotate("text", x = 1, y = 2000, label = "a", color = "red", size = 5) +
  annotate("text", x = 2, y = 1700, label = "b", color = "red", size = 5) +
  annotate("text", x = 3, y = 1400, label = "c", color = "red", size = 5) +
  scale_fill_discrete(name = "Sex", labels = c("Male", "Female"), 
                      type = c("royalblue1", "salmon")) 

p2

# Put figures together-----------------------------------------------
nested <- (p1/p2)+
  plot_annotation(tag_levels = 'A')
nested

pdf()
nested
dev.off()
