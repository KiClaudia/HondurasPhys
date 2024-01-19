##########################################################################
#####                                                               ######
#####                Figure for glucose and site                    ######
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
  select("phys_ID", "sex", "glucose", "month_caught", "site","BCI") %>%
  na.omit
data2

p1 <- ggplot(data = data2, aes(x=site, y=glucose, fill = site)) +
  geom_boxplot() +  
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  labs(title="FIG.XX Glucose of female and male iguanas at each site",x="Site", y = "Glucose (mg/dL)")+
  theme_classic() +
  coord_cartesian(ylim=c(0,300)) +
  annotate("text", x = 1, y = 300, label = "ac", color = "red", size = 5) +
  annotate("text", x = 2, y = 300, label = "bc", color = "red", size = 5) +
  annotate("text", x = 3, y = 300, label = "c", color = "red", size = 5) 
p1

