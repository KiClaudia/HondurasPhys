##########################################################################
#####                                                               ######
##### Figure for dROMs  for sex and month                           ######
#####                                                               ######
##########################################################################

library(patchwork)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(RColorBrewer)

# Load data-------------------------------------------------------
data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithBothBCI.csv")

View(data)
# plot---------------------------------------------
str(data)
data$sex <- as.character(data$sex)

p1 <- ggplot(data, aes(x=sex, y=drom, fill = month_caught)) + 
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  labs(x="Sex", y = "dROM (H2O2 mg/dL)")+
  scale_x_discrete(labels=c("Male", "Female")) +
  theme_classic() +
  coord_cartesian(ylim=c(0,20)) +
  scale_fill_discrete(name = "Month", labels = c("April", "May"), 
                      type = c("green", "purple")) 

p1

pdf()
nested
dev.off()
