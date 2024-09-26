library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggplot2)
library(viridisLite)
library(viridis)
install.packages("viridis")

# Is BKA related to site? How does month_caught play into this?

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithBothBCI.csv")
data$site <- as.factor(data$site)
data$sex <- as.character(data$sex)
data$sex <- as.factor(data$sex)
str(data)

# Filter data--------------

data2 <- data %>%
  select("phys_ID", "sex", "bka", "month_caught", "site","BCI") %>%
  na.omit
data2


# Separate out females and males-------------
fem <- data2 %>%
  filter(sex == "2")
mal <- data2 %>%
  filter(sex == "1")

# separate out month for females---------------
femapr <- fem %>%
  filter(month_caught == "april")
femmay <- fem %>%
  filter(month_caught == "may") #n=6

# Visualize (red dots are may iguanas, black is april)---------------

ggplot(data = data2, aes(x=site, y=bka, fill = sex)) +
  geom_boxplot() +  
  labs(x="Study Locations", y = "Bacterial Killing Ability", fill = "Sex") +
  scale_fill_discrete(name = "Sex", labels = c("Male", "Female"), 
                      type = c("royalblue1", "salmon")) +
  scale_x_discrete(labels=c("Site 1", "Site 2", "Site 3")) +
  theme_classic() +
  coord_cartesian(ylim=c(0,100)) +
  facet_grid(.~month_caught)

ggplot(data = data2, aes(x=site, y=bka, fill = site)) +
  geom_boxplot() +  
  labs(x="Study Locations", y = "Bacterial Killing Ability", fill = "site") +
  scale_x_discrete(labels=c("Site 1", "Site 2", "Site 3")) +
  theme_classic() +
  coord_cartesian(ylim=c(0,100)) 
 
