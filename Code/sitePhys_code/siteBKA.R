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

# Visualize (red dots are may iguanas, black is april)---------------

plot(data = data2, BCI ~ bka)

ggplot(data = data2, aes(x=month_caught, y = bka)) +
  geom_point()

ggplot(data = data2, aes(x=sex, y = bka)) +
  geom_point()

may <- data2 %>%
  filter(month_caught == "may")
p <- ggplot(data = data2, aes(x=site, y=bka, fill=site), facet(facet.by = "sex")) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  geom_point(data = may, aes(x=site, y=bka, fill=site), colour = "red") 
facet(p, facet.by = "sex")

# Summary statistics -------------------
data2 %>%
  group_by(sex) %>%
  get_summary_stats(bka, type = "mean_se")
data2 %>%
  group_by(month_caught) %>%
  get_summary_stats(bka, type = "mean_se")

# T.test to see if there are differences in bka by SEX------------------
t.test(bka ~ sex, data = data2) # YES

# Separate out females and males-------------
fem <- data2 %>%
  filter(sex == "2")
mal <- data2 %>%
  filter(sex == "1")

# T.test to see if there are differences in t by MONTH-----------------
t.test(bka ~ month_caught, data = fem) # YES
t.test(bka ~ month_caught, data = mal) # NO

# separate out month for females
femapr <- fem %>%
  filter(month_caught == "april")
femmay <- fem %>%
  filter(month_caught == "may") #n=6

# Any correlations between BCI and bka?---------------------
cor(femapr$BCI,femapr$bka) 
cor(mal$BCI,mal$bka) 

# even though we usually do beta distribution, since predictor variable is factor, going to use kruskal wallis

# Kruskal-Wallis
kruskal.test(bka ~ site, data = femapr)
kruskal.test(bka ~ site, data = mal)

# summary
mal %>%
  group_by("site") %>%
  get_summary_stats(type = c("mean_se"))
femapr %>%
  group_by("site") %>%
  get_summary_stats(type = c("mean_se"))
