library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggplot2)
library(viridisLite)
library(viridis)
install.packages("viridis")

# Is dROM related to site? How does month_caught play into this?

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithBothBCI.csv")
data$site <- as.factor(data$site)
str(data)

# Filter data--------------

data2 <- data %>%
  select("phys_ID", "sex", "drom", "month_caught", "site","BCI") %>%
  na.omit
data2

# Visualize (red dots are may iguanas, black is april)---------------

plot(data = data2, BCI ~ drom)

ggplot(data = data2, aes(x=month_caught, y = drom)) +
  geom_point()

ggplot(data = data2, aes(x=sex, y = drom)) +
  geom_point()

may <- data2 %>%
  filter(month_caught == "may")
p <- ggplot(data = data2, aes(x=site, y=drom, fill=site), facet(facet.by = "sex")) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  geom_point(data = may, aes(x=site, y=drom, fill=site), colour = "red") 
facet(p, facet.by = "sex")

# Summary statistics -------------------
data2 %>%
  group_by(sex) %>%
  get_summary_stats(drom, type = "mean_se")
data2 %>%
  group_by(month_caught) %>%
  get_summary_stats(drom, type = "mean_se")

# T.test to see if there are differences in dROMs by SEX------------------
t.test(drom ~ sex, data = data2) # yes need to separate

# Create dataframe for females and males------------------
fem <- data2 %>%
  filter(sex == "2")
mal <- data2 %>%
  filter(sex == "1")

# T.test to see if there are differences in drom by MONTH-----------------
t.test(drom ~ month_caught, data = fem) #YES
t.test(drom ~ month_caught, data = mal) #NO

# Create dataframe for females by month-----------
femAPR <- fem %>%
  filter(month_caught == "april") #n=13
femMAY <- fem %>%
  filter(month_caught == "may") #n=5....not enough to do analysis

# Any correlations between BCI and dROM?---------------------
cor(mal$BCI,mal$drom) 
cor(femAPR$BCI,femAPR$drom) 

# Outliers-----------
femAPR %>% 
  group_by(site) %>%
  identify_outliers(drom)

mal %>% 
  group_by(site) %>%
  identify_outliers(drom)

# Normality ---------------
modelfem  <- lm(drom ~ site, data = femAPR)
ggqqplot(residuals(modelfem))
ggqqplot(femAPR, "drom", facet.by = "site")

modelmal  <- lm(drom ~ site, data = mal)
ggqqplot(residuals(modelmal))
ggqqplot(mal, "drom", facet.by = "site")

# Homogeneity of Variance (not homogeneous)-----------
plot(modelfem, 1)
femAPR %>% levene_test(drom ~ site)

plot(modelmal, 1)
mal %>% levene_test(drom ~ site)

# ANOVA-------------

res.aovf <- femAPR %>% anova_test(drom ~ site)
res.aovf
res.aovm <- mal %>% anova_test(drom ~ site)
res.aovm

ggplot(data = femAPR, aes(x=site, y = drom)) +
  geom_point()

ggplot(data = mal, aes(x=site, y = drom)) +
  geom_point()
