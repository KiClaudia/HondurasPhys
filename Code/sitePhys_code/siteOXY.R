library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggplot2)
library(viridisLite)
library(viridis)
install.packages("viridis")

# Is OXY related to site? How does month_caught play into this?

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithBothBCI.csv")
data$site <- as.factor(data$site)
data$sex <- as.character(data$sex)
data$sex <- as.factor(data$sex)
str(data)

# Filter data--------------

data2 <- data %>%
  select("phys_ID", "sex", "oxy", "month_caught", "site","BCI") %>%
  na.omit
data2

# Visualize (red dots are may iguanas, black is april)---------------

plot(data = data2, BCI ~ oxy)

ggplot(data = data2, aes(x=month_caught, y = oxy)) +
  geom_point()

ggplot(data = data2, aes(x=sex, y = oxy)) +
  geom_point()

may <- data2 %>%
  filter(month_caught == "may")
p <- ggplot(data = data2, aes(x=site, y=oxy, fill=site), facet(facet.by = "sex")) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  geom_point(data = may, aes(x=site, y=oxy, fill=site), colour = "red") 
facet(p, facet.by = "sex")

# Summary statistics -------------------
data2 %>%
  group_by(sex) %>%
  get_summary_stats(oxy, type = "mean_se")
data2 %>%
  group_by(month_caught) %>%
  get_summary_stats(oxy, type = "mean_se")

# T.test to see if there are differences in oxy by SEX------------------
t.test(oxy ~ sex, data = data2) # no

# T.test to see if there are differences in oxy by MONTH-----------------
t.test(oxy ~ month_caught, data = data2) #no

# Any correlations between BCI and OXY?---------------------
cor(data2$BCI,data2$oxy) 

# Outliers-----------
data2 %>% 
  group_by(site) %>%
  identify_outliers(oxy)

# Normality ---------------
modelfem  <- lm(oxy ~ site, data = data2)
ggqqplot(residuals(modelfem))
ggqqplot(data2, "oxy", facet.by = "site")

# Homogeneity of Variance (not homogeneous)-----------
plot(modelfem, 1)
data2 %>% levene_test(oxy ~ site)

# ANOVA-------------

res.aov <- data2 %>% anova_test(oxy ~ site)
res.aov

ggplot(data = data2, aes(x=site, y = oxy)) +
  geom_point()
