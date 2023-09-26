library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggplot2)
library(viridisLite)
library(viridis)
install.packages("viridis")

# Is Testosterone related to site? How does month_caught play into this?

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithBothBCI.csv")
data$site <- as.factor(data$site)
data$sex <- as.character(data$sex)
data$sex <- as.factor(data$sex)
str(data)

# Filter data--------------

data2 <- data %>%
  select("phys_ID", "sex", "t", "month_caught", "site","BCI") %>%
  na.omit
data2

# Visualize (red dots are may iguanas, black is april)---------------

plot(data = data2, BCI ~ t)

ggplot(data = data2, aes(x=month_caught, y = t)) +
  geom_point()

ggplot(data = data2, aes(x=sex, y = t)) +
  geom_point()

may <- data2 %>%
  filter(month_caught == "may")
p <- ggplot(data = data2, aes(x=site, y=t, fill=site), facet(facet.by = "sex")) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  geom_point(data = may, aes(x=site, y=t, fill=site), colour = "red") 
facet(p, facet.by = "sex")

# Summary statistics -------------------
data2 %>%
  group_by(sex) %>%
  get_summary_stats(t, type = "mean_se")
data2 %>%
  group_by(month_caught) %>%
  get_summary_stats(t, type = "mean_se")

# T.test to see if there are differences in t by SEX------------------
t.test(t ~ sex, data = data2) # no

# separate out males and females
mal <- data2 %>%
  filter(sex == "1")
fem <- data2 %>%
  filter(sex == "2")

# T.test to see if there are differences in t by MONTH-----------------
t.test(t ~ month_caught, data = mal) #YES
t.test(t ~ month_caught, data = fem) #no

# separte out males by month
malAPR <- mal %>%
  filter(month_caught == "april")
malMAY <- mal %>%
  filter(month_caught == "may")

# Any correlations between BCI and t?---------------------
cor(fem$BCI,fem$t) 
cor(malAPR$BCI,malAPR$t) 
cor(malMAY$BCI,malMAY$t) 

# Outliers-----------
fem %>% 
  group_by(site) %>%
  identify_outliers(t)
malAPR %>% 
  group_by(site) %>%
  identify_outliers(t)
malMAY %>% 
  group_by(site) %>%
  identify_outliers(t)

# Filter out outliers
fem2 <- fem %>%
  filter(!phys_ID %in% c(46))
fem2

# Normality ---------------
modelfem  <- lm(t ~ site, data = fem2)
ggqqplot(residuals(modelfem))
ggqqplot(fem2, "t", facet.by = "site")
hist(fem2$t)

modelmalapr  <- lm(t ~ site, data = malAPR)
ggqqplot(residuals(modelmalapr))
ggqqplot(malAPR, "t", facet.by = "site")

modelmalmay  <- lm(t ~ site, data = malMAY)
ggqqplot(residuals(modelmalmay))
ggqqplot(malMAY, "t", facet.by = "site")

# Homogeneity of Variance ----------
plot(modelfem, 1)
fem2 %>% levene_test(t ~ site)
plot(modelmalapr, 1)
malAPR %>% levene_test(t ~ site)
plot(modelmalmay, 1)
modelmalmay %>% levene_test(t ~ site)

# ANOVA-------------
res.aov <- fem %>% anova_test(t ~ site)
res.aov
res.aovapr <- malAPR %>% anova_test(t ~ site)
res.aovapr
res.aovmay <- malMAY %>% anova_test(t ~ site)
res.aovmay

boxplot(t ~ site, data = fem)
boxplot(t ~ site, data = malAPR)
boxplot(t ~ site, data = malMAY)
