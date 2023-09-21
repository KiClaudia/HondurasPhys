# FEMALES ONLY: Is follicle number affected by site? I.e. do certain sites have more iguanas with more follicles? biologically meaning are some sites further along in reproduction
library(tidyverse)
library(ggpubr)
library(rstatix)

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithFemBCI.csv")
View(data)
# we have 22 females reproductive and non-reproductive

fem <- data %>%
  select("phys_ID", "site", "total_follicles", "BCI_fem")
str(fem)
fem$site <- as.factor(fem$site)

# visualize
plot(fem$total_follicles ~ fem$site) # seems like there will be differences

# outliers
fem %>% 
  group_by(site) %>%
  identify_outliers(total_follicles) # no extreme outliers
fem %>%
  group_by(site) %>%
  identify_outliers(BCI_fem) #49 and 52 are extreme

# normality
model <- lm(total_follicles ~ site, data = fem)
ggqqplot(residuals(model)) # looks okay

# homogeneity of variance
plot(model, 1)
fem %>% levene_test(total_follicles ~ site) # looks good

# running the anova
anova <- fem %>% anova_test(total_follicles ~ site)
anova

# post hoc tukey
pwc <- fem %>% tukey_hsd(total_follicles ~ site)
pwc
