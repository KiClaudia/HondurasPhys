# FEMALES ONLY: Is follicle size affected by site? I.e. do certain sites have more iguanas with larger follicles? biologically meaning are some sites further along in reproduction
library(tidyverse)
library(ggpubr)
library(rstatix)
library(FSA)

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithFemBCI.csv")
View(data)
# we have 22 females reproductive and non-reproductive

fem <- data %>%
  mutate(avgfolsize = rowMeans(select(., size_left_cm ,size_right_cm))) %>%
  select("phys_ID", "site", "avgfolsize", "size_left_cm", "size_right_cm")
str(fem)
fem$site <- as.factor(fem$site)
fem

# visualize
plot(fem$avgfolsize ~ fem$site) # seems like there will be differences

# outliers
fem %>% 
  group_by(site) %>%
  identify_outliers(avgfolsize) # no extreme outliers

# normality
model <- lm(avgfolsize ~ site, data = fem)
ggqqplot(residuals(model)) # not normal

# transform data to make normal, log(0) is undefined so inflate and then log
hist((fem$avgfolsize))
fem$avgfolsize <- (log(fem$avgfolsize + 0.001))

# homogeneity of variance
plot(model, 1)
fem %>% levene_test(avgfolsize ~ site) # not great but okay

# we cANNOT run anova as the data is normal and we do not meet all the assumptions. the homogeneity is also not great. we will run kruskal wallis

# kruskal wallis test

kmodel <- kruskal.test(fem$avgfolsize ~ fem$site)
kmodel

# post hoc Dunn test
dunnTest(avgfolsize ~ site,
         data = fem,
         method = "holm"
)
