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
  select("phys_ID", "site", "avgfolsize", "size_left_cm", "size_right_cm", "month_caught", "BCI_fem", "SVL_mm")
str(fem)
fem$month_caught <- as.factor(fem$month_caught)
fem$site <- as.factor(fem$site)
fem

# how does month caught affect this? We know that in general, may iguanas have larger follicles
may <- fem %>%
  filter(month_caught == "may")
ggplot(fem, aes(x=site, y=avgfolsize)) + 
  geom_point() +
  geom_point(data=may, aes(x=site, y=avgfolsize), colour="red", size = 3) # Looks like may iguanas may be pulling GL up

# Remove May iguanas to prevent confounding factor of month to play a role
df <- fem %>%
  filter(month_caught == "april")

# visualize
plot(df$avgfolsize ~ df$site) # seems like there will be differences

# outliers
df %>% 
  group_by(site) %>%
  identify_outliers(avgfolsize) # no extreme outliers

# how does BCI play a role in this (i.e. does meridian just have smaller iguanas so they have smaller follicles?)
ggplot(df, aes(x = site, y = BCI_fem)) +
  geom_point() # Meridian does have the 2 females with the lowest BCI BUT other iguanas at Meridian who have way higher BCI still have similar follicle sizes
ggplot(df, aes(x = site, y = SVL_mm)) +
  geom_point() # Meridian has smaller iguanas


# normality
model <- lm(avgfolsize ~ site * BCI_fem, data = df)
ggqqplot(residuals(model)) # not normal

# transform data to make normal, log(0) is undefined so inflate and then log
hist(log(df$avgfolsize))
df$avgfolsize <- (log(df$avgfolsize + 0.001))

# homogeneity of variance
plot(model, 1)
df %>% levene_test(avgfolsize ~ site) # not great but okay

# we cANNOT run anova as the data is normal and we do not meet all the assumptions. the homogeneity is also not great. we will run kruskal wallis

# kruskal wallis test

kmodel <- kruskal.test(df$avgfolsize ~ df$site)
kmodel

# post hoc Dunn test
dunnTest(avgfolsize ~ site,
         data = df,
         method = "holm"
)

