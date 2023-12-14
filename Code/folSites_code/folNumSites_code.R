# FEMALES ONLY: Is follicle number affected by site? I.e. do certain sites have more iguanas with more follicles? biologically meaning are some sites further along in reproduction
library(tidyverse)
library(ggpubr)
library(rstatix)

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithFemBCI.csv")
View(data)
# we have 22 females reproductive and non-reproductive

fem <- data %>%
  select("phys_ID", "site", "total_follicles", "BCI_fem", "month_caught", "SVL_mm", "mass_g")
str(fem)
fem$site <- as.factor(fem$site)
fem$month_caught <- as.factor(fem$month_caught)

# how does month caught affect this? We know that in general, may iguanas have larger follicles
may <- fem %>%
  filter(month_caught == "may")
ggplot(fem, aes(x=site, y=total_follicles)) + 
  geom_point() +
  geom_point(data=may, aes(x=site, y=total_follicles), colour="red", size = 3) # Looks like may iguanas may be pulling GL down, outlier for GR

# take out May iguanas to avoid confounding factor
df <- fem %>%
  filter(month_caught == "april")

# visualize
plot(df$total_follicles ~ df$site) # seems like there will be differences

# outliers
df %>% 
  group_by(site) %>%
  identify_outliers(total_follicles) # no extreme outliers

# normality
model <- lm(total_follicles ~ site * BCI_fem, data = df)
ggqqplot(residuals(model)) # looks okay

# homogeneity of variance
plot(model, 1)
df %>% levene_test(total_follicles ~ site) # looks good

# running the anova
anova <- df %>% anova_test(total_follicles ~ site * BCI_fem)
anova

# post hoc tukey
pwc <- df %>% tukey_hsd(total_follicles ~ site)
pwc

# summary stats
df %>%
  select(site, total_follicles) %>%
  group_by(site) %>%
  get_summary_stats(type = "mean_se")

# does SVL affect this?
anova2 <- df %>% anova_test(total_follicles ~ site * SVL_mm)
anova2




# power test
library(pwr)
pwr.f2.test(u = 2, v = 2, f2 = 0.363, sig.level = 0.05, power = NULL)

anova3 <- df %>% anova_test(total_follicles ~ site * SVL_mm *BCI_fem)
anova3
# if I run the model with just site, I have an ~77% power, if I run it with a second fixed effect (BCI or SVL), reduces it to 63% and 51% respectively
# running a model with all 3 fixed effecs has power of 1%...