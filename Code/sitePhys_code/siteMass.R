library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggplot2)
library(viridis)
library(viridisLite)
install.packages("viridis")

# Is MASS related to site? How does month_caught play into this?

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithBothBCI.csv")
data$site <- as.factor(data$site)

# Filter data

data2 <- data %>%
  select("phys_ID", "sex", "mass_g", "month_caught", "site") %>%
  na.omit
head(data2)

# Visualize (red dots are may iguanas, black is april)

ggplot(data = data2, aes(x=sex, y = mass_g)) +
  geom_point()

may <- data2 %>%
  filter(month_caught == "may")

p <- ggplot(data = data2, aes(x=site, y=mass_g, fill=site), facet(facet.by = "sex")) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  geom_point(data = may, aes(x=site, y=mass_g, fill=site), colour = "red") 
facet(p, facet.by = "sex")
# Do there seem to be differences in sites? YES
# Do there seem to be differences in terms of month? NO
# Do there seem to be differences in sex? YES

# Summary statistics (Need to do SEX separately)
data2 %>%
  group_by(sex) %>%
  get_summary_stats(mass_g, type = "mean_se")

# Create dataframe for females and males
fem2 <- data2 %>%
  filter(sex == "2")
mal2 <- data2 %>%
  filter(sex == "1")

# Outliers
fem2 %>% 
  group_by(site) %>%
  identify_outliers(mass_g)

mal2 %>% 
  group_by(site) %>%
  identify_outliers(mass_g)

# Normality (not going to worry about month or sex)
modelfem  <- lm(mass_g ~ site, data = fem2)
ggqqplot(residuals(modelfem))
ggqqplot(fem2, "mass_g", facet.by = "site")

modelmal  <- lm(mass_g ~ site, data = mal2)
ggqqplot(residuals(modelmal))
ggqqplot(mal2, "mass_g", facet.by = "site")

# Homogeneity of Variance (not homogeneous)
plot(modelfem, 1)
fem2 %>% levene_test(mass_g ~ site)

plot(modelmal, 1)
mal2 %>% levene_test(mass_g ~ site)

# ANOVA

res.aovf <- fem2 %>% anova_test(mass_g ~ site)
res.aovf
res.aovm <- mal2 %>% anova_test(mass_g ~ site)
res.aovm

# Tukey Post hoc
fem2 %>% tukey_hsd(mass_g ~ site)
mal2 %>% tukey_hsd(mass_g ~ site)


