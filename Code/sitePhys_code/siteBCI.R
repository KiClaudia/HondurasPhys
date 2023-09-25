library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggplot2)
library(viridis)
library(viridisLite)
install.packages("viridis")

# Is BCI related to site? How does month_caught play into this?

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithBothBCI.csv")
data$site <- as.factor(data$site)

# Filter data

data2 <- data %>%
  select("phys_ID", "sex", "BCI", "month_caught", "site") %>%
  na.omit
data2

# Visualize (red dots are may iguanas, black is april)

ggplot(data = data2, aes(x=sex, y = BCI)) +
  geom_point()

may <- data2 %>%
  filter(month_caught == "may")

ggplot(data = data2, aes(x=site, y=BCI, fill=site)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  geom_point(data = may, aes(x=site, y=BCI, fill=site), colour = "red") 
# Do there seem to be differences in sites? NO
# Do there seem to be differences in terms of month? NO


# Summary statistics (I don't think sex is different either)
data2 %>%
  group_by(sex) %>%
  get_summary_stats(BCI, type = "mean_se")

# Outliers
data2 %>% 
  group_by(site) %>%
  identify_outliers(BCI)

data3 <- data2 %>%
  filter(!phys_ID %in% c("42","43","49","52","51","69"))
data3

# Normality (not going to worry about month or sex)
model  <- lm(BCI ~ site, data = data3)
ggqqplot(residuals(model))
ggqqplot(data3, "BCI", facet.by = "site")

# Homogeneity of Variance
plot(model, 1)
data3 %>% levene_test(BCI ~ site)


# ANOVA
res.aov <- data3 %>% anova_test(BCI ~ site)
res.aov
