library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggplot2)
library(viridis)
library(viridisLite)
install.packages("viridis")

# Is SVL related to site? How does month_caught play into this?

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithBothBCI.csv")
data$site <- as.factor(data$site)

# Filter data

data2 <- data %>%
  select("phys_ID", "sex", "SVL_mm", "month_caught", "site") %>%
  na.omit
data2

# Visualize (red dots are may iguanas, black is april)

ggplot(data = data2, aes(x=sex, y = SVL_mm)) +
  geom_point()

may <- data2 %>%
  filter(month_caught == "may")

p <- ggplot(data = data2, aes(x=site, y=SVL_mm, fill=site), facet(facet.by = "sex")) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  geom_point(data = may, aes(x=site, y=SVL_mm, fill=site), colour = "red") 
facet(p, facet.by = "sex")
# Do there seem to be differences in sites? YES
# Do there seem to be differences in terms of month? NO


# Summary statistics (Need to do SEX separately)
data2 %>%
  group_by(sex) %>%
  get_summary_stats(SVL_mm, type = "mean_se")

# Create dataframe for females and males
fem <- data2 %>%
  filter(sex == "2")
mal <- data2 %>%
  filter(sex == "1")

# Outliers
fem %>% 
  group_by(site) %>%
  identify_outliers(SVL_mm)

mal %>% 
  group_by(site) %>%
  identify_outliers(SVL_mm)

fem2 <- fem %>%
  filter(!phys_ID %in% c("56"))
fem2

mal2 <- mal %>%
  filter(!phys_ID %in% c("51", "69"))
mal2

# Normality (not going to worry about month or sex)
modelfem  <- lm(SVL_mm ~ site, data = fem2)
ggqqplot(residuals(modelfem))
ggqqplot(fem2, "SVL_mm", facet.by = "site")

modelmal  <- lm(SVL_mm ~ site, data = mal2)
ggqqplot(residuals(modelmal))
ggqqplot(mal2, "SVL_mm", facet.by = "site")

# Homogeneity of Variance (not homogeneous)
plot(modelfem, 1)
fem2 %>% levene_test(SVL_mm ~ site)

plot(modelmal, 1)
mal2 %>% levene_test(SVL_mm ~ site)

# Cannot do ANOVA because data is slightly skewed and variance not homogeneous

# Kruskal-Wallis
kruskal.test(SVL_mm ~ site, data = fem2)
kruskal.test(SVL_mm ~ site, data = mal2)

# Wilcox pairwise post hoc
pairwise.wilcox.test(fem2$SVL_mm, fem2$site)
pairwise.wilcox.test(mal2$SVL_mm, mal2$site)


