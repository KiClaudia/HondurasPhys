# FEMALES ONLY: Now that we know BCI is not related to follicle number, we can ignore it for future models

library(tidyverse)
library(ggpubr)
install.packages("mblm")
library(mblm)
library(rcompanion)
library(Rfit)
install.packages("Rfit")

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithFemBCI.csv")
View(data)

data$sex <- as.factor(data$sex)

fem <- data %>%
  filter(sex == "2") %>%
  filter(total_follicles != "0") # taking out 0 follicles because those females were not reproductive
View(fem)

#--------------- Does dROM affect number of total follicles?----------------
fem1 <- fem %>%
  select("total_follicles", "drom", "site", "phys_ID") %>%
  drop_na()
fem1

plot(fem1$total_follicles ~ fem1$drom) # nothing 

cor(fem1$total_follicles, fem1$drom) # no correlation

# Assumption checking
hist((fem1$total_follicles)) #normal
hist((fem1$drom)) # normal

# LM model
model <- lm(total_follicles ~ drom, data = fem1)
summary(model)
par(mfrow = c(2, 2)) # data does not meet assumptions, cannot use LM, tried to log dROm does not work
plot(model)

# Kendall-Theil Sen Siegel nonparametric linear repgression is for 1 x and 1 y, uses lines between pair of points and uses median of slopes of these lines
modelk= mblm(total_follicles ~ drom, data = fem1)
summary(modelk)

# Rank based estimation regression uses estimators and inference that that are robust to outliers. used in lm like situations or anova like situaions
model.r = rfit(total_follicles ~ drom, data = fem1)
summary(model.r)
