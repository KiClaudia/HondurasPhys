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
data$month_caught <- as.factor(data$month_caught)


fem <- data %>%
  filter(sex == "2") %>%
  filter(total_follicles != "0") # taking out 0 follicles because those females were not reproductive
View(fem)

#--------------- Does OXY affect number of total follicles?----------------
fem1 <- fem %>%
  select("total_follicles", "oxy", "site", "phys_ID", "month_caught") %>%
  drop_na()
fem1

plot(fem1$total_follicles ~ fem1$oxy) # nothing 

may <- fem %>%
  filter(month_caught == "may")
ggplot(fem1, aes(x=oxy, y=total_follicles)) + 
  geom_point() +
  geom_point(data=may, aes(x=oxy, y=total_follicles), colour="red", size = 3) # is spread out so it is okay

cor(fem1$total_follicles, fem1$oxy) # no correlation

# Assumption checking
hist((fem1$total_follicles)) #normal
hist((fem1$oxy)) # normal

# LM model
model <- lm(total_follicles ~ oxy + month_caught, data = fem1)
summary(model)
par(mfrow = c(2, 2)) # data does not meet assumptions, cannot use LM, tried to log dROm does not work
plot(model)

# Rank based estimation regression uses estimators and inference that that are robust to outliers. used in lm like situations or anova like situaions
model.r = rfit(total_follicles ~ oxy + month_caught, data = fem1)
summary(model.r)
