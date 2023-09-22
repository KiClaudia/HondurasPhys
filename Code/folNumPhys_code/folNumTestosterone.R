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

#--------------- Does Testosterone affect number of total follicles?----------------
fem1 <- fem %>%
  select("total_follicles", "t", "site", "phys_ID", "month_caught") %>%
  drop_na()
fem1

plot(fem1$total_follicles ~ fem1$t) # nothing 
may <- fem %>%
  filter(month_caught == "may")
ggplot(fem1, aes(x=t, y=total_follicles)) + 
  geom_point() +
  geom_point(data=may, aes(x=t, y=total_follicles), colour="red", size = 3) # is spread out so it is okay

cor(fem1$total_follicles, fem1$t) # no correlation

# 46 is an outlier
fem1 <- fem1 %>%
  filter(phys_ID != "46")

# Assumption checking
hist((fem1$total_follicles)) #normal
hist((fem1$t)) #  normal

# LM model
model <- lm(total_follicles ~ t, data = fem1)
summary(model)
par(mfrow = c(2, 2)) # assumptions okay since data is normal, no significance
plot(model)


