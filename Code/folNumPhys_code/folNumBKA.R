# FEMALES ONLY: Now that we know BCI is not related to follicle number, we can ignore it for future models

library(tidyverse)
library(ggpubr)
library(betareg)

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithFemBCI.csv")
View(data)

data$sex <- as.factor(data$sex)

fem <- data %>%
  filter(sex == "2") %>%
  filter(total_follicles != "0") # taking out 0 follicles because those females were not reproductive
View(fem)

#--------------- Does BKA affect number of total follicles?----------------
fem1 <- fem %>%
  select("total_follicles", "bka", "site", "phys_ID", "month_caught") %>%
  drop_na()
fem1

fem1$month_caught <- as.factor(fem1$month_caught)
plot(fem1$total_follicles ~ fem1$bka) # nothing 

cor(fem1$total_follicles, fem1$bka) # no correlation

# BKA requires beta distribution

fem1 <- fem1 %>%
  mutate(data = bka/100) # change to decimal 
View(fem1)

# betareg needs to be between 0 and 1 but not 0 and 1, so the package suggests you
# do this (y * (n-1) + 0.5) / n where n is the sample size.
# n = 15 

fem1 <- fem1 %>%
  mutate(beta = ((data*14)+0.5)/15)
View(fem1)
range(fem1$beta) # range is good
fem1 <- fem1 %>%
  mutate(folbeta = (total_follicles/100))
fem1

betamodel <- betareg::betareg(fem1$folbeta ~ fem1$beta + fem1$month_caught)
summary(betamodel)

# predictor BKA is not significant even though intercept is which doesnt mean anythin (when bka is 0, they have -1 follicles)