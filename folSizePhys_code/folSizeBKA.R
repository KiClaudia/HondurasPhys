
library(tidyverse)
library(ggpubr)
library(betareg)
library(lmtest)

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithFemBCI.csv")
View(data)

data$sex <- as.factor(data$sex)

fem <- data %>%
  filter(sex == "2") %>%
  filter(total_follicles != "0") %>% # taking out 0 follicles because those females were not reproductive
  mutate(avgfolsize = rowMeans(select(., size_left_cm ,size_right_cm)))
fem
#--------------- Does BKA affect number of total follicles?----------------
fem1 <- fem %>%
  select("avgfolsize", "bka", "site", "phys_ID") %>%
  drop_na()
fem1

plot(fem1$avgfolsize ~ fem1$bka) # negative corr? 

cor(fem1$avgfolsize, fem1$bka) # -0.5

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
  mutate(folbeta = (avgfolsize/100))
fem1

plot(fem1$folbeta ~ fem1$beta)

betamodel <- betareg::betareg(fem1$folbeta ~ fem1$beta)
summary(betamodel)

betamodel <- betareg::betareg(fem1$beta ~ fem1$folbeta) # this is makes more sense biologically
summary(betamodel)

# what about the overall model, is it significant compared to a null moel?
null <- betareg::betareg(fem1$beta ~ 1)
summary(null)

lrt <- lrtest(null, betamodel)
lrt
we 