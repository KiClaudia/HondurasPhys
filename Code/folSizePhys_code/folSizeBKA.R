
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
  select("avgfolsize", "bka", "site", "phys_ID", "month_caught") %>%
  drop_na()
fem1

plot(fem1$bka ~ fem1$avgfolsize) # negative corr? 

may <- fem %>%
  filter(month_caught == "may")
ggplot(fem1, aes(x=bka, y=avgfolsize)) + 
  geom_point() +
  geom_point(data=may, aes(x=bka, y=avgfolsize), colour="red", size = 3) 

# we need to take out may iguanas, seem to be driving negative relationship

fem2 <- fem1 %>%
  filter(month_caught == "april")

cor(fem2$avgfolsize, fem2$bka) # -0.3

# BKA requires beta distribution

fem2 <- fem2 %>%
  mutate(data = bka/100) # change to decimal 
View(fem2)

# betareg needs to be between 0 and 1 but not 0 and 1, so the package suggests you
# do this (y * (n-1) + 0.5) / n where n is the sample size.
# n = 12 

fem2 <- fem2 %>%
  mutate(beta = ((data*11)+0.5)/12)
View(fem2)
range(fem2$beta) # range is good
fem2 <- fem2 %>%
  mutate(folbeta = (avgfolsize/100))
fem2

betamodel <- betareg::betareg(fem2$beta ~ fem2$folbeta) 
summary(betamodel)

