# FEMALES ONLY: is follicle SIZE related to testosterone?
library(tidyverse)
library(ggpubr)
library(rcompanion)
library(Rfit)
install.packages("Rfit")

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithFemBCI.csv")
View(data)

fem <- data %>%
  filter(total_follicles != "0") %>% # taking out 0 follicles because those females were not reproductive
  mutate(avgfolsize = rowMeans(select(., size_left_cm ,size_right_cm)))
fem

#--------------- Does testosterone affect number of total follicles?----------------
fem1 <- fem %>%
  select("size_left_cm", "size_right_cm", "avgfolsize", "phys_ID", "t") %>%
  na.omit()
fem1

# 46 is an outlier
fem1 <- fem1 %>%
  filter(phys_ID != "46")
fem1

plot(fem1$avgfolsize~fem1$t) # meh

cor(fem1$avgfolsize,fem1$t) # 0

# normality
hist(log(fem1$avgfolsize)) #normal ish whenlogged
fem1$avgfolsize <- log(fem1$avgfolsize)
hist((fem1$t)) #normal

model <- lm(avgfolsize ~ t, data = fem1)
summary(model)
par(mfrow = c(2, 2))
plot(model)

# model does not fit assumptions, need to use nonparametric rank based regression
model.r = rfit(avgfolsize ~ t, data = fem1)
summary(model.r)

