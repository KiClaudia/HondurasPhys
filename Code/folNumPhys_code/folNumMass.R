# FEMALES ONLY: Now that we know BCI is not related to follicle number, we can ignore it for future models

library(tidyverse)
library(ggpubr)

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithFemBCI.csv")
View(data)

data$sex <- as.factor(data$sex)
data$month_caught <- as.factor(data$month_caught)


fem <- data %>%
  filter(sex == "2") %>%
  filter(total_follicles != "0") # taking out 0 follicles because those females were not reproductive
View(fem)

#--------------- Does mass affect number of total follicles?----------------
fem1 <- fem %>%
  select("total_follicles", "mass_g", "site", "phys_ID", "month_caught")
fem1

plot(fem1$total_follicles ~ fem1$mass_g) 

cor(fem1$total_follicles, fem1$mass_g) # correlation close to 1, positive association strong

# Assumption checking
hist((fem1$total_follicles)) #normal
hist((fem1$mass_g)) # normal

# model
model <- lm(total_follicles ~ mass_g + month_caught, data = fem1)
summary(model)
par(mfrow = c(2, 2)) # data looks homoskedastic (scale-loc is pretty horizontal) and linear(res fit no pattern) and normal (qq)
plot(model)

# significant