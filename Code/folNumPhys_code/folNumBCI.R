# FEMALES ONLY: is follicle number related to BCI?
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

#--------------- Does BCI affect number of total follicles?----------------
fem1 <- fem %>%
  select("BCI_fem", "total_follicles", "site", "phys_ID", "month_caught")
fem1

plot(fem1$total_follicles~fem1$BCI_fem) # does not seem significant

cor(fem1$total_follicles,fem1$BCI_fem) # correlation close to 0

# data is not linear nor homoskedastic but normal
hist((fem1$total_follicles))
hist((fem1$BCI_fem))

model <- lm(total_follicles ~ BCI_fem * month_caught, data = fem1)
summary(model)
par(mfrow = c(2, 2))
plot(model)

# phys ID 52 is an outlier
fem1 <- fem1 %>%
  filter(phys_ID != "52")
fem1

# rerun model
model <- lm(total_follicles ~ BCI_fem * month_caught, data = fem1)
summary(model)
par(mfrow = c(2, 2))
plot(model)

# after removing outlier, assumptions look much better
# model is not significant. intercept is significant but it does not really mean anything (if BCI is 0, they should have 12 follicles?)