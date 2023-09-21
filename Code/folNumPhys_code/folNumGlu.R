# FEMALES ONLY: is follicle number related to glucose?
library(tidyverse)
library(ggpubr)

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithFemBCI.csv")
View(data)

data$sex <- as.factor(data$sex)

fem <- data %>%
  filter(sex == "2") %>%
  filter(total_follicles != "0") # taking out 0 follicles because those females were not reproductive
View(fem)

#--------------- Does glucose affect number of total follicles?----------------
fem1 <- fem %>%
  select("glucose", "total_follicles", "site", "phys_ID")
fem1

plot(fem1$total_follicles~fem1$glucose) # does not seem significant

cor(fem1$total_follicles,fem1$glucose) # correlation close to 0

# data is not linear nor homoskedastic but normal
hist((fem1$total_follicles))
hist((fem1$glucose))

model <- lm(total_follicles ~ glucose, data = fem1)
summary(model)
par(mfrow = c(2, 2))
plot(model)


# model is not significant. intercept is significant but it does not really mean anything (if glucose is 0, they should have 12 follicles?)