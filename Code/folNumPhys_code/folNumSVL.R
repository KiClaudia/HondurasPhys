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

#--------------- Does SVL affect number of total follicles?----------------
fem1 <- fem %>%
  select("total_follicles", "SVL_mm", "site", "phys_ID", "month_caught")
fem1

plot(fem1$total_follicles ~ fem1$SVL_mm) # seems like as SVL increase, total follicles increase
may <- fem %>%
  filter(month_caught == "may")
ggplot(fem1, aes(x=SVL_mm, y=total_follicles)) + 
  geom_point() +
  geom_point(data=may, aes(x=SVL_mm, y=total_follicles), colour="red", size = 3) 
# May iguanas do not seem to have a pattern

cor(fem1$total_follicles, fem1$SVL_mm) # correlation close to 1, positive association strong

# Assumption checking
hist((fem1$total_follicles)) #normal
hist((fem1$SVL_mm)) # phys ID 52 is an outlier
fem1 <- fem1 %>%
  filter(phys_ID != "52")
fem1

# model
model <- lm(total_follicles ~ SVL_mm+ month_caught, data = fem1)
summary(model)
par(mfrow = c(2, 2)) # data looks homoskedastic (scale-loc is pretty horizontal) and linear(res fit no pattern) and normal (qq)
plot(model)

# significant, as svl increases, follicles increase