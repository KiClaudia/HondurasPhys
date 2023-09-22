# FEMALES ONLY: is follicle SIZE related to OXY?
library(tidyverse)
library(ggpubr)
library(rcompanion)
library(Rfit)
install.packages("Rfit")

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithFemBCI.csv")
View(data)
data$month_caught <- as.factor(data$month_caught)

fem <- data %>%
  filter(total_follicles != "0") %>% # taking out 0 follicles because those females were not reproductive
  mutate(avgfolsize = rowMeans(select(., size_left_cm ,size_right_cm)))
fem

#--------------- Does oxy affect number of total follicles?----------------
fem1 <- fem %>%
  select("size_left_cm", "size_right_cm", "avgfolsize", "phys_ID", "oxy", "month_caught") %>%
  na.omit()
fem1

plot(fem1$avgfolsize~fem1$oxy) # maybe positive?

may <- fem %>%
  filter(month_caught == "may")
ggplot(fem1, aes(x=avgfolsize, y=oxy)) + 
  geom_point() +
  geom_point(data=may, aes(x=avgfolsize, y=oxy), colour="red", size = 3) # seems similar to other OXY points
cor(fem1$avgfolsize,fem1$oxy) # .1

# normality
hist(log(fem1$avgfolsize)) #normal ish whenlogged
fem1$avgfolsize <- log(fem1$avgfolsize)
hist((fem1$oxy)) #normal

model <- lm(oxy ~ avgfolsize, data = fem1)
summary(model)
par(mfrow = c(2, 2))
plot(model)

# model does not fit assumptions, need to use nonparametric rank based regression
model.r = rfit(oxy ~ avgfolsize, data = fem1)
summary(model.r)

