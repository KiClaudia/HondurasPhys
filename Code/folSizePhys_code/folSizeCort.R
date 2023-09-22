# FEMALES ONLY: is follicle SIZE related to CORT?
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

#--------------- Does cort affect number of total follicles?----------------
fem1 <- fem %>%
  select("size_left_cm", "size_right_cm", "avgfolsize", "phys_ID", "cort", "month_caught") %>%
  na.omit()
fem1

# 29 32 is an outlier
fem1 <- fem1 %>%
  filter(phys_ID != "29") %>%
  filter(phys_ID != "32")
fem1

plot(fem1$avgfolsize~fem1$cort) # maybe positive?

may <- fem %>%
  filter(month_caught == "may")
ggplot(fem1, aes(x=cort, y=avgfolsize)) + 
  geom_point() +
  geom_point(data=may, aes(x=cort, y=avgfolsize), colour="red", size = 3) 
# if anything cort would be driven to be positive by may iguanas but the model is still n.s. so we can leave it

cor(fem1$avgfolsize,fem1$cort) # .3

# normality
hist(log(fem1$avgfolsize)) #normal ish whenlogged
fem1$avgfolsize <- log(fem1$avgfolsize)
hist((fem1$cort)) #normalish

model <- lm(cort ~ avgfolsize, data = fem1)
summary(model)
par(mfrow = c(2, 2))
plot(model)

# model does not fit assumptions, need to use nonparametric rank based regression
model.r = rfit(cort ~ avgfolsize, data = fem1)
summary(model.r)


