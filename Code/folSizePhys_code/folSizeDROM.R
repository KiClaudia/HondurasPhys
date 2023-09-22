# FEMALES ONLY: is follicle SIZE related to drom?
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

#--------------- Does drom affect number of total follicles?----------------
fem1 <- fem %>%
  select("size_left_cm", "size_right_cm", "avgfolsize", "phys_ID", "drom", "month_caught") %>%
  na.omit()
fem1

plot(fem1$avgfolsize~fem1$drom) # negative?

may <- fem %>%
  filter(month_caught == "may")
ggplot(fem1, aes(x=avgfolsize, y=drom)) + 
  geom_point() +
  geom_point(data=may, aes(x=avgfolsize, y=drom), colour="red", size = 3) 
# looks like may iguana have really low dROm and may be driving this interaction (confounding month) so I will take out may iguanas

fem2 <- fem1 %>%
  filter(month_caught == "april") # taking out april iguanas
fem2

# normality
hist((fem2$avgfolsize)) #normal ish 
hist((fem2$drom)) #not normal

# model does not fit assumptions, need to use nonparametric rank based regression
model.r = rfit(drom ~ avgfolsize, data = fem2)
summary(model.r)

