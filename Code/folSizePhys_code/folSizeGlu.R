# FEMALES ONLY: is follicle SIZE related to glucose?
library(tidyverse)
library(ggpubr)

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithFemBCI.csv")
View(data)
data$month_caught <- as.factor(data$month_caught)

fem <- data %>%
  filter(total_follicles != "0") %>% # taking out 0 follicles because those females were not reproductive
  mutate(avgfolsize = rowMeans(select(., size_left_cm ,size_right_cm)))
fem

#--------------- Does glucose affect number of total follicles?----------------
fem1 <- fem %>%
  select("size_left_cm", "size_right_cm", "avgfolsize", "phys_ID", "glucose", "month_caught")
fem1

plot(fem1$avgfolsize~fem1$glucose) # maybe negative weak corr?

may <- fem %>%
  filter(month_caught == "may")
ggplot(fem1, aes(x=glucose, y=avgfolsize)) + 
  geom_point() +
  geom_point(data=may, aes(x=glucose, y=avgfolsize), colour="red", size = 3) 
# looks like may iguanas have lower glucose levels but even without the red points, looks like there would not be a relationship anyway so it does not change anything

cor(fem1$avgfolsize,fem1$glucose) # meh

# normality
hist((fem1$avgfolsize)) #normal ish whenlogged
fem1$avgfolsize <- log(fem1$avgfolsize)
hist((fem1$glucose)) #normalish

model <- lm(avgfolsize ~ glucose, data = fem1)
summary(model)
par(mfrow = c(2, 2))
plot(model)

# assumptions okay

