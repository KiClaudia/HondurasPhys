# FEMALES ONLY: is follicle SIZE related to glucose?
library(tidyverse)
library(ggpubr)

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithFemBCI.csv")
View(data)

fem <- data %>%
  filter(total_follicles != "0") %>% # taking out 0 follicles because those females were not reproductive
  mutate(avgfolsize = rowMeans(select(., size_left_cm ,size_right_cm)))
fem

#--------------- Does glucose affect number of total follicles?----------------
fem1 <- fem %>%
  select("size_left_cm", "size_right_cm", "avgfolsize", "phys_ID", "glucose")
fem1

plot(fem1$avgfolsize~fem1$glucose) # maybe negative weak corr?

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

# significant