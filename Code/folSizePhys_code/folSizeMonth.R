# FEMALES ONLY: is follicle SIZE related to month caught?
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
  select("size_left_cm", "size_right_cm", "avgfolsize", "phys_ID", "month_caught") %>%
  na.omit()
fem1

plot(fem1$avgfolsize~fem1$month_caught) # maybe positive?

# normality
hist(log(fem1$avgfolsize)) #normal ish whenlogged
fem1$avgfolsize <- log(fem1$avgfolsize)

model <- lm(avgfolsize ~ month_caught, data = fem1)
summary(model)
par(mfrow = c(2, 2))
plot(model)




