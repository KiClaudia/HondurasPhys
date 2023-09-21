# FEMALES ONLY: is follicle SIZE related to BCI?
library(tidyverse)
library(ggpubr)

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithFemBCI.csv")
View(data)

fem <- data %>%
  filter(total_follicles != "0") %>% # taking out 0 follicles because those females were not reproductive
  mutate(avgfolsize = rowMeans(select(., size_left_cm ,size_right_cm)))
fem

#--------------- Does BCI affect number of total follicles?----------------
fem1 <- fem %>%
  select("size_left_cm", "size_right_cm", "avgfolsize", "phys_ID", "BCI_fem")
fem1

plot(fem1$avgfolsize~fem1$BCI_fem) # does not seem significant

cor(fem1$avgfolsize,fem1$BCI_fem) # correlation close to 0

# 52 is outlier take out
fem1 <- fem1 %>%
  filter(phys_ID != "52") %>%
  print()

# normality
hist(log(fem1$avgfolsize)) #normal ish whenlogged
fem1$avgfolsize <- log(fem1$avgfolsize)
hist((fem1$BCI_fem)) #normalish

model <- lm(avgfolsize ~ BCI_fem, data = fem1)
summary(model)
par(mfrow = c(2, 2))
plot(model)

# assumptions okay

# model is not significant. intercept is significant but it does not really mean anything (if BCI is 0, they should have follicle size of 0.43cm)