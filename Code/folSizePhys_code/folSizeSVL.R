# FEMALES ONLY: is follicle SIZE related to SVL?
library(tidyverse)
library(ggpubr)

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithFemBCI.csv")
View(data)

fem <- data %>%
  filter(total_follicles != "0") %>% # taking out 0 follicles because those females were not reproductive
  mutate(avgfolsize = rowMeans(select(., size_left_cm ,size_right_cm)))
fem

#--------------- Does svl affect number of total follicles?----------------
fem1 <- fem %>%
  select("size_left_cm", "size_right_cm", "avgfolsize", "phys_ID", "SVL_mm")
fem1

# 52 is outlier take out
fem1 <- fem1 %>%
  filter(phys_ID != "52") %>%
  print()


plot(fem1$avgfolsize~fem1$SVL_mm) # maybe positive corr?

cor(fem1$avgfolsize,fem1$SVL_mm) # 0.5, medium correlation

# normality
hist(log(fem1$avgfolsize)) #normal ish whenlogged
fem1$avgfolsize <- log(fem1$avgfolsize)
hist((fem1$SVL_mm)) #normalish

model <- lm(avgfolsize ~ SVL_mm, data = fem1)
summary(model)
par(mfrow = c(2, 2))
plot(model)

# assumptions okay

# significant