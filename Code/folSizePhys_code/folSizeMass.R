# FEMALES ONLY: is follicle SIZE related to mass?
library(tidyverse)
library(ggpubr)

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithFemBCI.csv")
View(data)
data$month_caught <- as.factor(data$month_caught)

fem <- data %>%
  filter(total_follicles != "0") %>% # taking out 0 follicles because those females were not reproductive
  mutate(avgfolsize = rowMeans(select(., size_left_cm ,size_right_cm)))
fem

#--------------- Does MAss affect number of total follicles?----------------
fem1 <- fem %>%
  select("size_left_cm", "size_right_cm", "avgfolsize", "phys_ID", "mass_g", "month_caught")
fem1

plot(fem1$avgfolsize~fem1$mass_g) # maybe positive corr?

may <- fem %>%
  filter(month_caught == "may")
ggplot(fem1, aes(x=mass_g, y=avgfolsize)) + 
  geom_point() +
  geom_point(data=may, aes(x=mass_g, y=avgfolsize), colour="red", size = 3) # is spread out so it is okay


cor(fem1$avgfolsize,fem1$mass_g) # 0.5, medium correlation

# normality
hist(log(fem1$avgfolsize)) #normal ish whenlogged
fem1$avgfolsize <- log(fem1$avgfolsize)
hist((fem1$mass_g)) #normalish

model <- lm(avgfolsize ~ mass_g, data = fem1)
summary(model)
par(mfrow = c(2, 2))
plot(model)

# assumptions okay

# significant