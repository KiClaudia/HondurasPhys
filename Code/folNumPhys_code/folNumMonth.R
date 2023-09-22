# FEMALES ONLY: is follicle NUMBER related to month caught?
library(tidyverse)
library(ggpubr)
library(rcompanion)
library(Rfit)
install.packages("Rfit")

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithFemBCI.csv")
View(data)
data$month_caught <- as.factor(data$month_caught)

fem <- data %>%
  filter(total_follicles != "0") # taking out 0 follicles because those females were not reproductive

fem

#--------------- Does cort affect number of total follicles?----------------
fem1 <- fem %>%
  select("total_follicles", "phys_ID", "month_caught") %>%
  na.omit()
fem1

plot(fem1$total_follicles~fem1$month_caught) 

# normality
hist((fem1$total_follicles)) #normal 

model <- lm(total_follicles ~ month_caught, data = fem1)
summary(model)
par(mfrow = c(2, 2))
plot(model)




