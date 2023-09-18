# Calculating body condition index BCI using SVL and mass. Run linear regression
# and use residuals as BCI. (running male and females separate, not including unknown sex)

library(tidyverse)

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/HN2022physMaster.csv")
View(data)
data$sex <- as.factor(data$sex)

# separate out females and males
fem <- data %>%
  filter(sex == "2") 
View(fem)
 
mal <- data %>%
  filter(sex == "1")
View(mal)

# run lm and find the equation y=mx+b from model, see model residuals
femmod <- lm(fem$SVL_mm ~ fem$mass_g)
summary(femmod) # as mass increases, svl increases
resid(femmod)
fembci <- resid(femmod)
plot(fem$SVL_mm ~ fem$mass_g)

# since residuals above do not come with ID, we can calculate residuals 
# ourselves using this formula: y=mx+b (this gives us calculated residual, y is SVL, x is mass)
# actual measured SVL - calculated SVL = residual, double check with above residuals
fem <- fem %>%
  mutate(calcFemSVL = ((mass_g*0.20323)+106.91556)) %>%
  mutate(BCI_fem = SVL_mm - calcFemSVL)

View(fem)
# write csv
write.csv(fem, "C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithFemBCI.csv")


# same steps as above
malmod <- lm(mal$SVL_mm ~ mal$mass_g)
summary(malmod) # as mass increases, svl increases
resid(malmod)
malbci <- resid(malmod)
plot(mal$SVL_mm ~ mal$mass_g)

mal <- mal %>%
  mutate(calcmalSVL = ((mass_g*0.1286)+147)) %>%
  mutate(BCI_mal = SVL_mm - calcmalSVL)
View(mal)

write.csv(mal, "C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithMalBCI.csv")
