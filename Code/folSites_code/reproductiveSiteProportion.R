# FEMALES ONLY: What are the proportions of reproductive females at each site? 0 = not reproductive 1+ follicles = reproductive
library(tidyverse)
library(ggpubr)
library(rstatix)

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithFemBCI.csv")
View(data)
# we have 22 females reproductive and non-reproductive

fem <- data %>%
  select("phys_ID", "site", "total_follicles", "BCI_fem", "month_caught", "SVL_mm")
str(fem)
fem$site <- as.factor(fem$site)
fem$month_caught <- as.factor(fem$month_caught)

# replace NA with 0
fem2 <- fem %>%
  replace_na(list(total_follicles = 0))
fem2

# make column using binary Y/N for whether iguana was reproductive (Yes for yes reproductive, no for not)

fem3 <- fem2 %>%
  mutate(reproductive = if_else(total_follicles == "0", "No", "Yes"), .keep = "all")
fem3

# Look at table 
table(fem3$site, fem3$reproductive)

# Chi-square test is for categorical variables and frequency
x <- chisq.test(fem3$site, fem3$reproductive, correct=FALSE)
x
x$expected

library(corrplot)
corrplot(x$residuals, is.cor = FALSE)
round(x$residuals, 2)
