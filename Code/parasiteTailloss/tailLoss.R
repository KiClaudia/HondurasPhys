library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggplot2)
library(viridisLite)
library(viridis)
install.packages("viridis")

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithBothBCI.csv")
data$site <- as.factor(data$site)
data$sex <- as.character(data$sex)
data$sex <- as.factor(data$sex)
data$parasite.num <- as.factor(data$parasite.num)
data$month_caught <- as.factor(data$month_caught)
data$tail_break <- as.factor(data$tail_break)

#Filter data
data2 <- data %>%
  select("phys_ID", "sex", "parasite.num", "month_caught", "site","BCI", "SVL_mm", "mass_g", "tail_break") %>%
  na.omit
data2
str(data2)
#------------------- is tailbreak related to sex?---------------------
table(data2$sex, data2$tail_break)

# Chi-square test is for categorical variables and frequency
x <- chisq.test(data2$sex, data2$tail_break, correct=FALSE)
x
x$expected

#------------------- is tailbreak related to site?---------------------
table(data2$site, data2$tail_break)

site <- chisq.test(data2$site, data2$tail_break, correct=FALSE)
site
site$expected

# In order to tell which cells contributed most to the chi-square score, we can use below to see. highest absolute standard contributes most
library(corrplot)
corrplot(site$residuals, is.cor = FALSE)
round(site$residuals, 2)

#------------------ is tailbreak related to parasites?-----------------
table(data2$parasite.num, data2$tail_break)

tail <- chisq.test(data2$parasite.num, data2$tail_break, correct=FALSE)
tail
tail$expected

#------------------ is tailbreak related to month_caught?-----------------
table(data2$month_caught, data2$tail_break)

month <- chisq.test(data2$month_caught, data2$tail_break, correct=FALSE)
month

