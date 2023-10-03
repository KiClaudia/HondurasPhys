library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggplot2)

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithBothBCI.csv")
data$site <- as.factor(data$site)
data$sex <- as.character(data$sex)
data$sex <- as.factor(data$sex)
data$month_caught <- as.factor(data$month_caught)
data$tail_break <- as.factor(data$tail_break)

#Filter data
data2 <- data %>%
  select("phys_ID", "sex", "tail_break", "month_caught", "site","cort", "SVL_mm", "mass_g", "tail_break", "BCI", "drom", "oxy", "bka") %>%
  na.omit 
data2
str(data2)

#------ Do iguanas with and without tails have differences in BCI?-------------
boxplot(BCI ~ tail_break, data = data2)
var.test(BCI ~ tail_break, data = data2) # not equal
model <- lm(BCI ~ tail_break, data = data2)
ggqqplot(residuals(model)) # not normal

wilcox.test(BCI ~ tail_break, data = data2, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)

#------ Do iguanas with and without tails have differences in SVL?-------------
boxplot(SVL_mm ~ tail_break, data = data2)
var.test(SVL_mm ~ tail_break, data = data2) # not equal
model <- lm(SVL_mm ~ tail_break, data = data2)
ggqqplot(residuals(model)) # normal

t.test(SVL_mm ~ tail_break, data = data2, var.equal = FALSE)

#------ Do iguanas with and without tails have differences in MASS?-------------
boxplot(mass_g ~ tail_break, data = data2)
var.test(mass_g ~ tail_break, data = data2) # equal
model <- lm(mass_g ~ tail_break, data = data2)
ggqqplot(residuals(model)) # normal

t.test(mass_g ~ tail_break, data = data2, var.equal = TRUE)  

#------ Do iguanas with and without tails have differences in CORT?-------------
boxplot(cort ~ tail_break, data = data2)
var.test(cort ~ tail_break, data = data2) #not equal
model <- lm(cort ~ tail_break, data = data2)
ggqqplot(residuals(model)) # not normal

wilcox.test(cort ~ tail_break, data = data2, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)

#------ Do iguanas with and without tails have differences in DROM?-------------
boxplot(drom ~ tail_break, data = data2)
var.test(drom ~ tail_break, data = data2) # equal
model <- lm(drom ~ tail_break, data = data2)
ggqqplot(residuals(model)) # not normal

wilcox.test(drom ~ tail_break, data = data2, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)

#------ Do iguanas with and without tails have differences in OXY?-------------
boxplot(oxy ~ tail_break, data = data2)
var.test(oxy ~ tail_break, data = data2) # equal
model <- lm(oxy ~ tail_break, data = data2)
ggqqplot(residuals(model)) #  normal

t.test(oxy ~ tail_break, data = data2, var.equal = TRUE)

#------ Do iguanas with and without tails have differences in bka?-------------
# need to convert BKA so it is not a decimal
data3 <- data2 %>%
  mutate(decimal = bka/100)
data3

boxplot(decimal ~ tail_break, data = data3)
var.test(decimal ~ tail_break, data = data3) # equal
model <- lm(decimal ~ tail_break, data = data3)
ggqqplot(residuals(model)) #  not normal

wilcox.test(decimal ~ tail_break, data = data3, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
