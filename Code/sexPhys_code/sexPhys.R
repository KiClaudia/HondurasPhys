# Does sex affect the phys variables? (only looking at 1 and 2, not at 3 juveniles)
# I am running a lm to see if there is a main effect of sex on phys
library(tidyverse)
library(rstatix)

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/HN2022physMaster.csv")
View(data)

str(data$sex)
data$sex <- as.factor(data$sex)

FMdata <- data %>%
  filter(sex != "3")
View(FMdata)

# Does sex play a role in dROM? YES
plot(FMdata$drom ~ FMdata$sex) # the intercept is *** but doesn't actually code for anything (we don't have a 0 sex (just 1 2)) so it doesn't mean anything even though it is significant
drom <- lm(FMdata$drom ~ FMdata$sex)
summary(drom)
  FMdata %>% 
    select(drom, sex) %>%
    group_by(sex) %>%
     get_summary_stats(type = c("mean_se"))

# Does sex play a role in BKA? YES
plot(FMdata$bka ~ FMdata$sex)
bka <- lm(FMdata$bka ~ FMdata$sex)
summary(bka)

# Does sex play a role in oxy? NO
plot(FMdata$oxy ~ FMdata$sex)
oxy <- lm(FMdata$oxy ~ FMdata$sex)
summary(oxy)
FMdata %>% 
  select(oxy, sex) %>%
  group_by(sex) %>%
  get_summary_stats(type = c("mean_se"))

# Does sex play a role in cort? NO
plot(FMdata$cort ~ FMdata$sex)
cort <- lm(FMdata$cort ~ FMdata$sex)
summary(cort)
FMdata %>% 
  select(cort, sex) %>%
  group_by(sex) %>%
  get_summary_stats(type = c("mean_se"))

# Does sex play a role in t? YES
plot(FMdata$t ~ FMdata$sex)
t <- lm(FMdata$t ~ FMdata$sex)
summary(t)

# Does sex play a role in glucose? NO
plot(FMdata$glucose ~ FMdata$sex)
t <- lm(FMdata$glucose ~ FMdata$sex)
summary(t)
FMdata %>% 
  select(glucose, sex) %>%
  group_by(sex) %>%
  get_summary_stats(type = c("mean_se"))
