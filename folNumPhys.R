# FEMALES ONLY: is follicle number related to any of the phys measures?
library(tidyverse)

data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/HN2022physMaster.csv")
View(data)

data$sex <- as.factor(data$sex)

fem <- data %>%
  filter(sex == "2") 
View(fem)

fem1 <- fem %>%
  select("SVL_mm", "total_follicles", "site", "phys_ID")
fem1
