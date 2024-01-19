# 2016 parasite trends

library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggplot2)
library(viridisLite)
library(viridis)

# Load in fecal and blood parasites---------------------------------------
fecal <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/originalData/2016fecalParasites.csv")
blood <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/originalData/2016bloodParasites.csv")

# select relevant columns------------------------------------------------
fecaldf <- fecal %>%
  rename(site = location) %>%
  rename(parasite = fecal_parasites) %>%
  select("site", "ID" ,"parasite") %>%
  filter(site == "GL") %>%
  filter(!row_number() %in% c(18:22)) %>%
  add_column(year = "2016")
View(fecaldf) 

blooddf <- blood %>%
  rename(site = Location) %>%
  rename(parasite = blood_parasites) %>%
  select("site", "ID", "parasite") %>%
  filter(site == "GL") %>%
  add_column(year = "2016")
View(blooddf)

#-------------------what is the proportion of iguanas at gumbalimba that have fecal parasites?-----------------

#2/17 = 11% has fecal parasites

#-------------------what is the proportion of iguanas at gumbalimba that have blood parasites?-----------------

#4/14 = 28% has blood parasites

