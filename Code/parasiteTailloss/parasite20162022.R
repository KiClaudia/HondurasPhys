# comparing 2016 and 2022 parasite data for gumbalimba
library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggplot2)
library(viridisLite)
library(viridis)

# Load in fecal and blood parasites---------------------------------------
fecal <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/originalData/2016fecalParasites.csv")
blood <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/originalData/2016bloodParasites.csv")
HN2022 <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/originalData/HN2022physMaster.csv")

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

ectodf <- HN2022 %>%
  rename(parasite = parasite.num)%>%
  rename(ID = phys_ID)%>%
  select("ID", "site", "parasite") %>%
  filter(site == "GL") %>%
  add_column(year = "2022")
View(ectodf)

# merge blood and ecto dataset ---------------------------------------------
bloodecto <- rbind(ectodf, blooddf)
View(bloodecto)

# Fecal parasites 2016 vs. Ectoparasites 2022
table(bloodecto$year, bloodecto$parasite)

xbloodecto <- chisq.test(bloodecto$year, bloodecto$parasite, correct=FALSE)
xbloodecto
xbloodecto$expected

# In order to tell which cells contributed most to the chi-square score, we can use below to see. highest absolute standard contributes most
library(corrplot)
corrplot(xbloodecto$residuals, is.cor = FALSE)
round(xbloodecto$residuals, 2)


# merge fecal and ecto dataset ---------------------------------------------
fecalecto <- rbind(ectodf, fecaldf)
View(fecalecto)

# Fecal parasites 2016 vs. Ectoparasites 2022
table(fecalecto$year, fecalecto$parasite)

xfecalecto <- chisq.test(fecalecto$year, fecalecto$parasite, correct=FALSE)
xfecalecto
xfecalecto$expected

# In order to tell which cells contributed most to the chi-square score, we can use below to see. highest absolute standard contributes most
corrplot(xfecalecto$residuals, is.cor = FALSE)
round(xfecalecto$residuals, 2)

# merge blood/fecal and ecto dataset ---------------------------------------------
all <- rbind(ectodf, fecaldf, blooddf)
View(all)

# Fecal/blood parasites 2016 vs. Ectoparasites 2022
table(all$year, all$parasite)

xall <- chisq.test(all$year, all$parasite, correct=FALSE)
xall
xall$expected

# In order to tell which cells contributed most to the chi-square score, we can use below to see. highest absolute standard contributes most
corrplot(xall$residuals, is.cor = FALSE)
round(xall$residuals, 2)
