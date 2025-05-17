library(tidyverse)
library(rstatix)
library(ggpubr)
library(ggplot2)
library(viridisLite)
library(viridis)
install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device="win") 
fonts() 

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

#------------------- % of parasites infested at each site---------------------
table(data2$site, data2$parasite.num)

data3 <- data.frame(x = c("Site 1", "Site 2", "Site 3"), y = c(1.0, 0.57, 0.78))
data3
str(data3)

p1 <- ggplot(data=data3, aes(x=x, y=y, fill = x)) +
  geom_bar(stat="identity") +
  labs(x="Study Locations", y = "Percent of Iguanas Infested") +
  theme_minimal() +
  theme_classic() +
  theme(text=element_text(family="Times New Roman", size=16)) +
  scale_fill_brewer(palette="Dark2") +
  geom_text(aes(label=(y)), vjust=1.6, color="black", size=5) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "none") 
  
pdf()
p1
dev.off()
#------------------- % of Tailbreak at each site---------------------
table(data2$site, data2$tail_break)

data4 <- data.frame(x = c("Site 1", "Site 2", "Site 3"), y = c(0.75, 0.80, 0.53))

p2 <- ggplot(data=data4, aes(x=x, y=y, fill = x)) +
  geom_bar(stat="identity") +
  labs(x="Study Locations", y = "Percent of Iguanas with Tail Break") +
  theme_minimal() +
  theme_classic() +
  theme(text=element_text(family="Times New Roman", size=16))+
  scale_fill_brewer(palette="Dark2") +
  geom_text(aes(label=(y)), vjust=1.6, color="black", size=5) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "none") 
 

pdf()
p2
dev.off()
