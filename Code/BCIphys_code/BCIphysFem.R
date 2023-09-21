# FEMALES: does BCI affect phys measurements?
# I am running LM quickly without checking the assumptions to get a preliminary idea. I will check BCI again when I run the actual regression
data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithFemBCI.csv")
View(data) #BCI_fem is the column we want

# Does BCI play a role in dROM? NO
drom <- lm(data$drom ~ data$BCI_fem)
summary(drom)

# Does BCI play a role in BKA? NO
bka <- lm(data$bka ~ data$BCI_fem)
summary(bka)

# Does BCI play a role in oxy? NO
oxy <- lm(data$oxy ~ data$BCI_fem)
summary(oxy)

# Does BCI play a role in cort? NO
cort <- lm(data$cort ~ data$BCI_fem)
summary(cort)

# Does BCI play a role in t? NO
t <- lm(data$t ~ data$BCI_fem)
summary(t)
