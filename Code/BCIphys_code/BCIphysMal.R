# MALES: does BCI affect phys measurements?
# I am running LM quickly without checking the assumptions to get a preliminary idea. I will check BCI again when I run the actual regression
data <- read.csv("C:/Users/claud/OneDrive - USU/Desktop/Ctenosaura oedirhina/Honduras trip 2022/HN2022analysis/HondurasPhys/workingdata/masterWithMalBCI.csv")
View(data) #BCI_mal is the column we want

# Does BCI play a role in dROM? NO
plot(data$drom ~ data$BCI_mal)
drom <- lm(data$drom ~ data$BCI_mal)
summary(drom)

# Does BCI play a role in BKA? NO
plot(data$bka ~ data$BCI_mal)
bka <- lm(data$bka ~ data$BCI_mal)
summary(bka)

# Does BCI play a role in oxy? NO
oxy <- lm(data$oxy ~ data$BCI_mal)
summary(oxy)

# Does BCI play a role in cort? NO
cort <- lm(data$cort ~ data$BCI_mal)
summary(cort)

# Does BCI play a role in t? NO
t <- lm(data$t ~ data$BCI_mal)
summary(t)

# Does BCI play a role in glucose? NO
glu <- lm(data$glucose ~ data$BCI_mal)
summary(glu)

# Is BCI affected by month caught? NO
month <- aov(data$BCI_mal ~ data$month_caught)
summary(month)

str(data$month_caught)
data$month_caught <- as.factor(data$month_caught)
