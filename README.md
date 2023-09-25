# HondurasPhys
analysis for honduras 2022 field samples collected from gumbalimba, meridian, and grand roatan (phys). 

Testing from github
Testing from rstudio

# Some questions we will attempt to answer:
1) Does BCI and sex affect any of the phys measures?
2) Correlate female follicle size and number with phys (separately)
3) Are there differences in phys measures based on site?
4) Are there differences in BCI/SV/MASS based on site?

# 9/18/2023
BCI script created, calculate BCI for females and males, created new data sheet based on it CSV.

BCIphysFem script created. use to check if BCI affects phys measures for female. didn't check assumptions, used this as prelimiary test. will check again when running actual regression for each phys variable

# 9/19/2023
BCIphysMal script created. same as above ^^

Summary ---> preliminary analysis tells us that BCI does not affect PHYS. Sex does affect phys for dROM, BKA, T (will need to do stats on those with the sexes separate)

folNumBCI script made to see correlations between follicle number and BCI, none
Now that we know BCI is not related to follicle number, we can ignore it for future models in the folnum series

for the folNum series, also looked at SVL, mass, drom, OXY, cort, T, bka, glucose

# 9/21/2023
folSizePhys_code folder made. all scripts in here will be looking at whether follicle size is related to phys or morphometric measures

Used linear regression for normal data. 

# 9/22/2023
We need to take into consideration month caught for the analysis we did
Checked for BCI fem and mal 
Checked month_caught for folSize series. look at script to see how, graphically.
Checked month_caught for folNum series. look at script to see how, graphically.
Checked month_caught and BCI for follicle site analysis
Editted BCI_code to make a new CSV that has BCI from both sexes
