# Reading the dataset into Rstudio
getwd()
rent <- read.csv("NEW_DATA.csv", header = TRUE, stringsAsFactors = FALSE) 
# representing the data in R
str(rent)
View(rent)
# Comparing the numeric values of rental prices of the year 2018 and the population of 2018 
rent$POPULATION <- as.numeric(rent$POPULATION)
# library lattice is to run the histogram
library(lattice)
# histogram is used to check whether the data is normally distributed or not
histogram(~POPULATION | 2018,data =  rent)

# normality test carried out by using the shapiro test   
normality_test <- shapiro.test(rent$POPULATION)
normality_test
normality_test <- shapiro.test(rent$X2018)
normality_test
# normality checking using q-q plot
qqnorm(rent$POPULATION)
qqline(rent$POPULATION, col = 'blue')
# library pwr and dplyr are used to perform the power analysis
library(pwr)
library(dplyr)
# to check the effective size 
effective_size <- cohen.ES(test = "r", size = "large")
effective_size


#power analaysis is to check the oprtimal sample sizes
power_analysis <- pwr.r.test(n= NULL, # observations in each group 
                             r = 0.5, sig.level = 0.05, # Type I probability
                             power = 0.95,
                             alternative = "two.sided")
power_analysis
# atleast 46 samples in each group are required to do the optimal test
plot(power_analysis)


# spearman test is stastical method which is used to find the relationship between the two continous variables
test <- cor.test(rent$X2018, rent$POPULATION, method = 'spearman', exact = FALSE)
test

#does the population has any effect with increase in the rent in 2018?
# p-value is greater than 0.05 which shows that there is no relation between population and rent
# so i have to conclude my research question as population has no effect on rent
