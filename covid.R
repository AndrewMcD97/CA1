# Reading In The Covid World Progression .csv Into
# A Dataframe
covidProgression <- read.csv("covid.csv", na="")

# Checking The Structure Of Our Dataset
summary(covidProgression)
str(covidProgression)

# Checking The First Few Lines Of Data To Get A Better
# Idea Of What Im Working With
head(covidProgression)

# Making A Smaller Dataframe With The Variables Needed
# To Answer The Research Questions
covidData <- subset(covidProgression, select = c(3,4,11,14,40,46,47,53,58,59))
head(covidData)

# The Variables Are : total_cases_per_million, total_vaccinations_per_hundred,total_deaths_per_million
# human_development_index, median_age ,diabetes_prevalence ,population_density ,life_expectancy ,location

# Check Variable Types
str(covidData)

# We need to convert date from chr to Date
covidData$date <- as.Date(covidData$date, "%Y-%m-%d")

# We only want the latest date available to us which is 27/04/21
covidData <- covidData[covidData$date == "2021-04-27", ]

# Check correlations between variables using the psyche package 
install.packages('psych')
library(psych)
pairs.panels(covidData,  
             smooth = TRUE, # If TRUE, draws loess smooths    
             scale = FALSE, # If TRUE, scales the correlation text font
             density = TRUE, # If TRUE, adds density plots and histograms
             ellipses = TRUE, # If TRUE, draws ellipses 
             method = "spearman", # Correlation method also (also "pearson" or "kendall")   
             pch = 21, # pch symbol 
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS smoothed fit 
             cor = TRUE, # If TRUE, reports correlations 
             jiggle = FALSE, # If TRUE, data points are jittered 
             factor = 2, # Jittering factor
             hist.col = 4, # Histograms color
             stars = TRUE, # If TRUE, adds significance level with stars
             ci = TRUE) # If TRUE, adds confidence intervals   


# -------------------------------------------------------------
# Research Question 1 - Is there any link between the total cases in a 
# country (total_cases_per_million)and its life expectancy at birth (life_expectancy)?
#
# Null (H0) – The total cases in a country per million does not have a correlation with 
# the life expectancy of a country at birth
#
# Alternative (H1) – The total cases in a country per million does have a correlation 
# with the life expectancy of the country at birth
# -------------------------------------------------------------

# No conversions of varible type need to take place

# The linearity or correlation of the two variables will now be found
attach(covidData)
qqplot(life_expectancy, total_cases_per_million, 
     pch=20, main="Comparison Of Life Expectancy With Cases Per Million In Each Country",
     xlab="Life Expectancy (In Years)",
     ylab="Total Cases (Per Million)")

# Time to visualize the normality of our two varibales
opar = par(no.readonly = TRUE)
par(opar)
# And chart them side by side
par(mfrow = c(1,2))
hist(life_expectancy, col='green', main = "Distribution Of Life Expectancy")
hist(total_cases_per_million, col='green', main = "Distribution Of Total Cases (Per Mil)")

# Create a QQ-plot of the variables and add
# A normality line to further evaluate normality
qqnorm(life_expectancy, main="Normal QQ-Plot of Life Expectancy Data")
qqline(life_expectancy, col="red")
qqnorm(total_cases_per_million, main="Normal QQ-Plot of Total Cases (Per Mil) Data")
qqline(total_cases_per_million, col="red")

# -------------------------------------------------------------
# Research Question 2 -  Does population density (population_density) in a 
# country have any effect on the total cases per million population (total_cases_per_million)?
#
# Null (H0) – The number of people per square kilometer in a country does not have a 
# correlation with the total number of cases per million people in a country
#
# Alternative (H1) – The number of people per square Km in a country has a correlation 
# with the total cases per million people in a country
# -------------------------------------------------------------

# No conversions of variable type need to take place

# The linearity or correlation of the two variables will now be found
attach(covidData)
qqplot(population_density, total_cases_per_million, 
       pch=20, main="Comparison Of Population Density (Per Sq Km) With Cases Per Million In Each Country",
       xlab="Population Density (Per Sq Km)",
       ylab="Total Cases (Per Million)",
       xlim=c(0,1000))

# Time to visualize the normality of our two varibales
opar = par(no.readonly = TRUE)
par(opar)
# And chart them side by side
par(mfrow = c(1,2))
hist(population_density, col='green', main = "Distribution Of Population Density")
hist(total_cases_per_million, col='green', main = "Distribution Of Total Cases (Per Mil)")

# Create a QQ-plot of the variables and add
# A normality line to further evaluate normality
qqnorm(population_density, main="Normal QQ-Plot of Population Density Data")
qqline(population_density, col="red")
qqnorm(total_cases_per_million, main="Normal QQ-Plot of Total Cases (Per Mil) Data")
qqline(total_cases_per_million, col="red")

# -------------------------------------------------------------
# Research Question 3 -   Does the Human development index (human_development_index ) 
# in a country have any bearing upon the rate that people are being vaccinated at (total_vaccinations_per_hundred)?
#
# Null (H0) – The average standard of living in a country has no bearing on the amount of total vaccinations per 
# hundred people in a country
#
# Alternative (H1) – The standard of living in a country being higher results in a higher number of total 
# vaccinations per hundred people at the present rate.
# -------------------------------------------------------------

# No conversions of variable type need to take place

# The linearity or correlation of the two variables will now be found
attach(covidData)
qqplot(human_development_index, total_vaccinations_per_hundred, 
       pch=20, main="Comparison Of Human Development Index With Total Vaccines Per Hundred In Each Country",
       xlab="Human Development Index %",
       ylab="Total Vaccines (Per Hundred)")

# Time to visualize the normality of our two variables
opar = par(no.readonly = TRUE)
par(opar)
# And chart them side by side
par(mfrow = c(1,2))
hist(human_development_index, col='green', main = "Distribution Of Human Dev Index")
hist(total_vaccinations_per_hundred, col='green', main = "Distribution Of Total Vaccines (Per Hundred)")

# Create a QQ-plot of the variables and add
# A normality line to further evaluate normality
qqnorm(human_development_index, main="Normal QQ-Plot of HDI")
qqline(human_development_index, col="red")
qqnorm(total_vaccinations_per_hundred, main="Normal QQ-Plot of Total Vax (Per Hundred)")
qqline(total_vaccinations_per_hundred, col="red")

# -------------------------------------------------------------
# Research Question 4 -  Does the median age (median_age) of a country have any 
# correlation with the total deaths per million (total_deaths_per_million) in a country?
#
# Null (H0) – The median age value of a country has no reflection upon the total 
# deaths per million in a specific country.
#
# Alternative (H1) – The higher the median age value of a country’s population the 
# higher the amount of total deaths per million due to Covid-19. 
# -------------------------------------------------------------

# No conversions of variable type need to take place

# The linearity or correlation of the two variables will now be found
attach(covidData)
qqplot(median_age, total_deaths_per_million, 
       pch=20, main="Comparison Of Median Age With Deaths Per Million In Each Country",
       xlab="Median Age",
       ylab="Total Deaths (Per Million)")

# Time to visualize the normality of our two variables
opar = par(no.readonly = TRUE)
par(opar)
# And chart them side by side
par(mfrow = c(1,2))
hist(median_age, col='green', main = "Distribution Of Median Age")
hist(total_deaths_per_million, col='green', main = "Distribution Of Total Deaths (Per Mil)")

# Create a QQ-plot of the variables and add
# A normality line to further evaluate normality
qqnorm(median_age, main="Normal QQ-Plot of Median Age Data")
qqline(median_age, col="red")
qqnorm(total_deaths_per_million, main="Normal QQ-Plot of Total Deaths (Per Mil) Data")
qqline(total_deaths_per_million, col="red")

# -------------------------------------------------------------
# Research Question 5 -  Is there a link between the prevalence of diabetes in a 
# country (diabetes_prevalence) and the number of Covid cases per million (total_cases_per_million)?
#
# Null (H0) – The percentage of the population in a specific country with diabetes has no effect 
# on the number of covid cases per million in a country.
#
# Alternative (H1) – The higher presence of diabetes in a specific country results in a higher 
# number of covid cases per million
# -------------------------------------------------------------

# No conversions of variable type need to take place

# The linearity or correlation of the two variables will now be found
attach(covidData)
qqplot(diabetes_prevalence, total_cases_per_million, 
       pch=20, main="Comparison Of Diabetes Prevalence With Cases Per Million In Each Country",
       xlab="Diabetes Prevalence %",
       ylab="Total Cases (Per Million)")

# Time to visualize the normality of our two varibales
opar = par(no.readonly = TRUE)
par(opar)
# And chart them side by side
par(mfrow = c(1,2))
hist(diabetes_prevalence, col='green', main = "Distribution Of Diabetes Prevalence")
hist(total_cases_per_million, col='green', main = "Distribution Of Total Cases (Per Mil)")

# Create a QQ-plot of the variables and add
# A normality line to further evaluate normality
qqnorm(diabetes_prevalence, main="Normal QQ-Plot of Diabetes Data")
qqline(diabetes_prevalence, col="red")
qqnorm(total_cases_per_million, main="Normal QQ-Plot of Total Cases (Per Mil) Data")
qqline(total_cases_per_million, col="red")

# ----------------------------------------------------------------

# TESTING ALL THE QUESTIONS

# -----------------------------------------------------------------
# Q1 - Spearmans
cor.test(life_expectancy, total_cases_per_million,
         method="spearman")

# Q2 - Spearmans
cor.test(population_density, total_cases_per_million,
         method="spearman")

# Q3 - Spearmans
cor.test(human_development_index, total_vaccinations_per_hundred,
         method="spearman")

# Q4 - Spearmans
cor.test(median_age, total_deaths_per_million,
         method="spearman")

# Q5 - Spearmans
cor.test(diabetes_prevalence, total_cases_per_million,
         method="spearman")