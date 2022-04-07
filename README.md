# Credit-Card-Expenses
####################### Installation of R and RStudio ##########################
# (editor comment symbol in R is # )

# Download R software from http://cran.r-project.org/bin/windows/base/
# Run the R set up (exe) file and follow instructions
# Double click on the R icon in the desktop and R window will open
# Download RStudio from http://www.rstudio.com/
# Run R studio set up file and follow instructions
# Click on R studio icon, R Studio IDE Studio will load
# Go to R-Script (Ctrl + Shift + N)
# Write 'Hello World'
# Save & Run (Ctrl + Enter)

################################################################################

'Hello World'

############# Working directory ################

getwd() ### Session - Get Working Directory - To Source File Location ###
# setwd() ### Session - Set Working Directory - To Source File Location
dir()  ### working directory listing
ls()   ### Work space listing of objects
rm('object')  ### Remove an element "object", if exist
rm(list = ls(all = TRUE)) ### Cleaning

################## Descriptive Statistics #####################

# The monthly credit card expenses of an individual in 1000 rupees is 
# given in the file Credit_Card_Expenses.csv.
# Q1. Read the dataset
# Q2. Compute mean, median minimum, maximum, variance, standard
# deviation, skewness, kurtosis and quantiles of Credit Card Expenses
# Q3. Compute default summary of Credit Card Expenses
# Q4. Draw Boxplot of Credit Card Expenses


#### read the csv file using read.csv() function 


Credit_Card_Expenses <- read.csv("Credit_Card_Expenses.csv")
Credit_Card_Expenses
mydata = Credit_Card_Expenses ### load it to another variable
print(mydata) ### print the data frame


### To read a particular column or variable of data set to a new variable Example: 
### Read CC_Expenses to CC

CC = mydata$CC_Expenses
print(CC)

######  Descriptive statistics for variable #####

Mean = mean(CC)
print(Mean)
Median=median(CC)
print(Median)
StandaradDeviation=sd(CC)
print(StandaradDeviation)
Variance=var(CC)
print(Variance)
Minimum=min(CC)
print(Minimum)
Maximum=max(CC)
print(Maximum)
Quantile=quantile(CC)
print(Quantile)

Summary=summary(CC) #five point summary
print(Summary)

##### Another way to calculate Descriptive statistics #####

# install.packages("psych")
library('psych')
data_descriptive=describe(CC)
print(data_descriptive)

#####  Plotting of the Descriptive Statistics #####

dotchart(CC)
boxplot(CC)
boxplot(CC, col="dark green")

################# Data Pre-processing ##################

### Use Import Dataset - From Text (base) - Missing_Values_Telecom - Heading - Yes ###
mydata = read.csv("Missing_Values_Telecom.csv")
newdata = na.omit(mydata) # Discard all records with missing values
write.csv(newdata, file = 'newdata.csv',row.names = TRUE)

### Compute the means excluding the missing values
cmusage = mydata[,2] 
l3musage = mydata[,3] 
avrecharge = mydata[,4]
cmusage_mean = mean(cmusage, na.rm = TRUE) 
l3musage_mean = mean(l3musage, na.rm = TRUE)
avrecharge_mean = mean(avrecharge, na.rm = TRUE)

### Replace the missing values with mean
cmusage[is.na(cmusage)]=cmusage_mean
l3musage[is.na(l3musage)]= l3musage_mean 
avrecharge[is.na(avrecharge)]=avrecharge_mean

mynewdata = cbind(cmusage, l3musage, avrecharge, mydata[,5],mydata[,6]) 
mydata
mynewdata
write.csv(mynewdata, file = 'Missing_Values_Telecom_mod.csv',row.names = TRUE)

############## Data Normalization and Random Sampling #################

mydata = read.csv("Supply_Chain.csv")
# mydata = Supply_Chain
mystddata = scale(mydata)
mystddata

mydata= read.csv("bank_data.csv")
nrow(mydata)
sample = sample(2, nrow(mydata), replace = TRUE, prob = c(0.750, 0.250))
sample1 = mydata[sample ==1,]
nrow(sample1)
sample2 = mydata[sample ==2,]
nrow(sample2)

################################################################################


##### R Functions for Probability Distributions #####

# Every distribution that R handles has four functions. There is a root name, for example, 
# the root name for the normal distribution is norm. This root is prefixed by one of the letters

# p for "probability", the cumulative distribution function (c. d. f.)
# q for "quantile", the inverse c. d. f.
# d for "density", the density function (p. f. or p. d. f.)
# r for "random", a random variable having the specified distribution
# For the normal distribution, these functions are pnorm, qnorm, dnorm, and rnorm. 
# For the binomial distribution, these functions are pbinom, qbinom, dbinom, and rbinom. And so forth.
# R has functions to handle many probability distributions like, Beta, Cauchy, Gamma, Poisson, etc.. 

#### Example of Normal Distribution ##############

# Direct Look-Up
# pnorm is the R function that calculates the c. d. f.
# F(x) = P(X <= x) where X is normal. 

print(pnorm(27.4, 50, 20)) # Here it look up P(X < 27.4) when X is normal with mean 50 and standard deviation 20.

# Inverse Look-Up

# qnorm is the R function that calculates the inverse c. d. f. F-1 of the normal distribution The c. d. f. and the inverse c. d. f. are related by

# p = F(x)
# x = F-1(p)

# So given a number p between zero and one, qnorm looks up the p-th quantile of the normal distribution.

# Q: What is F^(-1)(0.95) when X has the N(100, 15^2) distribution?

print(qnorm(0.95, mean=100, sd=15))

### Random Variates

# rnorm is the R function that simulates random variates having a specified normal distribution. 
# As with pnorm, qnorm, and dnorm, optional arguments specify the mean and standard deviation of the distribution.

x <- rnorm(100, mean=10, sd=5)
print(x)

###### below it plots the histogram of the above 100 random points generated from normal distribution with mean=10 and sd=5

print(hist(x, probability=TRUE)) ### hist plots Histogram ###

### Home Task: Do the same for other distributions for hands on study 

##### For any Functional Help write the following #####

# ?rnorm()
# ?pnorm()
# ?dnorm()
# ?qnorm()

########  Testing of Hypothesis  ########

# Installing all the required packages for the R Notebook

install.packages("car")
install.packages("ggplot2")
install.packages("gplots")
install.packages("qqplotr")
library(car)
library(gplots)
library(qqplotr)
library(ggplot2)

######## One Sample t Test #######

# Problem: Testing whether the average Processing Time of PO_Processing 
# data set is less than equal to 40.

# Step 1: Reading the data as mydata

# SESSION -> SET WORKING DIRECTORY -> TO SOURCE FILE LOCATION

mydata = read.csv('PO_Processing.csv',header = T,sep = ",")

# IMPORT DATASET -> FROM TEXT -> CHOOSE DATA -> HEADING = YES, RENAME THE DATA

PT = mydata$Processing_Time

# Step 2: Using the t Test function to test our hypothesis

# ?t.test

t.test(PT, alternative = 'greater', mu = 40)

# p-value < 0.05 => Reject H0.

######## Normality Test ########

# Problem: Checking whether the Processing Time Data is Normally Distributed

qqnorm(PT)
qqline(PT)

# Normality Check using Shapiro-Wilk test

shapiro.test(PT) 

######## One Way ANOVA ########

# Reading data and variables in R

mydata = read.csv('Sales_Revenue_Anova.csv',header = T,sep = ",")
location = mydata$Location 
revenue = mydata$Sales.Revenue

# Converting location to factor

location = factor(location)

# H0 : location-wise sales figures are equal. 

# Computing ANOVA table

fit = aov(revenue  ~  location)
summary(fit)
# H0 rejected. Sales figures are not equal. 
aggregate(revenue ~ location, FUN = mean)
boxplot(revenue ~ location)
plotmeans(revenue ~ location)

# Tukey's Honestly Significant Difference (HSD) Test

TukeyHSD(fit)

################################# END OF SESSION ###############################
