
#========================================================================
#
# Author: AS 
# Creation: 2020-11-22 
# Last modified: Time-stamp: <2020-09-10 17:27:53 Angelo Secchi>
#
# Description: Introduction to Econometrics with R 
#
#
#========================================================================


## Introduction
## ------------

#simple arithmetic
1+1
sqrt(2)

#defining variables and checking types
foo <- sqrt(2)
faa <- sqrt(2)
typeof(foo)

#logical test
foo == faa
foo != faa

#define numerical vectors
vec <- c(4,3,5,6,12)
vec
typeof(vec)

#access vector elements and combining them
vec[2]
vec[2]+vec[3]

#define and access characters vectors
vec2 <- c("secchi","jacquemet","charroin")
typeof(vec2)
vec2[2]
vec2[2]+vec[1]
paste(vec2[2],vec[1])
paste(vec2[2],vec2[1])
paste(vec2[2],vec2[1],sep="++++")
c(vec2,'iodice')
vec2 <- c(vec2,'iodice')

#define sequences and replications
x <- seq(from = 1, to = 5, by = 1)
x
p_unif <- rep(1/5,5)
p_unif

#compute mean in different ways
sum(p_unif)
weighted.mean(x,p_unif)
p <- c(rep(2/5,2),rep(1/15,3))
p
sum(p)
weighted.mean(x,p)
p*x
sum(p*x)

#inspecting and cleaning the environment 
ls()
rm(p)
ls()
rm(list=ls())
ls()

#present working directory and howto change it
getwd()
setwd('/home/angelo/Downloads')
getwd()

#user-defined functions
compute_wmean <- function(input1,input2) {
    x_inside <- input1
    p_inside <- input2
    output <- sum(x_inside*p_inside)
    return(output)
}

x_outside <- seq(from = 1, to = 5, by = 1)
p_outside_unif <- rep(1/5,5)
p_outside <- c(rep(2/5,2),rep(1/15,3))
compute_wmean(x_outside,p_outside_unif)
compute_wmean(x_outside,p_outside)

#comparing user-defined and built-in function
compute_wmean(x_outside)
compute_wmean(x)
p <- c(rep(2/5,2),rep(1/5,3))
sum(p)
compute_wmean(x,p)
weighted.mean(x,p)


#Ex. 1 Write a function "compute_wmean_wcheck" that checks if the sum of the
# weigths is 1 and print the phrase "WARNING: weigths do not sum to 1"
# if it is not the case

#Ex. 2 Write a function "compute_wmean_asR" that behaves with respect
#to the weights as the built-in function weighted.mean.


## Linear regression on artificial data
## ------------------------------------

# Generate data - sample 1
set.seed(1)
x1 <- rnorm(250,0,10)
u <- rnorm(250,0,100)
#Generate the model [population regression function]
y <- 10+1*x1+u
# Estimate the model [built-in command]
estim1 <- lm(y ~ x1)
summary(estim1)

# Estimate the model with OLS minimization solutions
b1 <- cov(x1,y)/var(x1)
b0 <- mean(y)-b1*mean(x1)
print(paste('The sample regression function reads: y=',round(b0,3),'+',round(b1,3),'*x1',sep=''))

# Estimate the model with OLS minimization solutions [matrix form]
X <- as.matrix(cbind(1,x1))
Y <- as.matrix(y)
b <- solve(t(X)%*%X)%*%t(X)%*%Y
t(b)

# Estimate the model via numerical minimization
OLS_objective <- function(b){
  sum((y-b[1]-b[2]*x1)^2)
}
b_num <- optim(c(0,0),OLS_objective)
b_num$par

# Monte Carlo experiment r=20
set.seed(1)
WD<-getwd()
#set sample size and number of replications
n<-250; r<-20
#set the true parameters value
b0<-10;b1<-1;su<-100
#initialize two vectors to store results
b0hat<-numeric(r)
b1hat<-numeric(r)
#Generate X
x1<-rnorm(n,0,10)
#Main loop to repeat estimate r times
for (j in 1:r) {
  #draw sample
  u<-rnorm(n,0,su)
  #generate the model
  y<-b0+b1*x1+u
  #estimate the model by OLS and store the results
  bhat<-coefficients(lm(y~x1))
  b0hat[j]<-bhat["(Intercept)"]
  b1hat[j]<-bhat["x1"]
}
#Generate and save the file
WD <- getwd()
outfile <- paste(WD,"/ols_mc1.pdf",sep="")
pdf(file=outfile,width=7,height=7)
hist(b1hat,freq=FALSE,xlim=c(-1,3),
     main=(expression(paste("E(",hat(beta)[1],")=0.9058, SD(",hat(beta)[1],")=0.6312"))),
     xlab=expression(hat(beta)[1]))
xfit<-seq(-1,3,0.01)
lines(xfit,dnorm(xfit,mean(b1hat),sd(b1hat)),lwd=2,col="red")
dev.off()

## Install packages
## ----------------
install.packages("tidyverse")
install.packages(c("haven", "readr","vroom","felm"))

## Import and manipulate data
## --------------------------
library(haven)
library(readr)
# Load the .dta file
WD <- getwd()
car_data <- read_dta(paste(WD,"/data/auto.dta",sep=''))
# Print the saved dataset in the console
car_data
# select part of a data frame
df[1:2,1:2]
# select variables by name
car_data$price
car_data$price*2
# change a specific value in a data frame
car_data$price[2]
car_data$price[2] <- 0
car_data$price[2]
car_data$price[2] <- 4749

# Save as .csv
write_csv(car_data,paste(WD,"/data/auto.csv",sep=''))
# Load the .csv file
car_data <- read_csv(paste0(WD,"/data/auto.csv",sep=''))

# Examining the data 
# Names of the variables
names(car_data)
# Head of the dataset (first six lines)
head(car_data)
# First 11 lines
head(car_data, n = 11)
# Last 7 lines
tail(car_data, n = 7)
# Summarize all variables
summary(car_data)
# Summarize price variable
summary(car_data$price)

# load tidyverse package
library("tidyverse")
# select-ing 
# Select our desired variables; define as car_sub
car_sub <- select(car_data, price, mpg)
# Print the dataset
car_sub

# arrange-ing 
# Arrange by price and mpg
arrange(car_sub, price, mpg)
# Arrange by price (descending) and mpg (ascending)
arrange(car_sub, desc(price), mpg)

# summarize-ing 
# Using summarize() to find the mean and standard deviation of price
summarize(car_sub, mean(price), sd(price))
# Same thing but naming the outputs
summarize(car_sub, price_mean = mean(price), price_sd = sd(price))

# filter-ing to subset data row-wise
filter(car_sub,price<=3800)
filter(car_sub,price <= 3800 & mpg==35)
filter(car_sub,price <= 3800 | mpg==35)

## Plotting the data 
## -----------------

# A simple histogram of mpg
hist(car_sub$mpg)
# A prettier histogram of mpg
hist(
  # The variable for the histogram
  x = car_sub$mpg,
  # The main title
  main = "Distribution of fuel economy",
  # The x-axis label
  xlab = "MPG (miles per gallon)")
# The blue vertical line at the median MPG (lwd is line width)
abline(v = median(car_sub$mpg), col = "blue", lwd = 3)
# Scatterplot of mpg and price
plot(
  x = car_sub$mpg,
  y = car_sub$price,
  xlab = "Fuel economy (MPG)",
  ylab = "Price")

## Pipe and join data.frame
## ------------------------

# prepare data.frame
Died.At <- c(22,40,72,41)
Writer.At <- c(16, 18, 36, 36)
First.Name <- c("John", "Edgar", "Walt", "Jane")
Second.Name <- c("Doe", "Poe", "Whitman", "Austen")
Sex <- c("MALE", "MALE", "MALE", "FEMALE")
Date.Of.Death <- c("2015-05-10", "1849-10-07", "1892-03-26","1817-07-18")
# create a data frame 
df <- data.frame(Died.At, Writer.At, First.Name, Second.Name, Sex, Date.Of.Death)
df

# use pipe to manipulate data frames  [super powerful]
df %>% filter(Died.At <= 41 & Sex=="MALE")
df %>% filter(Died.At <= 41 & Sex=="MALE") %>% select(Sex)
df2 <- df %>% filter(Died.At <= 41 & Sex=="MALE") %>% select(Sex)

# yet another  way to generate a data frame
superheroes <- "
    name, alignment, gender,         publisher
 Magneto,       bad,   male,            Marvel
   Storm,      good, female,            Marvel
Mystique,       bad, female,            Marvel
  Batman,      good,   male,                DC
   Joker,       bad,   male,                DC
Catwoman,       bad, female,                DC
 Hellboy,      good,   male, Dark Horse Comics
"
superheroes_df <- read_csv(superheroes, skip = 1)
superheroes_df

publishers <- "
  publisher, yr_founded
         DC,       1934
     Marvel,       1939
      Image,       1992
"
publishers_df <- read_csv(publishers, skip = 1)
publishers_df

# INNER_JOIN()
## return all rows from x where there are matching values
## in y, and all columns from x and y. If there are multiple matches
## between x and y, all combination of the matches are returned.
inner_join(superheroes_df,publishers_df,by = "publisher")

# LEFT_JOIN()
## return all rows from x, and all columns from x and y. Rows in x with
## no match in y will have NA values in the ne##w columns. If there are
## multiple matches between x and y, all combinations of the matches
## are returned.
left_join(superheroes_df,publishers_df,by = "publisher")

# RIGHT_JOIN()
## return all rows from y, and all columns from x and y. Rows in y with
## no match in x will have NA values in the new columns. If there are
## multiple matches between x and y, all combinations of the matches
## are returned.
right_join(superheroes_df,publishers_df)

# FULL_JOIN()
## return all rows and all columns from both x and y. Where there are
## not matching values, returns NA for the one missing.

full_join(superheroes_df,publishers_df)


## Wrapping-up
## -----------
df <- left_join(superheroes_df,publishers_df)
# use mutate() to generate new variables
df %>% mutate(new_var=mean(yr_founded))
#use group_by to operate on sub-groups 
df %>% mutate(new_var=mean(yr_founded)) %>%  group_by(publisher) %>% mutate(new_var_byg=mean(yr_founded))
# use summarise() to summarise data
df %>%  group_by(publisher) %>% select(publisher,yr_founded) %>% summarise_all(max)



