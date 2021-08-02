## How many individuals in the melanoma dataset from the 
## MASS package died from a melanoma?

library(MASS)
mel = MASS::Melanoma
View(mel)
?Melanoma
# status 
# 1 = died from melanoma
# 2 = alive
# 3 = dead from other causes

##mel[mel$status == 1]
length(which(mel$status == 1)) ## 57 is the correct answer


## Question 2
## What is the average age of individuals in the melanoma dataset 
## from the MASS package who are alive?
View(mel)

which(mel$status == 2) ## indexes of vector of alive people

sum(mel$age[mel$status ==2])/length(mel$age[mel$status ==2])

mean(mel$age[mel$status == 2])

##Answer: 50.00746

## Question 3
## Which animal in the mammals dataset from the MASS package has 
## the largest brain weight relative to its body weight 
## (that is, the largest brain weight to body weight ratio)?
mam = MASS::mammals
View(mam)

ratio = mam$brain/mam$body
ratio
#mam$name[which.max(mam$brain/mam$body)]
which.max(mam$brain/mam$body)
##Answer: Ground squirrel

##Question 4
# Create side-by-side boxplots for each of the numeric variables in the iris dataset. 
# To do so, simply supply the usual function with a dataframe of only the numeric 
# variables of the dataset. (Use the code block above.)
library(datasets)
data(iris)
iris
str(iris)

iris$Species = NULL
head(iris)

View(iris)

boxplot(iris, las = 2)

## exclude species from dataset
iris$Species = NULL
head(iris)


boxplot(iris, las = 2)

boxplot(iris)

?boxplot
summary(iris)

var(iris)
iris

sd(iris$Sepal.Length)
sd(iris$Sepal.Width)
sd(iris$Petal.Length)
sd(iris$Petal.Width)

##Question 4b
# Based on this plot, 
# which variable is the most variable? 
# Calculate the standard deviation of this variable. (Use the code block above.)
sd(iris$Petal.Length)

#Question 5
# The above code block has access to a list stored in the variable z.
# Calculate the sum of:
  #The minimum first element of z
  #The maximum of the second element of z
  #The mean of the third element of z

z = list(
  a = c(1, 2, 3, 4),
  b = c(5, 6, 7, 8), 
  c = c(9, 10, 11, 12), 
  d = function(arg = 42) {print("Hello World")},
  e = diag(5)
)

z[1]
z[[1]]

min(z[[1]]) + max(z[[2]]) + mean(z[[3]])

?airquality
#Question 6 - New York

#####################################
#Question 7:
#Using the airquality dataset, what is the average wind speed in May ?

View(airquality)
##data frame
str(airquality)
##Galton[Galton$sex == "F",]$height
airquality[airquality$Month == 5,]$Wind

mean(airquality[airquality$Month == 5,]$Wind)

#Question 8
#Using the airquality dataset, what is the average ozone measurement? 
#Hint: read the documentation of any function that returns an unexpected result. 
#You will likely find a solution to the issue.

mean(airquality$Ozone, na.rm = TRUE) # 42.12931

#Question 9 
#Using the airquality dataset, create a scatter plot to compare windspeed and temperature. 
#Based on this plot, you believe that:

plot(airquality$Temp, airquality$Wind)
  
#Question 10 
# What proportion of the elements of x are larger than 2 in magnitude? 
# Be sure to run the two lines in order, otherwise your vector will not contain the expected elements.

set.seed(1337)
x = rnorm(10000)
x
str(x)
head(x, 10)

head(x[x < 0], 100)

length(x[x< -2])


x[abs(x > 2)]

length(x[(x > 2) | (x < -2)])/length(x)

length(x[abs(x>2)])/length(x) ##221 + 223/10000

#Question 11
#Write a function called f that has a single argument input with a default value of 42 
# which is assumed to be a vector of numeric values. The function should output a vector 
# that is input but with any negative values replaced with 0.
#Hint: The ifelse() function could be useful here. Note that all three arguments to ifelse() are vectors.


f = function(input = 42){
  ifelse(input < 0, 0, input)
} 

v1 = c(-1,3,5,7)

f(v1)
set.seed(42)
x = rnorm(100, mean = 0, sd = 10)
mean(f(input = x)) - f()

#-37.70725


#Question 12
#Create three vectors x0, x1, and y. Each should have a length of 30 and store the following:
  #x0: Each element should be the value 1
  #x1: The first 30 square numbers, starting from 1 (so 1, 4, 9, etc.)
  #y: The result of running the given code, after creating the other two vectors

x0 = rep(1, 30)
x1 = (1:30)^2
#done for me
set.seed(42)
y  = 5 * x0 + x1 + rnorm(n = 30, mean = 0 , sd = 1)

mean(y)
# 320.2353

#Question 13
# Create a matrix X with columns x0 and x1. 
# Report the sum of the elements in rows 17 and 19.

x0
x1

X = cbind( col1 = x0, col2 = x1)

sum(X[17,]) + sum(X[19,])
##Answer: 652

#Question 14
#(XT X)^???1 XT y

beta_hat = solve(t(X) %*% X) %*% t(X) %*% y

beta_hat

sum(beta_hat)

#6.427899

#Question 15
#y_hat = X beta_hat

y_hat = X %*% beta_hat

#y_hat
#y
#str(y_hat)
#y_hat[1,]

#Question 15
sum((y - y_hat)^2)
#42.67698