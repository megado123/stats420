#install.packages("faraway")

# 403 African Americans were interviewed in a study to understand the prevalence of:
# obesity, diabetes, and other cardiovascular risk factors in central Virginia.
diabetes = faraway::diabetes
length(diabetes) ## 19 variables in the dataset
View(diabetes) 
head(diabetes)
?diabetes
str(diabetes)

library(tibble)
diabetes = as_tibble(diabetes)
str(diabetes)
?str
View(diabetes)

colnames(diabetes)


head(diabetes)
?tibble

# number of observations for a tibble - length of tibble - 403
nrow(diabetes)

#number of variables - 19
ncol(diabetes)

?sapply
lapply(diabetes, is.factor)


?is.factor
colName(diabetes[is.factor == TRUE])

is.factor(diabetes)

diabetes[is.factor == TRUE] ## returns tibble

v = sapply(diabetes, is.factor, simplify = TRUE)
v[which(v == TRUE)]


v[which( == TRUE)]

v2

str(v2)

v3 = v2[1:3[1]]

?v2
v3 = as.vector(v2)
colnames(v3)

temp = which(is.factor(diabetes))
temp

(diabetes[ diabetes$gender == "female", ])



#mean of hdl
mean(diabetes$hdl, na.rm = TRUE)
?mean
#50.44527

#What is the standard deviation 
#of total cholesterol of individuals in this sample?
?diabetes
sd(diabetes$chol, na.rm = TRUE)
#44.44556

#what is the range of ages
max(diabetes$age)
##92
min(diabetes$age)
##19
range(diabetes$age)
##19 92

range(diabetes$age)[1]

range(diabetes$age)[2]

nrow(diabetes[ diabetes$gender == "female", ])
nrow(temp)

nrow(diabetes[ diabetes$gender == "male", ])
nrow(temp)

#tibble of females
mean(diabetes[ diabetes$gender == "female", ]$hdl)
# 52.11111 (confirmed in excel)
#write.csv(diabetes[ diabetes$gender == "female", ], "C:/Users/a05v6zz/Desktop/females.csv")

#Create a scatter plot of HDL (y-axis) vs weight (x-axis). Use a non-default color for the points. 
#(Also,be sure to give the plot a title and label the axes appropriately.) 
#Based on the scatter plot, does there seem
#to be a relationship between the two variables? Briefly explain.
?diabetes
plot(diabetes$weight, diabetes$hdl)

plot(hdl ~ weight, data = diabetes)

plot(hdl ~ weight, data = diabetes,
     xlab = "Weight (in pounds)",
     ylab = "High Density Lipoprotein",
     main = "HDL vs Weight",
     pch  = 20,
     cex  = 1,
     col  = "dodgerblue")

#As weight increases, HDL decreases

#Exercise 1i
#(i) Create a scatter plot of total cholesterol (y-axis) vs weight (x-axis). Use a non-default color for the points.
#(Also, be sure to give the plot a title and label the axes appropriately.) Based on the scatter plot, does there
#seem to be a relationship between the two variables? Briefly explain.


plot(chol ~ weight, data = diabetes,
     xlab = "Weight (in pounds)",
     ylab = "Total Cholesterol",
     main = "Cholesterol vs Weight",
     pch  = 20,
     cex  = 1,
     col  = "dodgerblue")

#There does not appear to be a relationship between the variables

#Exercise 1j
#(j) Create side-by-side boxplots for HDL by gender. Use non-default colors 
#for the plot. (Also, be sure to give the plot a title and label the axes 
#appropriately.) Based on the boxplot, does there seem to be a difference
#between HDL level and gender? Briefly explain.

boxplot(hdl ~ gender, data = diabetes)

boxplot(hdl ~ gender, data = diabetes,
        xlab   = "Gender",
        ylab   = "High Density Lipoprotein",
        main   = "HDL vs Gender",
        pch    = 20,
        cex    = 1,
        col    = "darkorange",
        border = "dodgerblue")

IQR(diabetes$hdl, na.rm = TRUE)

IQR(diabetes[ diabetes$gender == "female", ]$hdl, na.rm = TRUE)

IQR(diabetes[ diabetes$gender == "male", ]$hdl, na.rm = TRUE)


#There is more variablity/outliers in females?

#Exercise 2
#make sure to change the file in the RMarkDown

library(readr)
nutrition <- read_csv("~/University of Illinois/Status 420/Week 1/homework/w01-hw/nutrition.csv")
View(nutrition)

#(a) Create a histogram of Calories. Do not modify R's default bin selection. 
#Make the plot presentable.
#Describe the shape of the histogram. Do you notice anything unusual?
hist(nutrition$Calories)

?hist

hist(nutrition$Calories,
     xlab   = "Calories",
     main   = "Histogram of Calories",
     #breaks = 12,
     col    = "dodgerblue",
     border = "darkorange")

#This appears to be left leaning (confirm this), however there appears to be an unexpected
#higher frequency of 350-400 calories

#2b
#(b) Create a scatter plot of calories (y-axis) vs protein (x-axis). 
#Make the plot presentable. Do you notice
#any trends? Do you think that knowing only the protein content of a food, 
#you could make a good prediction
#of the calories in the food?
nutrition$Protein

plot(Calories ~ Protein, data = nutrition,
     xlab = "Protein",
     ylab = "Calories",
     main = "Calories vs Protein",
     pch  = 20,
     cex  = 1,
     col  = "dodgerblue")

#The trend shows that at low amounts of proein the number of calories is all over.
#The trend that is appearant is as the number of protein increases the n
#As the protein in food increases, the possibility to making a prediction of the 
#calories in food increases.

?I()

#(c) Create a scatter plot of 
#Calories (y-axis) vs 4 * Protein + 4 * Carbs + 9 * Fat + 2 * Fiber (xaxis).
#Make the plot presentable. 
#You will either need to add a new variable to the data frame, or, 
#use the
#I() function in your formula in the call to plot(). 
#If you are at all familiar with nutrition, you may realize
#that this formula calculates the calorie count based on the protein, carbohydrate, and fat values. You'd
#expect then that the result here is a straight line. 
#Is it? If not, can you think of any reasons why it is not?

ncol(nutrition) #15 columsn
calorieCalc = 4 * nutrition$Protein + 4 * nutrition$Carbs + 9 * nutrition$Fat + 2 * nutrition$Fiber
nutrition = cbind(nutrition, calorieCalc)
ncol (nutrition)
#View(nutrition)

plot(Calories ~ calorieCalc, data = nutrition,
     xlab = "4 * Protein + 4 * Carbs + 9 * Fat + 2 * Fiber",
     ylab = "Calories",
     main = "Calories vs \n Calorie count based on the protein, carbohydrate, and fat values",
     pch  = 20,
     cex  = 1,
     col  = "dodgerblue")

#Is this a straight line - mostly?


#Exercise 3
a = 1:10
b = 10:1
c = rep(1, times = 10)
d = 2 ^ (1:10)

sum_of_squares = function(x){
  sum(x ^ 2)
}

# requires initialization of output, seems not ideal
output = 0
sum_of_squares2 = function(x){
  output = (x ^ 2) + output
}


sum_of_squares(x = a)
#385, which has been confirmed
a
temp = a^ 2
temp
sum(temp)

sum_of_squares(x = c(c, d)) 
#1398110

temp = c(c, d)
temp
sum(temp)

#Write a function called rms_diff.
length(a)

rms_diff = function (x, y){
    sqrt(  (1/length(x)) * sum(( x - y) ^ 2))
}
a
b
rms_diff(x = a, y = b)

rms_diff(x = d, y = c)

rms_diff(x = d, y = 1)

rms_diff(x = a, y = 0) ^ 2 * length(a)
