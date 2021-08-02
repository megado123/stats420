#Quiz

library(MASS)

?cats
View(cats)
str(cats) #sex is a factor variable
model_cat = lm(Hwt ~ Bwt, data = cats)
coef(summary(model_cat))[2,1]

model_cat_add = lm(Hwt ~ Bwt + Sex, data = cats)

model_cat_int = lm(Hwt ~ Bwt + Sex + Bwt:Sex,  data = cats)

model_cat_int2 = lm(Hwt ~  Bwt * Sex, data = cats)

coefficients(summary(model_cat_int))
beta_0 = coefficients(summary(model_cat_int))[1,1]
beta_1 = coefficients(summary(model_cat_int))[2,1]
beta_2 = coefficients(summary(model_cat_int))[3,1]
beta_3 = coefficients(summary(model_cat_int))[3,1]

#Question 2
beta_1 = coefficients(summary(model_cat_int))[2,1]

#Question 3
beta_1 + beta_3

#Quetion 4
model_cat_add = lm(Hwt ~ Bwt + Sex, data = cats)
beta_0 = coefficients(summary(model_cat_int))[1,1]
beta_1 = coefficients(summary(model_cat_int))[2,1]
beta_2 = coefficients(summary(model_cat_int))[3,1]
beta_3 = coefficients(summary(model_cat_int))[3,1]

beta_2

#Question 5
model_cat_add = lm(Hwt ~ Bwt + Sex, data = cats)
model_cat_int = lm(Hwt ~ Bwt + Sex + Bwt:Sex,  data = cats)
(anova(model_cat_add, model_cat_int))


#Question 6  - interaction
View(iris)
#Question 7
# predict the sepal length of a versicolor with a petal length of 5.10.
iris_add = lm(Sepal.Length ~ Petal.Length + Species, data = iris)

predict(iris_add, newdata = data.frame(Petal.Length = 5.10, Species = "versicolor" ))

summary(iris_add)

unique(iris$Species)

confint(iris_add, parm = "Speciesvirginica", level = 0.90)[1,1]

#Question 9
iris_add = lm(Sepal.Length ~ Petal.Length + Species, data = iris)
model_no_species = lm(Sepal.Length ~ Petal.Length, data = iris)
anova(model_no_species, iris_add )$F

#Question 10 - how many beta parameters
iris_int = lm(Sepal.Length ~ Petal.Length * Species, data = iris)
summary(iris_int)
length(coefficients(summary(iris_int))[, 1])

#Question 11
iris_int = lm(Sepal.Length ~ Petal.Length * Species, data = iris)

new_data = data.frame(Petal.Length = 5.10, Species = "versicolor")
predict(iris_int, newdata = new_data,interval = c("prediction"), level = 0.99)[1,3]

#Question 12
iris_int = lm(Sepal.Length ~ Petal.Length * Species, data = iris)
beta_0 = coefficients(summary(iris_int))[1,1]
beta_1 = coefficients(summary(iris_int))[2,1]
beta_2 = coefficients(summary(iris_int))[3,1]
beta_3 = coefficients(summary(iris_int))[4,1]
beta_4 = coefficients(summary(iris_int))[5,1]
beta_5 = coefficients(summary(iris_int))[6,1]

beta_1 + beta_4

anova(iris_add, iris_int)


#Question 14
?swiss
View(swiss)
str(swiss)
unique(swiss$Education)
swiss_int = lm(Fertility ~ Education + Catholic + Infant.Mortality + 
               Education:Catholic + Education:Infant.Mortality + Catholic:Infant.Mortality +
               Education:Catholic:Infant.Mortality, data = swiss)
beta_0 = coefficients(summary(swiss_int))[1,1]
beta_1 = coefficients(summary(swiss_int))[2,1]
beta_2 = coefficients(summary(swiss_int))[3,1]
beta_3 = coefficients(summary(swiss_int))[4,1]
beta_4 = coefficients(summary(swiss_int))[5,1]
beta_5 = coefficients(summary(swiss_int))[6,1]
beta_6 = coefficients(summary(swiss_int))[7,1]
beta_7 = coefficients(summary(swiss_int))[8,1]
x2 = 90
x3 = 20
beta_1 + (beta_4 * x2) + (beta_5 * x3) + (beta_7 * x2 * x3)

#Question 15
swiss_int_2way = lm(Fertility ~ Education + Catholic + Infant.Mortality + 
                 Education:Catholic + Education:Infant.Mortality + Catholic:Infant.Mortality, data = swiss)

anova(swiss_int_2way, swiss_int)

coefficients(summary(swiss_int))[8, 4]
