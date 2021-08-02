
library(readr)
nutrition = read_csv("nutrition.csv")

View(nutrition)

#1 Significance of regression
nutrition = read_csv("nutrition.csv")
null_nut_model = lm(Calories ~ 1, data = nutrition)
full_nut_model = lm(Calories ~ Carbs + Fat + Protein, data = nutrition)

#value of the test statistic
anova(null_nut_model, full_nut_model)$F[2]

#F = SSReg/(p-1) / SSE/ (n-p)

p = length(coef(full_nut_model))
n = nrow(nutrition)
           
SSReg = sum(fitted(full_nut_model) - fitted(null_nut_model))^2
SSE = sum(resid(full_nut_model)^2)

(f_calculated = (SSReg/(p -1)) / (SSE/(n-p)))
anova(null_nut_model, full_nut_model)$F[2]
  

#2.2e-16
anova(null_nut_model, full_nut_model)[2, 6]

#1b
anova(null_nut_model, full_nut_model)

coef(summary(full_nut_model))

coef(full_nut_model)

names(full_nut_model)

full_nut_model$fitted.values

##
big_mac = data.frame( Carbs =  47, Fat = 28, Protein = 25 )
predict(full_nut_model, newdata = big_mac)
predict(full_nut_model, newdata = big_mac, interval = "confidence", level = 0.99)


names(summary(full_nut_model))

summary(full_nut_model)$r.squared
#1

y     = nutrition$Calories
y_hat = full_nut_model$fitted.values
n     = nrow(nutrition)
p     = length(coef(full_nut_model)) 
(sqrt(sum((y - y_hat) ^ 2) / (n - p)))

#s_e for the model
summary(full_nut_model)$sigma

sd(y)
sqrt( (1/(length(y)-1) *   sum((y - mean(y))^2)))

mean(y)

confint(full_nut_model, level = 0.99, parm = "Fat")

#Question 2:
goalies = read_csv("goalies_cleaned.csv")
null_model = lm(W ~ GA + SA + SV , data = goalies)
full_model = lm(W ~ GA + SA + SV + MIN + PIM, data = goalies)
anova(null_model, full_model)


goalies = read_csv("goalies_cleaned.csv")
#Model 2
null_model =  lm(W ~ GA + SA + SV + MIN + PIM, data = goalies)
#Model 3
full_model = lm(W ~ ., data = goalies)
anova(null_model, full_model)
#test statistic, F value
anova(null_model, full_model)[2,6]
#p-value
anova(null_model, full_model)[2,5]


#2.c
model_2 = lm(W ~ GA + SA + SV + MIN + PIM, data = goalies)
summary(model_2)$coef["SA", ]
#t-value
summary(model_2)$coef["SA", "t value"]
#p-value
summary(model_2)$coef["SA", "Pr(>|t|)"]



#Question 3

y = as.vector(goalies$W)
#was unable to do this as a vector, went with matrix
X = as.matrix(cbind("(Intercept)" = rep(1, nrow(goalies)), goalies[,-which(names(goalies) == "W")]))
(beta_hat_no_lm = (solve(t(X) %*% X) %*% t(X) %*% y)[,1])

beta_hat_no_lm

#confirming returning a vector
is.vector(beta_hat_no_lm)
sum(beta_hat_no_lm)

model = lm(W ~ ., data = goalies)
beta_hat_lm = coef(model)
#confirming returning a vector
is.vector(beta_hat_lm)

all.equal(beta_hat_no_lm, beta_hat_lm )


#Exercise 3
e = summary(model)$residuals
s_e_calc = sqrt((t(e) %*% e)/ (nrow(goalies) - length(coef(model))))
s_e_calc = (s_e_calc)[1,1]

(s_e = summary(model)$sigma)

all.equal(s_e, s_e_calc)



#Calculate R*2

#SSReg/SST

#e was redisuals from part d
y_hat = y - e
reg = y_hat - mean(y)
tot = y - mean(y)

r_squared_calc = ((t(reg) %*% reg)) / (t(tot) %*% tot)
r_squared_calc = r_squared_calc[1,1]

(r_squared      = summary(model)$r.squared)

all.equal(r_squared , r_squared_calc)







View(goalies)
#exercise 3
y = as.vector(goalies$W)
nrow(goalies)
x0 = rep(1: nrow(goalies))
x1 = goalies$GA
x2 = goalies$SA
x3 = goalies$SV
x4 = goalies$SV_PCT
x5 = goalies$GAA
x6 = goalies$SO
x7 = goalies$MIN
x8 = goalies$PIM
X = cbind(x0, x1, x2, x3, x4, x5, x6, x7, x8)
