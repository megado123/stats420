#Homework 9 - Exercise 2

?Boston
library(leaps)
library(MASS)
base_model = lm(medv ~ ., data = Boston)
base_model_int = lm(medv ~ ., data = Boston)
calc_loocv_rmse = function(model) {
  sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
}

#full Model - checking with leaps
all_boston_leaps = summary(regsubsets(medv ~ ., data = Boston))
all_boston_model$which
all_boston_model$rss
all_boston_model$adjr2
best_r2_ind = which.max(all_boston_model$adjr2)
all_boston_model$which[best_r2_ind, ]

bost_mod_leaps8 = lm(medv ~ zn + chas + nox + rm + dis + ptratio + black + lstat, data = Boston)
bost_mod_leaps7  = lm(medv ~ chas + nox + rm + dis + ptratio + black + lstat, data = Boston)

calc_loocv_rmse(bost_mod_leaps8)

#full Model - checking leaps with interactions - cant do exhaustive search
all_boston_leaps_int = summary(regsubsets(medv ~ ( .) ^ 2, data = Boston))
all_boston_leaps_int$which
all_boston_leaps_int$rss
all_boston_leaps_int$adjr2
best_r2_ind = which.max(all_boston_leaps_int$adjr2)
all_boston_leaps_int$which[best_r2_ind, ]

bost_mod_leaps8_int = lm(medv ~ zn + chas + nox + rm + dis + ptratio + black + lstat, data = Boston)
bost_mod_leaps7_int  = lm(medv ~ chas + nox + rm + dis + ptratio + black + lstat, data = Boston)

calc_loocv_rmse(bost_mod_leaps7)

#Including all interaction terms is way better than leaps?
test = lm(medv ~ (.) ^ 2 + I(lstat ^ 2), data = Boston)
summary(test)
#3.397
calc_loocv_rmse(test)
bptest(test)

#Including all interaction terms is way better than leaps?
test = lm(medv ~ (.) ^ 2 + I(lstat ^ 2), data = Boston)
summary(test)
#3.418
calc_loocv_rmse(test)
bptest(test)






#

#Backwards AIC
base_mod_back_aic = step(base_model, direction = "backward")
calc_loocv_rmse(base_mod_back_aic) #4.871
base_mod_back_aic_int = step(base_model_int, direction = "backward")   
calc_loocv_rmse(base_model_int) #4.871

#Backwards BIC
n = length(resid(base_model))
base_mod_back_bic = step(base_model, direction = "backward", k = log(n))
calc_loocv_rmse(base_mod_back_bic) #4.849

n = length(resid(base_model_int))
base_mod_back_aic_int = step(base_model_int, direction = "backward", k = log(n))
calc_loocv_rmse(base_mod_back_aic_int) #4.849

#Forward AIC
mod_start = lm(medv ~ 1, data = Boston)
bost_mod_forw_aic = step(
  mod_start, 
  scope = medv ~ lstat + crim + rm  + tax + zn + indus + chas + nox + age + dis + rad + tax + ptratio + black, 
  direction = "forward")

calc_loocv_rmse(bost_mod_forw_aic)

summary(lm(medv ~ ., data = Boston))



