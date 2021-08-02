#Home 3

rm(list=ls())

library(MASS)

#1a.
cat_model = lm( Hwt ~ Bwt, data = cats)
cat_model
summary(cat_model)
names(summary(cat_model))

s_e  = summary(cat_model)$sigma

Sxx = sum((cats$Bwt - mean(cats$Bwt))^2)

cat_model$coefficients[2]/(s_e/sqrt(Sxx))

(p_value = summary(cat_model)$coefficients[2,4])

(beta_1_hat_t = summary(cat_model)$coefficients["Bwt", "t value"])
(p_val_beta_1_calculated = 2 *pt(abs(beta_1_hat_t), df = nrow(cats) - 2, lower.tail = FALSE))

#1b - calculate 99% confi interval for beta_1

confint(cats_model, level = 0.99)[2,]

#manually calculated
(beta_1_hat = coef(cats_model)[2])
(beta_1_se = summary(cats_model)$sigma/sqrt(Sxx))
(crit = qt(.995, df = nrow(cats) - 2))
c(beta_1_hat - (crit * beta_1_se), beta_1_hat + (crit * beta_1_se))

#1d

new_bwt= data.frame(Bwt = c(2.5, 3.0))
(predict_data_mean = predict(cat_model, newdata = new_bwt, interval = c("confidence"), level = 0.95))

names(summary(predict_data_mean))


#to post
new_bwt= data.frame(Bwt = c(1.0, 2.0))
(predict_data_mean = predict(cat_model, newdata = new_bwt, interval = c("confidence"), level = 0.99))

summary(predict_data_mean)



#difference between the mean of the cats body weight in the dataset and the value estimated for the body weight at 3.0
abs(mean(cats$Bwt) - 3.0)

#difference between the mean of the cats body weight in the dataset and the value estimated for the body weight at 2.5
abs(mean(cats$Bwt) - 2.5)

abs(mean(cats$Bwt) - 3.0) > abs(mean(cats$Bwt) - 2.5)


#Calculate a 90% confidence interval for ??0 
#Give an interpretation of the interval in the context of the problem.
model = lm(eruptions ~ waiting, data = faithful)
confint(model, level = 0.90)[1, ]
#For a confidence interval of 90%, beta_0 would be between -2.13833519 and -1.60969678

#1.d)

new_bwt= data.frame(Bwt = c(2.5, 3.0))
(predict_data_mean = predict(cats_model, newdata = new_bwt, interval = c("confidence"), level = 0.95))

predict_data_mean[1,3]- predict_data_mean[1,2]
predict_data_mean[2,3]- predict_data_mean[2,2]




#1.e)
model = lm(eruptions ~ waiting, data = faithful)
new_duration = data.frame(waiting = 72)
predict(model, newdata = new_duration, interval = c("prediction"), level = 0.99)[1,3]

#Question 2
library(faraway)
cholesterol_model = lm(chol ~ weight, data = diabetes)
summary(cholesterol_model)

#2b
hdl_model = lm(hdl ~ weight, data = diabetes)
summary(hdl_model)


#Question 3:
get_p_val_beta_1 = function(model, beta_1_0 = 0) {
  model_info    = summary(model)$coefficients
  beta_1_hat    = model_info[2,1]    #estimate
  beta_1_hat_se = model_info[2,2] # std error
  t = (beta_1_hat - beta_1_0)/ beta_1_hat_se
  p_val = 2 * pt(abs(t), df = length(resid(model)) - 2, lower.tail = FALSE)
  values = c(
    t = t,
    p_val = p_val)
}


get_p_val_beta_1(cat_model, beta_1 = 4.2)
get_p_val_beta_1(cholesterol_model)
get_p_val_beta_1(hdl_model)



#Question 4
birthday = 19810803
set.seed(birthday)
n = 42
x = seq(0, 20, length = n)

birthday = 19810803
set.seed(birthday)
n = 42
x = seq(0, 20, length = n)
sigma  = 5
beta_0 = 3
beta_1 = 0.75
sim_beta_hat = function(x, beta_0, beta_1, sigma) {
  beta_hat_0 = rep(0, 1500)
  beta_hat_1 = rep(0, 1500)
  n = 42
  for (i in 1:1500){
    epsilon = rnorm(n, mean = 0, sd = sigma)
    y = beta_0 + beta_1 * x + epsilon
    sim_data = data.frame(predictor = x, response = y)
    sim_model = lm(response ~ predictor, data = sim_data)
    beta_hat_0[i] = coef(sim_model)[1]
    beta_hat_1[i] = coef(sim_model)[2]
  }
  data.frame(beta_hat_0 = beta_hat_0, beta_hat_1 = beta_hat_1)
}

(beta_hat = sim_beta_hat(x, beta_0, beta_1, sigma))



sigma * sqrt( (1/n) + (mean(x) ^ 2 / Sxx))           






#Question 5
birthday = 19810803
set.seed(birthday)
n = 20
x = seq(-5, 5, length = n)

sigma  = 4
beta_0 = 1
beta_1 = 3

sim_data = function(x, beta_0, beta_1, sigma) {
  beta_hat_0 = rep(0, 2000)
  s_e = rep(0, 2000)
  for (i in 1:2000){
    epsilon = rnorm(n, mean = 0, sd = sigma)
    y = beta_0 + beta_1 * x + epsilon
    sim_data = data.frame(predictor = x, response = y)
    sim_model = lm(response ~ predictor, data = sim_data)
    beta_hat_0[i] = coef(sim_model)[1]
    s_e = summary(sim_model)$sigma
  }
  data.frame(beta_hat_0 = beta_hat_0, s_e = s_e)
}
vals  = sim_data(x, beta_0, beta_1, sigma)


nrow(vals)
#calculate the 90% confidence interval

(crit         = qt(.95, df = nrow(vals) - 2))   #critical value
Sxx           = sum((x - mean(x)) ^ 2)          #Sxx
SE_beta_0_hat = vals$s_e * sqrt( (1/n) + ((mean(x)^2) / Sxx))

lower_90 = vals$beta_hat_0 - (crit * SE_beta_0_hat)
upper_90 = vals$beta_hat_0 + (crit * SE_beta_0_hat)

#what proportion of intervals contain the true value of beta_0
(mean(beta_0 > lower_90 & beta_0 < upper_90))

(mean(0 > lower_90 & 0 < upper_90))

(mean(beta_0 > lower_90 & beta_0 < upper_90))
