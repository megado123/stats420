#modeling, 

stop_dist_model = lm(dist ~speed, data = cars)

#returns 4 estimates
confint(stop_dist_model, level = 0.99)

#level of model that was fit
confint(stop_dist_model, level = 0.99)[2,2]

#get confidence interval for a a single parameter
confint(stop_dist_model, parm = "(Intercept)", level = 0.99)


summary(stop_dist_model)

#store estimate
beta_1_hat = coef(stop_dist_model)[2]

beta_1_hat

summary(stop_dist_model)$coefficients

#store standard error, sigma/Sxx
beta_1_hat_se = summary(stop_dist_model)$coefficients[2,2]
beta_1_hat_se

#verify standard error
(Sxx= sum((cars$speed - mean(cars$speed))^2))
(s_e = summary(stop_dist_model)$sigma) # what shoudl be stored for question 5

(beta_1_hat_se_calc = s_e/sqrt(Sxx))

all.equal(beta_1_hat_se_calc, beta_1_hat_se)

#calculate the critical value
(1 - 0.99)/2
(crit = qt(0.995, df = nrow(cars) - 2))

#lower and upper bound for confidence interval
c(beta_1_hat - crit * beta_1_hat_se, beta_1_hat + crit * beta_1_hat_se)
  