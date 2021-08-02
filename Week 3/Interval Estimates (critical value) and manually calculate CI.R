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

#calculate the critical value, alpha/2 to be on the right
#confidence level is 99
(1 - 0.99)/2
1-0.005
(crit = qt(0.995, df = nrow(cars) - 2))

#lower and upper bound for confidence interval
c(beta_1_hat - crit * beta_1_hat_se, beta_1_hat + crit * beta_1_hat_se)

#is the propability   
pt(qt(0.995, df = nrow(cars) - 2), df = nrow(cars) -2 )

#alpha
1- pt(qt(0.995, df = nrow(cars) - 2), df = nrow(cars) -2 )

#new data
new_speeds = data.frame(speed = c(5, 21))

predict(stop_dist_model, newdata = new_speeds)

predict(stop_dist_model, newdata = new_speeds,
        interval = c("confidence"), level= 0.99)

predict(stop_dist_model, newdata = new_speeds,
        interval = c("prediction"), level= 0.99)

#####

#possible x points
speed_grid = seq(min(cars$speed), max(cars$speed), by = 0.01)

#storing ci
dist_ci_band = predict(stop_dist_model, 
                       newdata = data.frame(speed = speed_grid), 
                       interval = "confidence", level = 0.99)

#storing pi
dist_pi_band = predict(stop_dist_model, 
                       newdata = data.frame(speed = speed_grid), 
                       interval = "prediction", level = 0.99) 

plot(dist ~ speed, data = cars,
     xlab = "Speed (in Miles Per Hour)",
     ylab = "Stopping Distance (in Feet)",
     main = "Stopping Distance vs Speed",
     pch  = 20,
     cex  = 2,
     col  = "grey",
     ylim = c(min(dist_pi_band), max(dist_pi_band)))
abline(stop_dist_model, lwd = 5, col = "darkorange")

lines(speed_grid, dist_ci_band[,"lwr"], col = "dodgerblue", lwd = 3, lty = 2)
lines(speed_grid, dist_ci_band[,"upr"], col = "dodgerblue", lwd = 3, lty = 2)
lines(speed_grid, dist_pi_band[,"lwr"], col = "dodgerblue", lwd = 3, lty = 3)
lines(speed_grid, dist_pi_band[,"upr"], col = "dodgerblue", lwd = 3, lty = 3)
points(mean(cars$speed), mean(cars$dist), pch = "+", cex = 3)
