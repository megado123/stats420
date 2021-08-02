#dang he went fast
#confint()
#qt() 
#predict

#fit model
stop_dist_model = lm(dist ~ speed, data = cars)

#plot data and fitted model
plot(dist ~ speed, data = cars,
     xlab = "Speed (in Miles Per Hour)",
     ylab = "Stopping Distance (in Feet)",
     main = "Stopping Distance vs Speed",
     pch  = 20,
     cex  = 2,
     col  = "grey")
abline(stop_dist_model, lwd = 5, col = "darkorange")

confint(stop_dist_model, level = 0.99)
# get a specific value of the matrix
confint(stop_dist_model, level = 0.99)[2,2]

confint(stop_dist_model, parm = "(Intercept)", level = 0.99)

confint(stop_dist_model, parm = "speed", level = 0.99)

#information about fitted model
summary(stop_dist_model)

#note in summary the Std. Error 0.4155

beta_1_hat = coef(stop_dist_model)[2]

#store the standard error
beta_1_hat_se = summary(stop_dist_model)$coefficients[2,2]

#verify the standard error
Sxx = sum((cars$speed - mean(cars$speed))^2)
s_e = summary(stop_dist_model)$sigma

#Residual Standard Error RSE, R calls sigma
beta_1_hat_se_calculated = s_e/sqrt(Sxx)
beta_1_hat_se

#David why is this false?  No super important, but I would like to understand.
beta_1_hat_se == beta_1_hat_se_calculated

# calculate critical value for two-sided 99% CI
(1 - 0.99)/2 # = 0.005, 1 - confidence 
#and since 2 sided, we want 1/2 of area to be on either side

crit = qt(0.995, df = nrow(cars) - 2)
##2.682204
confint(stop_dist_model, parm = "speed", level = 0.99)
