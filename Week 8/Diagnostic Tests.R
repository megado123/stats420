#simulated data

sim_1 = function(sample_size = 500){
  x = runif(n = sample_size) * 5
  y = 3 + 5 * x + rnorm(n = sample_size, mean = 0, sd = 1)
  data.frame(x,y)
}

#mean is linear function
#variance is a function of x - non-constant variance
sim_2 = function(sample_size = 500){
  x = runif(n = sample_size) * 5
  y = 3 + 5 * x + rnorm(n = sample_size, mean = 0, sd = x)
  data.frame(x,y)
}

#mean is not a function of x - violation of assumptions
sim_3 = function(sample_size = 500){
  x = runif(n = sample_size) * 5
  y = 3 + 5 * x ^ 2 + rnorm(n = sample_size, mean = 0, sd = 5)
  data.frame(x,y)
}

set.seed(42)
sim_data_1 = sim_1()
fit_1 = lm(y ~ x, data = sim_data_1)
plot (y ~ x, data = sim_data_1, col = "grey", pch = 20,
      main = "Data from Model 1")
abline(fit_1, col = "darkorange", lwd = 3)

#fitted vs. residuals
plot(fitted(fit_1), resid(fit_1), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model 1")
abline(h = 0, col = "darkorange", lwd = 2)

#Model 2
#generate data with increasing variance
#works for linear regression
set.seed(42)
sim_data_2 = sim_2()
fit_2 = lm(y ~ x, data = sim_data_2)
plot (y ~ x, data = sim_data_2, col = "grey", pch = 20,
      main = "Data from Model 2 - non-constant variance issue")
abline(fit_2, col = "darkorange", lwd = 3)


#fitted vs. residuals
#works for MLR and SLR
#lower and higher variance, normal variance issue
plot(fitted(fit_2), resid(fit_2), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model 2")
abline(h = 0, col = "darkorange", lwd = 2)

#Model 3
#generate data with increasing variance
set.seed(42)
sim_data_3 = sim_3()
fit_3 = lm(y ~ x, data = sim_data_3)
plot (y ~ x, data = sim_data_3, col = "grey", pch = 20,
      main = "Data from Model 3")
abline(fit_3, col = "darkorange", lwd = 3)

#fitted vs. residuals
#constant variance, but bad form
#mean is not a linear function of predictor
plot(fitted(fit_3), resid(fit_3), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model 3")
abline(h = 0, col = "darkorange", lwd = 2)

#breush-pagan test

library(lmtest)


qq_plot = function(e) {
  n = length(e)
  normal_quantiles = qnorm(((1:n - 0.5) / (n)))
  #normal_quantiles = qnorm(((1:n) / (n + 1)))
  # plot theoretical verus observed quantiles
  plot(normal_quantiles, sort(e),
       xlab = c("Theoretical Quantiles"),
       ylab = c("Sample Quantiles"),
       col = "dodgerblue")
  title("Normal Q-Q Plot written function")

  ## calculate line through the first and third quartiles
  slope = (quantile(e, 0.75) - quantile(e, 0.25)) / (qnorm(0.75) - qnorm(0.25))
  intercept = quantile(e, 0.25) - slope * qnorm(0.25)
  # add to existing plot
  abline(intercept, slope, lty = 2, lwd = 2, col = "darkorange")
}

set.seed(42)
x = rnorm(100, mean = 0, sd = 1)
par(mfrow = c(1, 2))
qqnorm(x, col = "darkgrey")
qqline(x, lty = 2, lwd = 2, col = "dodgerblue")
qq_plot(x)

#normal data
par(mfrow = c(3,3))
set.seed(42)
qq_plot(rnorm(10))
qq_plot(rnorm(25))
qq_plot(rnorm(100))

#t data
par(mfrow = c(1,3))
set.seed(42)
qq_plot(rt(10, df = 4))
qq_plot(rt(25, df = 4))
qq_plot(rt(100, df = 4))

#exponential Data
par(mfrow = c(1,3))
set.seed(42)
qq_plot(rexp(10))
qq_plot(rexp(25))
qq_plot(rexp(100))



#data without assumption violation
qqnorm(resid(fit_1), main = "Normal Q-Q Plot, fit_1", col = "darkgrey")
qqline(resid(fit_1), col = "dodgerblue", lwd = 2)

qqnorm(resid(fit_2), main = "Normal Q-Q Plot, fit_2", col = "darkgrey")
qqline(resid(fit_2), col = "dodgerblue", lwd = 2)

qqnorm(resid(fit_3), main = "Normal Q-Q Plot, fit_3", col = "darkgrey")
qqline(resid(fit_3), col = "dodgerblue", lwd = 2)


set.seed(42)
shapiro.test(rnorm(25))
shapiro.test(rexp(25))

shapiro.test(resid(fit_1))
shapiro.test(resid(fit_2))
shapiro.test(resid(fit_3))
