#previously we have known the data
# we assumed a model

#what if we knew the true model
#but didn't know the data
#given a true model, simulate data

#assume the truth
#Y|X ~ N(5 - 2x, 9)
#Bo = 5, B1 = -2, sigma^ 2 = 9
# get data, and calculate 
#Bo hat, B1 hat
#does out procedure work correctly

#generate data according to model
# mean + noise

# Y = 5 - 2x + ei, e ~ N(Mu = 0, sigma^2 = 9)
#Bo = 5, B1 = -2 sigmaSquared = 9

num_obs = 21
beta_0 = 5
beta_1 = -2
sigma = 3 # standard deviation

#generate data

x_values = seq(from = 0, to = 10, length.out = num_obs)

set.seed(1)
(epsilon = rnorm( n = num_obs, mean = 0, sd = sigma))

y_vals = (beta_0 + (beta_1 * x_values)) + epsilon

# fit model
sim_fit = lm( y_vals ~ x_values)

plot(y_vals, x_values)

abline(sim_fit)


#write function to do simulation
sim_slr = function(x, beta_0 = 10, beta_1 = 5, sigma = 1){
  n = length(x)
  epsilon = rnorm(n, mean = 0, sd = sigma)
  y = beta_0 + beta_1 * x + epsilon
  data.frame(predictor = x, response = y)
}

set.seed(1)
sim_data = sim_slr(x = x_values, beta_0 = 5, beta_1 = -2, sigma = 3)
head(sim_data)

# they are the same
sim_data$response


#recreate, but use a 

set.seed(1)
sim_data = sim_slr(x = x_values, beta_0 = 5, beta_1 = -2, sigma = 3)
sim_fit = lm(response ~ predictor, data = sim_data)
coef(sim_fit)

plot(response ~ predictor, data = sim_data,
     xlab = "Simulated Predictor Variable",
     ylab = "Simulated Response Variable",
     main = "Simulated Regression Data",
     pch = 20,
     cex = 2, 
     col = "grey")
abline(sim_fit, lwd = 3, lty = 1, col = "darkorange")

abline(beta_0, beta_1, lwd = 2, col = "dodgerblue")

legend("topright", c("Estimate", "Truth"), lty = c(1,2), lwd = 2,
                     col = c("darkorange", "dodgerblue"))
