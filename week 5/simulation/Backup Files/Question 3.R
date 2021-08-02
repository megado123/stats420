birthday = 19810803
set.seed(birthday)

num_sims = 1000 # number of simulations
x_values = seq(0, 5, length = 25)
n        = length(x_values)   # sample size
alpha    = c(0.01,0.05,0.10)
beta_1   = seq(0, 3, .1)
p_value  = rep(0, length(beta_1))


simulate3 = function(num_sim, sigma){
  p_values = matrix(0, num_sim,  length(beta_1))  
  for(sim in 1: num_sim){
    for(i in 1:length(beta_1)) {
      signal   = x_values * beta_1[i]
      eps      = rnorm(25, mean= 0, sd = sigma)
      y        = signal + eps
      fit      = lm(y ~ x_values)
      p_value[i] = summary(fit)$coefficients[2,4]
    }
    p_values[sim, ]  = p_value
  }
  as.data.frame(p_values)
}

get_power = function(data, alpha){
  pwr = matrix(0, nrow = length(alpha), ncol = ncol(data))
  for (j in 1: length(alpha)){
    rejectHo = rep(0, ncol(data))
    for (i in 1: ncol(data)){
      #we should reject Ho when beta > 0
      rejectHo[i] = sum(data[ , i] < alpha[j])
    }
    pwr[ j ,  ] = rejectHo/nrow(data)  
  }
  pwr = as.data.frame(pwr)
  colnames(pwr) = beta_1
  pwr
}

#simulate for sigma = 1
data_sigma1  = simulate3(1000, 1)
#get power power = #tests rejected/#simulations for a given beta_1 value
power_sigma1 = get_power(data_sigma1, alpha)

#simulate for sigma = 2
data_sigma2  = simulate3(1000, 2)
#get power power = #tests rejected/#simulations for a given beta_1 value
power_sigma2 = get_power(data_sigma2, alpha)

#simulate for sigma = 4
data_sigma3  = simulate3(1000, 4)
#get power power = #tests rejected/#simulations for a given beta_1 value
power_sigma3 = get_power(data_sigma3, alpha)


power_sigma1[1, ]

power_sigma2

power_sigma3

plot(power_sigma1[1,])

power_grid = beta_1

power_grid



#Sigma = 1
par(mar = c(5, 5, 2, 2)) # adjusted plot margins, otherwise the "hat" does not display

plot(beta_1, power_sigma1[1, ], pch = 20, col = "dodgerblue", cex = 1,
     xlab = expression(hat(beta)[1] ~ "values"),
     ylab = expression(hat(Power)),
     main = expression(hat(Power) ~ "for " ~ sigma ~ " = 1"))

points(beta_1, power_sigma1[2, ], pch = 20, col = "darkorange", cex = 1)

points(beta_1, power_sigma1[3, ], pch = 20, col = "red", cex = 1)

lines(beta_1, power_sigma1[1, ], col = "dodgerblue")

lines(beta_1, power_sigma1[2, ], col = "darkorange")

lines(beta_1, power_sigma1[3, ], col = "red")

legend("bottomright", c(expression(alpha ~ " = 0.01" ), expression(alpha ~ "= 0.05"), expression(alpha ~ "= 0.10")), lty = 1, lwd = 2,
       col = c("dodgerblue", "darkorange", "red"), adj = c(0.2, 0.6))

#Sigma = 2

par(mar = c(5, 5, 2, 2)) # adjusted plot margins, otherwise the "hat" does not display

plot(beta_1, power_sigma2[1, ], pch = 20, col = "dodgerblue", cex = 1,
     xlab = expression(hat(beta)[1] ~ "values"),
     ylab = expression(hat(Power)),
     main = expression(hat(Power) ~ "for " ~ sigma ~ " = 2"))

points(beta_1, power_sigma2[2, ], pch = 20, col = "darkorange", cex = 1)

points(beta_1, power_sigma2[3, ], pch = 20, col = "red", cex = 1)

lines(beta_1, power_sigma2[1, ], col = "dodgerblue")

lines(beta_1, power_sigma2[2, ], col = "darkorange")

lines(beta_1, power_sigma2[3, ], col = "red")

legend("bottomright", c(expression(alpha ~ " = 0.01" ), expression(alpha ~ "= 0.05"), expression(alpha ~ "= 0.10")), lty = 1, lwd = 2,
       col = c("dodgerblue", "darkorange", "red"), adj = c(0.2, 0.6))


#Sigma = 3

par(mar = c(5, 5, 2, 2)) # adjusted plot margins, otherwise the "hat" does not display

plot(beta_1, power_sigma3[1, ], pch = 20, col = "dodgerblue", cex = 1,
     xlab = expression(hat(beta)[1] ~ "values"),
     ylab = expression(hat(Power)),
     main = expression(hat(Power) ~ "for " ~ sigma ~ " = 4"))

points(beta_1, power_sigma3[2, ], pch = 20, col = "darkorange", cex = 1)

points(beta_1, power_sigma3[3, ], pch = 20, col = "red", cex = 1)

lines(beta_1, power_sigma3[1, ], col = "dodgerblue")

lines(beta_1, power_sigma3[2, ], col = "darkorange")

lines(beta_1, power_sigma3[3, ], col = "red")

legend("bottomright", c(expression(alpha ~ " = 0.01" ), expression(alpha ~ "= 0.05"), expression(alpha ~ "= 0.10")), lty = 1, lwd = 2,
       col = c("dodgerblue", "darkorange", "red"), adj = c(0.2, 0.6))
