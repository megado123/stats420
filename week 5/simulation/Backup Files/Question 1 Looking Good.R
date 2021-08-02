
birthday = 19810803
set.seed(birthday)

library(readr)
study_1 = read_csv("study_1.csv")

num_sims = 3000 # number of simulations
n = 15          # sample size
p = 5           # number of beta parameters

beta_0 = 2
beta_1 = 1
beta_2 = 1
beta_3 = 1
beta_4 = 1
sigma_1 = 1
sigma_2 = 5
sigma_3 = 10

x0 = rep(1, n)
x1 = study_1$x1
x2 = study_1$x2
x3 = study_1$x3
x4 = study_1$x4
X = cbind(x0, x1, x2, x3, x4)
C = solve(t(X) %*% X)
y = rep(0, n)

sim_data = data.frame(x1, x2, x3, x4, y)

simulate = function(num_sims, sigma){

  y_hat        = rep(0, n)
  
  beta_hat_0   = rep(0, num_sims)
  beta_hat_1   = rep(0, num_sims)
  beta_hat_2   = rep(0, num_sims)
  beta_hat_3   = rep(0, num_sims)
  beta_hat_4   = rep(0, num_sims)
  
  beta_hat_00   = rep(0, num_sims)
  beta_hat_11   = rep(0, num_sims)
  beta_hat_22   = rep(0, num_sims)
  beta_hat_33   = rep(0, num_sims)
  beta_hat_44   = rep(0, num_sims)
  #calculating se^2 3 different ways
  se_squared_1   = rep(0, num_sims) 
  se_squared_2 = rep(0, num_sims) 
  se_squared_3 = rep(0, num_sims) 
  
  y_hat_00_1   = rep(0, num_sims)
  y_hat_00_2   = rep(0, num_sims)
  y_hat_00_3   = rep(0, num_sims)
  
  for(i in 1:num_sims) {
    
    eps = rnorm(n, mean = 0 , sd = sigma)
    #makes n=15 values, populate
    sim_data$y = beta_0 * x0 + beta_1 * x1 + beta_2 * x2 + beta_3 * x3 + beta_4 * x4 + eps
    #fit the model
    fit = lm(y ~ x1 + x2 + x3 + x4, data = sim_data)

    
    #grabbing all the beta hat values
    beta_hat_0[i] = coef(fit)[1]
    beta_hat_1[i] = coef(fit)[2]
    beta_hat_2[i] = coef(fit)[3]
    beta_hat_3[i] = coef(fit)[4]
    beta_hat_4[i] = coef(fit)[5]
    
    #get all beta_values to determine expected value at x0
    (beta_hat = solve(t(X) %*% X) %*% t(X) %*% sim_data$y)

    beta_hat_00[i] = beta_hat[1]
    beta_hat_11[i] = beta_hat[2]
    beta_hat_22[i] = beta_hat[3]
    beta_hat_33[i] = beta_hat[4]
    beta_hat_44[i] = beta_hat[5]    
    

    #calculating se 2 different ways, and extracting it from the model for a third way
    y_hat = X %*% solve(t(X) %*% X) %*% t(X) %*% sim_data$y
    e     = sim_data$y - y_hat
    
    se_squared_1[i] = (summary(fit)$sigma) ^ 2
    se_squared_2[i] = (t(e) %*% e / (n - p))
    se_squared_3[i] = (sum(fit$residuals^2)/(n - p))

    xo = c(1, -3, 2.5, .5, 0)
    
    y_hat_00_1[i] = xo %*% beta_hat
    y_hat_00_2[i] = beta_hat_0[i] + (beta_hat_1[i] * -3) + (beta_hat_2[i] * 2.5) + (beta_hat_3[i] * 0.5) + (beta_hat_4[i] * 0)
    y_hat_00_3[i] = beta_hat[1] +    (beta_hat[2] * -3 ) +( beta_hat[3] * 2.5) + (beta_hat[4] * 0.5) + (beta_hat[5] * 0)

 
  }
  data.frame(beta_hat_0 = beta_hat_0, beta_hat_00 = beta_hat_00, beta_hat_1 = beta_hat_1, beta_hat_2 = beta_hat_2, beta_hat_3 = beta_hat_3, beta_hat_4 = beta_hat_4, se_squared_1 = se_squared_1, se_squared_2 = se_squared_2, se_squared_3 = se_squared_3,
             y_hat_00_1 = y_hat_00_1, y_hat_00_2 = y_hat_00_2, y_hat_00_3 = y_hat_00_3)
}

sim1 = simulate(3000, sigma_1)
sim2 = simulate(3000, sigma_2)
sim3 = simulate(3000, sigma_3)

#confirmed se_square1 = se_sqaure2 = se_squared3, confirmed y_hat_00_1 = y_hat_00_2
head(sim1)


#brief analyisis for sim1
beta_1                        # true mean
mean(sim1$beta_hat_1)         # empirical mean
c(mean(sim1$beta_hat_1), beta_1)

var(sim1$beta_hat_1)          # empirical variance
sigma_1 ^ 2 * C[1 + 1, 1 + 1] # true variance


#check the rule
sd_bh1 = sqrt(sigma_1 ^ 2 * C[1 + 1, 1 + 1])
head(sd_bh1)
# We expect these to be: 0.68, 0.95, 0.997

sd_bh1 = sqrt(sigma_1 ^ 2 * C[1 + 1, 1 + 1])
# We expect these to be: 0.68, 0.95, 0.997
mean(beta_1 - 1 * sd_bh1 < sim1$beta_hat_1 & sim1$beta_hat_1 < beta_1 + 1 * sd_bh1)

mean(beta_1 - 2 * sd_bh1 < sim1$beta_hat_1 & sim1$beta_hat_1 < beta_2 + 2 * sd_bh1)

mean(beta_1 - 3 * sd_bh1 < sim1$beta_hat_1 & sim1$beta_hat_1 < beta_2 + 3 * sd_bh1)



#brief analyisis for sim2
beta_1                        # true mean
mean(sim2$beta_hat_1)         # empirical mean
c(mean(sim2$beta_hat_1), beta_1)

var(sim2$beta_hat_1)          # empirical variance
sigma_2 ^ 2 * C[1 + 1, 1 + 1] # true variance

#brief analyisis for sim3
beta_1                        # true mean
mean(sim3$beta_hat_1)         # empirical mean
c(mean(sim3$beta_hat_1), beta_1)

var(sim3$beta_hat_1)          # empirical variance
sigma_3 ^ 2 * C[1 + 1, 1 + 1] # true variance







#Simulation Study with sigma = 1
par(mfrow = c(3,1))
hist(sim1$beta_hat_1, prob = TRUE, breaks = 20,
     xlab = expression(hat(beta)[1]), main = "", border = "dodgerblue")

curve(dnorm(x, mean = beta_1, sd = sqrt(sigma_1 ^ 2 * C[1 + 1, 1 + 1])),
      col = "darkorange", add = TRUE, lwd = 3)

#simulation study with sigma = 5
hist(sim2$beta_hat_1, prob = TRUE, breaks = 20,
     xlab = expression("Histogram for" ~ hat(beta)[1]), main = "", border = "dodgerblue")

curve(dnorm(x, mean = beta_1, sd = sqrt(sigma_2 ^ 2 * C[1 + 1, 1 + 1])),
      col = "darkorange", add = TRUE, lwd = 3)

#simulation study with sigma = 10
hist(sim3$beta_hat_1, prob = TRUE, breaks = 20,
     xlab = expression("Histogram for" ~ hat(beta)[1]), main = "", border = "dodgerblue")

curve(dnorm(x, mean = beta_1, sd = sqrt(sigma_3 ^ 2 * C[1 + 1, 1 + 1])),
      col = "darkorange", add = TRUE, lwd = 3)


# define grid of x values
x = seq(-4, 6, length = 100)
# plot curve for standard normal
plot(x, dnorm(x, mean = 1, sd = sqrt(sigma_1 ^ 2 * C[1 + 1, 1 + 1])), type = "l", lty = 1, lwd = 2,
     xlab = "x", ylab = "Density", main = "Normal Distribution of Beta 1 hat")

#comparing distibutions
curve(dnorm(x, mean = beta_1, sd = sqrt(sigma_1 ^ 2 * C[1 + 1, 1 + 1])),
      col = "darkorange", add = TRUE, lwd = 3)


curve(dnorm(x, mean = beta_1, sd = sqrt(sigma_2 ^ 2 * C[1 + 1, 1 + 1])),
      col = "darkorange", add = TRUE, lwd = 3)


curve(dnorm(x, mean = beta_1, sd = sqrt(sigma_3 ^ 2 * C[1 + 1, 1 + 1])),
      col = "darkorange", add = TRUE, lwd = 3)

head(sim1)

sd(sim1$y_hat_00_1)
var(sim1$y_hat_00_1)

sd(sim2$y_hat_00_1)
var(sim2$y_hat_00_1)

sd(sim3$y_hat_00_1)
var(sim3$y_hat_00_1)

##
plot(x, dnorm(x, mean = 1, sd = sqrt(sigma_1 ^ 2 * C[1 + 1, 1 + 1])), type = "l", lty = 1, lwd = 2,
     xlab = "x", ylab = "Density", main = "Normal Distribution of Beta 1 hat")

#comparing distibutions
curve(dnorm(x, mean = beta_1, sd = sqrt(sigma_1 ^ 2 * C[1 + 1, 1 + 1])),
      col = "darkorange", add = TRUE, lwd = 3)


curve(dnorm(x, mean = beta_1, sd = sqrt(sigma_2 ^ 2 * C[1 + 1, 1 + 1])),
      col = "darkorange", add = TRUE, lwd = 3)


curve(dnorm(x, mean = beta_1, sd = sqrt(sigma_3 ^ 2 * C[1 + 1, 1 + 1])),
      col = "darkorange", add = TRUE, lwd = 3)
##

hist(sim1$y_hat_00_1, prob = TRUE, breaks = 20,
     xlab = expression(hat(y)[(xo)]), main = "", border = "dodgerblue")

curve(dnorm(x, mean = 2, sd = (sigma_1 * 2)),
      col = "darkorange", add = TRUE, lwd = 3)

n = 15
p = 5
lines(x, dt(x, df = n - p), lty = 2, lwd = 2, col = "dodgerblue")



hist(((n-p) * sim1$se_squared)/(sigma_1^2), prob = TRUE, breaks = 20,
     xlab = mtext((expression( over((n-p) * se ^ 2, sigma ^ 2))), side=1, line=4), main = expression("Histogram for" ~ over((n-p) * se ^ 2, sigma ^ 2 ) ~ "where n = 15, p = 5," ~ sigma ~ "= 1"), border = "dodgerblue", cex = 1.5)

curve(dchisq(x, df = 10),
      col = "darkorange", add = TRUE, lwd = 3)
legend("topright", "True Distribution", lty = c(1, 2), lwd = 2, col = "darkorange")


sd(sim1$se_squared_1)
sd(sim2$se_squared_1)
sd(sim3$se_squared_1)

mean(sim1$se_squared_1)
mean(sim2$se_squared_1)
mean(sim3$se_squared_1)
#Simulation Study with sigma = 1
hist(sim1$se_squared_1, prob = TRUE, breaks = 20,
     xlab = expression(hat(y)[1]), main = "", border = "dodgerblue")
n = 15
p = 5

lines(x, df(x, df1 = n - 1, df2 = n-p), lty = 2, lwd = 2, col = "dodgerblue")




lines(x, dchisq(x, df = p - 1), lty = 2, lwd = 2, col = "dodgerblue")

#Simulation Study with sigma = 1
hist(sim2$se_squared_1, prob = TRUE, breaks = 20,
     xlab = expression(se^2), main = "", border = "dodgerblue")
n = 15
p = 5
lines( x, df(x, df1 = n - 1, df2 = n-p), lty = 2, lwd = 2, col = "dodgerblue")

#Simulation Study with sigma = 1
hist(sim3$se_squared_1, prob = TRUE, breaks = 20,
     xlab = expression(hat(y)[1]), main = "", border = "dodgerblue")
lines(x, df(x, df1 = n - 1, df2 = n-p), lty = 2, lwd = 2, col = "dodgerblue")

n = 15
p = 5
lines(x, df(x, df1 = n - 1, df2 = n-p), lty = 2, lwd = 2, col = "dodgerblue")

C = solve(t(X) %*% X)
C
xoo = c(1, -3, 2.5, .5, 0)
#comparing standard deviation from simulation to standard deviation calculated
c(sd1_empirical = sd(sim1$y_hat_00_1), sd1 = sigma_1 * sqrt( t(xoo) %*% C %*% xoo))
c(sd1_empirical = sd(sim1$y_hat_00_2), sd1 = sigma_1 * sqrt( t(xoo) %*% C %*% xoo))

head(sim1)

c(sd2_empirical = sd(sim2$y_hat_00_1), sd2 = sigma_2 * sqrt( t(xoo) %*% C %*% xoo ))

c(sd3_empirical = sd(sim3$y_hat_00_1), sd3 = sigma_3 * sqrt( t(xoo) %*% C %*% xoo ))

c(sd3_empirical = sd(sim3$y_hat_00_2), sd3 = sigma_3 * sqrt( t(xoo) %*% C %*% xoo ))


my_beta_hat = rbind(5.7752881, 1.2929440, 1.2441390, -0.4389524, 0.7657343)
my_beta_hat

my_xoo =  cbind(1, -3, 2.5, .05, 0)

my_xoo %*% my_beta_hat

5.7752881 + (1.2929440 * -3) + (1.2441390 * 2.5) + (-0.4389524 * 0.)



get_true_variance = function(sigma){
  sigma ^ 2 * C[1 + 1, 1 + 1]
}

get_true_sd = function(sigma){
  sqrt(sigma ^ 2 * C[1 + 1, 1 + 1])
}

true_mean      = c(beta_1,beta_1, beta_1)
empirical_mean = c(mean(sim1$beta_hat_1),      mean(sim2$beta_hat_1),      mean(sim3$beta_hat_1))
true_variance  = c(get_true_variance(sigma_1), get_true_variance(sigma_2), get_true_variance(sigma_3))
empirical_var  = c(var(sim1$beta_hat_1),       var(sim2$beta_hat_1),       var(sim3$beta_hat_1))
true_sd        = c(get_true_sd(sigma_1),       get_true_sd(sigma_2),       get_true_sd(sigma_3))
empirical_sd   = c(sd(sim1$beta_hat_1),        sd(sim2$beta_hat_1),        sd(sim3$beta_hat_1))

simulations = c("sim 1", "sim 2", "sim 3")                 

results = data.frame(simulations, true_mean, empirical_mean, true_variance, empirical_var, true_sd, empirical_sd)
colnames(results) = c("Model", "True Mean", "Empirical Mean", "True Variance", "Empirical Variance", "True SD", "Empirical SD" )

  
#knitr::kable(results,format = "latex", caption = $beta$ ~ "analysis")
