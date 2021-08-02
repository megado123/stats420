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

xo = c(1, -3, 2.5, 0.5, 0)
sim_data = data.frame(x1, x2, x3, x4, y)

simulate = function(num_sims, sigma){
  
  beta_hat_1   = rep(0, num_sims)
  se_squared   = rep(0, num_sims) 
  y_hat_xo     = rep(0, num_sims)

  for(i in 1:num_sims) {
    #create noise
    eps = rnorm(n, mean = 0 , sd = sigma)
    #simulate y values
    sim_data$y = beta_0 * x0 + beta_1 * x1 + beta_2 * x2 + beta_3 * x3 + beta_4 * x4 + eps
    #fit the model
    fit = lm(y ~ x1 + x2 + x3 + x4, data = sim_data)
    beta_hat_1[i]   = coef(fit)[2]
    se_squared[i]   = (summary(fit)$sigma) ^ 2
    beta_hat = solve(t(X) %*% X) %*% t(X) %*% sim_data$y
    y_hat_xo[i] = xo %*% beta_hat

  }
  data.frame(beta_hat_1 = beta_hat_1, se_squared = se_squared, y_hat_xo)
}

#run 3000 simulations for 3 different values of sigma, generating 3 different dataframes
sim1 = simulate(3000, sigma_1)
sim2 = simulate(3000, sigma_2)
sim3 = simulate(3000, sigma_3)

#Examine beta_1
c(true_mean = mean(sim1$beta_hat_1), empirical_mean =  beta_1)

c(true_variance = sigma_1 ^ 2 * C[1 + 1, 1 + 1], empirical_variance = var(sim1$beta_hat_1) )

c(true_sd = sqrt(sigma_1 ^ 2 * C[1 + 1, 1 + 1]), empirical_sd = sqrt(sigma_1 ^ 2 * C[1 + 1, 1 + 1]))


#check the probability rule
# We expect these to be: 0.68, 0.95, 0.997

(sd_bh1 = sqrt(sigma_1 ^ 2 * C[1 + 1, 1 + 1]))
# We expect these to be: 0.68, 0.95, 0.997
mean(beta_1 - 1 * sd_bh1 < sim1$beta_hat_1 & sim1$beta_hat_1 < beta_1 + 1 * sd_bh1)

mean(beta_1 - 2 * sd_bh1 < sim1$beta_hat_1 & sim1$beta_hat_1 < beta_2 + 2 * sd_bh1)

mean(beta_1 - 3 * sd_bh1 < sim1$beta_hat_1 & sim1$beta_hat_1 < beta_2 + 3 * sd_bh1)

prob = rep(0, 3)
get_prob = function(sigma, beta_hat_1){
  sd_bh1 = sqrt(sigma ^ 2 * C[1 + 1, 1 + 1])
  prob[1] = mean(beta_1 - 1 * sd_bh1 < beta_hat_1 & beta_hat_1 < beta_1 + 1 * sd_bh1)
  prob[2] = mean(beta_1 - 2 * sd_bh1 < beta_hat_1 & beta_hat_1 < beta_2 + 2 * sd_bh1)
  prob[3] = mean(beta_1 - 3 * sd_bh1 < beta_hat_1 & beta_hat_1 < beta_2 + 3 * sd_bh1)
  prob
}
# We expect these to be: 0.68, 0.95, 0.997 since beta_hat_1 follows a normal distribution
get_prob(sigma_1, sim1$beta_hat_1)
get_prob(sigma_2, sim2$beta_hat_1)
get_prob(sigma_3, sim3$beta_hat_1)

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
hist(sim1$beta_hat_1, prob = TRUE, breaks = 20,
     xlab = expression(hat(beta)[1]), main = "", border = "dodgerblue")

curve(dnorm(x, mean = beta_1, sd = sqrt(sigma_1 ^ 2 * C[1 + 1, 1 + 1])),
      col = "darkorange", add = TRUE, lwd = 3)

#simulation study with sigma = 5
hist(sim2$beta_hat_1, prob = TRUE, breaks = 20,
     xlab = expression(hat(beta)[1]), main = "", border = "dodgerblue")

curve(dnorm(x, mean = beta_1, sd = sqrt(sigma_2 ^ 2 * C[1 + 1, 1 + 1])),
      col = "darkorange", add = TRUE, lwd = 3)

#simulation study with sigma = 10
hist(sim3$beta_hat_1, prob = TRUE, breaks = 20,
     xlab = expression(hat(beta)[1]), main = "", border = "dodgerblue")

curve(dnorm(x, mean = beta_1, sd = sqrt(sigma_3 ^ 2 * C[1 + 1, 1 + 1])),
      col = "darkorange", add = TRUE, lwd = 3)





#-------------------------------
hist(sim1$beta_hat_1, prob = TRUE, breaks = 20,
     xlab = expression(hat(beta)[1]), main = expression(hat(beta)[1]  ~ "for" ~ sigma ~ "=1") , border = "dodgerblue")

curve(dnorm(x, mean = beta_1, sd = sqrt(sigma_1 ^ 2 * C[1 + 1, 1 + 1])),
      col = "darkorange", add = TRUE, lwd = 3)

# add legend
legend("topright", "True Distribution", lty = c(1, 2), lwd = 2, col = "darkorange")


dev.off()
# define grid of x values

# plot curve for true normal distributions of beta_1_hat
plot(x, dnorm(x, mean = 1, sd = sqrt(sigma_1 ^ 2 * C[1 + 1, 1 + 1])), type = "l", lty = 1, lwd = 2,
     xlab = expression(hat(beta)[1]), ylab = "Density", main = expression("True normal distribution of " ~ hat(beta)[1] ~ "for" ~ sigma ~ "values"), col = "darkorange")

curve(dnorm(x, mean = beta_1, sd = sqrt(sigma_2 ^ 2 * C[1 + 1, 1 + 1])),
      col = "dodgerblue", add = TRUE, lwd = 3)


curve(dnorm(x, mean = beta_1, sd = sqrt(sigma_3 ^ 2 * C[1 + 1, 1 + 1])),
      col = "red", add = TRUE, lwd = 3)

legend("topright", c(expression(sigma ~ " = 1"), expression(sigma ~ " = 5"), expression(sigma ~ " = 10")), lty = c(1,1,1), lwd = 2,
       col = c("darkorange", "dodgerblue", "red"), adj = c(0.2, 0.6), cex=0.75)



head(sim1)

sd(sim1$y_hat_00_1)
var(sim1$y_hat_00_1)

sd(sim2$y_hat_00_1)
var(sim2$y_hat_00_1)

sd(sim3$y_hat_00_1)
var(sim3$y_hat_00_1)


max(sim1$y_hat_xo)

#simulation 1
hist(sim1$y_hat_xo, prob = TRUE, breaks = 20,
     xlab = expression(hat(E) ~ "[Y|" ~ x[1] ~ "= -3," ~ x[2] ~" = 2.5," ~ x[3] ~" = 0.5," ~ x[4] ~ "= 0]"), main = expression("Histogram for" ~ hat(E) ~ "[Y] when " ~ sigma ~ " = 1"), border = "dodgerblue",  ylim = c(0, .2))

                
curve(dnorm(x, mean = 2, sd = sigma_1 * sqrt( t(xoo) %*% C %*% xoo)),
      col = "darkorange", add = TRUE, lwd = 3)

#simulation 2
hist(sim2$y_hat_xo, prob = TRUE, breaks = 20,
     xlab = expression(hat(E) ~ "[Y|" ~ x[1] ~ "= -3," ~ x[2] ~" = 2.5," ~ x[3] ~" = 0.5," ~ x[4] ~ "= 0]"), main = expression("Histogram for" ~ hat(E) ~ "[Y] when " ~ sigma ~ " = 5"), border = "dodgerblue",  ylim = c(0, .04))


curve(dnorm(x, mean = 2, sd = sigma_2 * sqrt( t(xoo) %*% C %*% xoo)),
      col = "darkorange", add = TRUE, lwd = 3)

#simulation 3
hist(sim3$y_hat_xo, prob = TRUE, breaks = 20,
     xlab = expression(hat(E) ~ "[Y|" ~ x[1] ~ "= -3," ~ x[2] ~" = 2.5," ~ x[3] ~" = 0.5," ~ x[4] ~ "= 0]"), main = expression("Histogram for" ~ hat(E) ~ "[Y] when " ~ sigma ~ " = 10"), border = "dodgerblue",  ylim = c(0, .02))


curve(dnorm(x, mean = 2, sd = sigma_3 * sqrt( t(xoo) %*% C %*% xoo)),
      col = "darkorange", add = TRUE, lwd = 3)



n = 15
p = 5
lines(x, dt(x, df = n - p), lty = 2, lwd = 2, col = "dodgerblue")




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
