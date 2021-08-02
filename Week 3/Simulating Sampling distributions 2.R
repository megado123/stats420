#simulating sampling distributions
#simulation of data, and varifying against the true model
set.seed(42)
sample_size = 100 # this is n

x = seq(-1,1, length = sample_size)
x
Sxx = sum((x - mean(x)) ^ 2)
Sxx

beta_0 = 3
beta_1 = 6
sigma = 2

var_beta_1_hat = sigma ^ 2 / Sxx
var_beta_1_hat
var_beta_0_hat = sigma ^ 2 * ((1/sample_size) + mean(x)/Sxx)
var_beta_0_hat

num_samples = 10000 # make 10,000 datasets 
beta_0_hats = rep(0, num_samples) #pre-allocate a vector
beta_1_hats = rep(0, num_samples) #pre-allocate a vector

for (i in 1: num_samples) {
  eps = rnorm(sample_size, mean = 0, sd = sigma)
  y = beta_0 + (beta_1 * x) + eps
  
  sim_model = lm (y ~ x)
  
  beta_0_hats[i] = coef(sim_model)[1]
  beta_1_hats[i] = coef(sim_model)[2]
}

mean(beta_1_hats) #empirical mean
beta_1            #true mean

var(beta_1_hats)  #empirical variance
var_beta_1_hat    #true variance

#empirical distribution
hist(beta_1_hats, prob = TRUE, breaks = 20,
     xlab = expression(hat(beta)[1]), main = "", border = "dodgerblue")

#curve of true distribution
curve(dnorm(x, mean = beta_1, sd = sqrt(var_beta_1_hat)),
      col = "darkorange", add = TRUE, lwd = 3)

#use simulated data to calculate the probability that a value is less than 6.2
#empirical mean = probabilit that a value is less than 6.2   
mean(beta_1_hats < 6.2)




mean(beta_0_hats) #empirical mean
beta_0            #true mean
var(beta_0_hats)  #empirical variance
var_beta_0_hat    #true variance

#empirical distribution
hist(beta_0_hats, prob = TRUE, breaks = 25,
     xlab = expression(hat(beta)[0]), main = "", border = "dodgerblue")

#curve of true distribution
curve(dnorm(x, mean = beta_0, sd = sqrt(var_beta_0_hat)),
      col = "darkorange", add = TRUE, lwd = 3)
