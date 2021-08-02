#simulating sampling distributions

set.seed(42)
sample_size = 100
x = seq(-1, 1, length = sample_size)
Sxx = sum((x - mean(x))^2)

beta_0 = 3
beta_1 = 6
sigma  = 2

var_beta_1_hat = sigma ^ 2 / Sxx
var_beta_0_hat = sigma^2 *((mean(x)^2/Sxx) + (1/sample_size))

num_samples = 10000
beta_0_hats = rep(0, num_samples)
beta_1_hats = rep(0, num_samples)

for (i in 1:num_samples){
  eps = rnorm(sample_size, mean= 0, sd = sigma)
  y   = beta_0 + beta_1 * x + eps
  
  sim_model = lm(y ~ x)
  beta_0_hats[i] = coef(sim_model)[1]
  beta_1_hats[i] = coef(sim_model)[2]
}

mean(beta_1_hats) # empirical mean
beta_1            # true mean
var(beta_1_hats)  # empirical variance
var_beta_1_hat    # true variance

hist(beta_1_hats, prob = TRUE, breaks = 20, 
     xlab = expression(hat(beta)[1]), main = "", border = "dodgerblue")
curve(dnorm(x, mean = beta_1, sd = sqrt(var_beta_1_hat)), 
      col = "darkorange", add = TRUE, lwd = 3)

mean(beta_0_hats) # empirical mean
beta_0            # true mean
var(beta_0_hats)  # empirical variance
var_beta_0_hat    # true variance

par(mar = c(5, 5, 1, 1)) # adjusted plot margins, otherwise the "hat" does not display
plot(cumsum(beta_1_hats) / (1:length(beta_1_hats)), type = "l", ylim = c(5.95, 6.05),
     xlab = "Number of Simulations",
     ylab = expression("Empirical Mean of " ~ hat(beta)[1]),
     col  = "dodgerblue")
abline(h = 6, col = "darkorange", lwd = 2)


# define grid of x values
x = seq(-4, 4, length = 100)

# plot curve for standard normal
plot(x, dnorm(x), type = "l", lty = 1, lwd = 2,
     xlab = "x", ylab = "Density", main = "Normal vs t Distributions")
# add curves for t distributions
lines(x, dt(x, df = 1), lty = 3, lwd = 2, col = "darkorange")
lines(x, dt(x, df = 10), lty = 2, lwd = 2, col = "dodgerblue")

# add legend
legend("topright", title = "Distributions",
       legend = c("t, df = 1", "t, df = 10", "Standard Normal"), 
       lwd = 2, lty=c(3, 2, 1), col = c("darkorange", "dodgerblue", "black"))
