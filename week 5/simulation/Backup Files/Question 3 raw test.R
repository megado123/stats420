
num_sims = 1000 # number of simulations
n = 25          # sample size

beta_0   = 0
beta_1   = seq(0, 3, .1) #when beta_1 is 0, there is no linear relationship
length(beta_1)
x_values = seq(0, 5, length = 25)
alpha    = c(0.01,0.05,0.10)
rejectHo = rep(0, length(alpha))
sigmas    = c(1, 2, 4)
y = rep(0, n)

sim_data = data.frame(x_values, y)

simulate = function(num_sims, sigma){
  p_value = matrix(0, nrow = 3, ncol = num_sims)
  for(i in 1:num_sims, sigma) {
    eps = rnorm(n, mean= 0, sd = sigma)
    ys   =  (beta_1 * x_values) + eps
    sim_model = lm(y ~ x_values)
    #fail to reject 
    p_value = summary(sim_model)$coefficients[2,4]
    for(i in 1:length(alpha)) {
      ifelse(p_value < alpha[i], rejectHo[i] = 1, rejectHo[i] = 0)
    }
    data.frame(beta_1 = beta_1, 
             p_value_alpha_01 = p_value[, 1], rejectHo_alpha_01 = rejectHo[ , 1],
             p_value_alpha_05 = p_value[, 2], rejectHo_alpha_05 = rejectHo[ , 2],
             p_value_alpha_10 = p_value[, 3], rejectHo_alpha_10 = rejectHo[ , 3]
             ) 
}

