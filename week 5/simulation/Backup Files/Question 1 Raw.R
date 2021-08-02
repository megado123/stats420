library(readr)
study_1 = read_csv("study_1.csv")


birthday = 19810803
set.seed(birthday)

n = 15 # sample size
p = 5

beta_0 = 2
beta_1 = 1
beta_2 = 1
beta_3 = 1
beta_4 = 1

sigma  = c(1, 5, 10)
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

x_00 = c(1, -3, 2.5, 0.5, 0)

#for a given simulation, used to store y values
y = rep(0, n)
sim_data = data.frame(x1, x2, x3, x4, y)

View(sim_data)

num_sims = 3000
beta_hat_1_sigma1 = rep(0, num_sims)
beta_hat_1_sigma2 = rep(0, num_sims)
beta_hat_1_sigma3 = rep(0, num_sims)


simulate = function(num_sims, sigma){
  beta_hat_1 = rep(0, num_sims)
  
  #prepareing all the beta_hat vectors
  beta_hat_0 = rep(0, num_sims)
  beta_hat_2 = rep(0, num_sims)
  beta_hat_3 = rep(0, num_sims)
  beta_hat_4 = rep(0, num_sims)
  
  
  
  se         = rep(0, num_sims)
  se_2       = rep(0, num_sims)
  se_3       = rep(0, num_sims)
  y_hat      = rep(0, n)
  
  y_hat_00   = rep(0, num_sims)
  y_hat_00_2   = rep(0, num_sims)
  y_hat       = rep(0, num_sims)
  for(i in 1:num_sims) {
    
    eps = rnorm(n, mean = 0 , sd = sigma)
    #makes n=15 values, populate
    sim_data$y = beta_0 * x0 + beta_1 * x1 + beta_2 * x2 + beta_3 * x3 + beta_4 * x4 + eps
    
    
    fit = lm(y ~ x1 + x2 + x3 + x4, data = sim_data)
    beta_hat_1[i] = coef(fit)[2]
    
    #grabbing all the beta hat values
    beta_hat_0[i] = coef(fit)[1]
    beta_hat_2[i] = coef(fit)[3]
    beta_hat_3[i] = coef(fit)[4]
    beta_hat_4[i] = coef(fit)[5]
    
    (beta_hat = solve(t(X) %*% X) %*% t(X) %*% sim_data$y)
    
    
    #calculating se 2 different ways
    y_hat = X %*% solve(t(X) %*% X) %*% t(X) %*% sim_data$y
    e     = sim_data$y - y_hat
    
    se_3[i] = summary(fit)$sigma
    se_2[i] = sqrt(t(e) %*% e / (n - p))
    se[i] = sqrt(sum(fit$residuals^2)/(n - p))
    
    y_hat_00[i] = x_00 %*% beta_hat
    y_hat_00_2[i] = beta_hat_0[i] + (beta_hat_1[i] * -3) + (beta_hat_2[i] * 2.5) + (beta_hat_3[i] * 0.5) + (beta_hat_4[i] * 0)
    
  }
  #return beta_hat 1, se, and se_2 (se and se_2 should be equal)
  beta_hat_1
  data.frame(beta_hat_1 = beta_hat_1, se = se, se_2 = se_2, se_3 = se_3, beta_hat_0 = beta_hat_0, beta_hat_1 = beta_hat_1, beta_hat_2 = beta_hat_2, beta_hat_3 = beta_hat_3, beta_hat_4 = beta_hat_4, y_hat_00 = y_hat_00, y_hat_00_2 = y_hat_00_2 )
}


#simulate for the first 3,000 for a given sigma value
sigma_1
sim_data_1  = simulate(num_sims, sigma = sigma_1)


head(sim_data_1)
mean(sim_data_1$beta_hat_1)   # empirical mean
beta_1                        # true mean
var(sim_data_1$beta_hat_1)    # empirical variance
sigma ^ 2 * C[1 + 1, 1 + 1]   # true variance
sigma_1 ^ 2 * C[1 + 1, 1 + 1] # true variance

mean(sim_data_1$se)
#E[se] = sigma square, is 1.


#Charts for the first case when Sigma = 1
hist(sim_data_1$beta_hat_1, prob = TRUE, breaks = 20,
     xlab = expression(hat(beta)[1]), main = "", border = "dodgerblue")

curve(dnorm(x, mean = beta_1, sd = sqrt(sigma_1 ^ 2 * C[1 + 1, 1 + 1])),
      col = "darkorange", add = TRUE, lwd = 3)

#Verify Probabilities to make sure it follows the normal distribution






#beta_2_hat_alt = replicate(n = num_simuation, sim_beta_hat2())



#plot for the se^2?
boxplot(sim_data_1$beta_hat_1)


#Charts expected value?
hist(sim_data_1$y_hat_00, prob = TRUE, breaks = 40,
     xlab = expression(E[hat(y)[00]]), main = "", border = "dodgerblue")

#with a t distribution?
#curve(dnorm(x, mean = beta_1, sd = sqrt(sigma_1 ^ 2 * C[1 + 1, 1 + 1])),
#      col = "darkorange", add = TRUE, lwd = 3)
















mean(beta_hat_1_sigma1)
mean(beta_1)

#variance from simulation
var(beta_hat_1_sigma1)
#expected variance
#expected variance
(var_beta_1 = (sigma_1 ^ 2) * C[1 + 1, 1 + 1])

#\hat{\beta}_1 \sim N\left(\beta_1, \sigma^2 C_{11} \right)






beta_hat_1_sigma2 = simulate(num_sims, sigma = sigma_2)

beta_hat_1_sigma3 = simulate(num_sims, sigma = sigma_3)
head(sim_data)
simulate(beta_hat_1.sigma2, sigma = sigma_2)
simulate(beta_hat_1.sigma3, sigma = sigma_3)

#check the simulated values again the real values
mean(beta_hat_1.sigma1)

replicate(3000, )


(beta_hat = C %*% t(X) %*% y)

#these values are the same as the beta_hat values
coef(lm(y ~ x1 + x2, data = sim_data))

#values should be close to what we expect
c(beta_0, beta_1, beta_2) #actual values

#calculate the fitted values to calulate se, which should be the same thing as is returned with the lm function

y_hat = X %*% beta_hat
(s_e = sqrt(sum((y - y_hat) ^ 2) / (n - p)))

summary(lm(y ~ x1 + x2, data = sim_data))$sigma

#expect it to follow a normal distribution
#\hat{\beta}_2 \sim N\left(\beta_2, \sigma^2 C_{22}  \right)
