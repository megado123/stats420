#Homework 4, Question 5

set.seed(1337)
n = 20
p = 5
beta_0 = 1
beta_1 = 2.5
beta_2 = 0
beta_3 = 4
beta_4 = 1
sigma = 4


x0 = rep(1, 20)
x1 = runif(20, min = 0, max = 5)
x2 = runif(20, min = 0, max = 10)
x3 = rnorm(n = 20, mean = 0, sd = 1)
x4 = rnorm(n = 20, mean = 0, sd = 2)
(X = cbind(x0, x1, x2, x3, x4))
C = solve(t(X) %*% X)
y = rep(0, 20)
sim_data = data.frame(x1, x2, x3, x4, y)

C

#report out the diagnal of C
diag(C)

#report out sim_data row 10
sim_data[10, ]


num_sims = 2000
beta_hat_1 = rep(0, 1)
beta_hat_0_pval = rep(0, 1)
beta_hat_1_pval = rep(0, 1)
beta_hat_2_pval = rep(0, 1)
beta_hat_3_pval = rep(0, 1)
beta_hat_4_pval = rep(0, 1)



for(i in 1:num_sims) {
  eps = rnorm(n, mean = 0 , sd = sigma)
  sim_data$y = beta_0 * x0 + beta_1 * x1 + beta_2 * x2 + beta_3 * x3 + beta_4 * x4 + eps
  fit = lm(y ~ x1 + x2 + x3 + x4, data = sim_data)
  beta_hat_1[i]    = coef(fit)[2]
  beta_hat_0_pval[i]      = summary(fit)$coefficients[1, "Pr(>|t|)"]
  beta_hat_1_pval[i]      = summary(fit)$coefficients[2, "Pr(>|t|)"]
  beta_hat_2_pval[i]      = summary(fit)$coefficients[3, "Pr(>|t|)"]
  beta_hat_3_pval[i]      = summary(fit)$coefficients[4, "Pr(>|t|)"]
  beta_hat_4_pval[i]      = summary(fit)$coefficients[5, "Pr(>|t|)"]
}

sigma ^ 2 * C[1 + 1, 1 + 1]

#e
mean(beta_hat_1)
mean(beta_1)

var(beta_hat_1)
(var_beta_1 = (sigma ^ 2) * C[1 + 1, 1 + 1])


hist(beta_hat_1, prob = TRUE, breaks = 20,
     xlab = expression(hat(beta)[1]), main = "", border = "dodgerblue")

curve(dnorm(x, mean = beta_1, sd = sqrt(sigma ^ 2 * C[1 + 1, 1 + 1])),
      col = "darkorange", add = TRUE, lwd = 3)

mean(beta_hat_1_pval < .05) #most are less than .05, so reject Ho, since there is a relationship
mean(beta_hat_1_pval < .05)
mean(beta_hat_2_pval < .05) #few are greater than .05, so we would accept Ho, and given B2 = 0 this makes sense
mean(beta_hat_3_pval < .05)
mean(beta_hat_4_pval < .05)


#f - what proportion of the p-values for beta_3_pval are less and .05, is this what you would expect?

mean(beta_hat_3_pval < .05)

#This is not what I expected, this would imply that I should accept Ho meaning that beta_3 does not play a signficant roll

#There is a significant relationship between between b3 and the response for given values of the x values, so we would reject Ho, meaning
#there is a significant relationship, so this is what I would expect

#g

mean(beta_hat_2_pval < .05)

#This is what I would expect given we know that there is a 



head(sim_data, 10)
eps = rnorm(n, mean = 0, sd = sigma)
y = beta_0 + beta_1 * x1 + beta_2 * x2 + eps
C[1+2, 1+2] * sigma
