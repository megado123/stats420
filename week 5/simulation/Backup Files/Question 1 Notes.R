set.seed(1337)
n = 100 # sample size
p = 3
beta_0 = 5
beta_1 = -2
beta_2 = 6
sigma = 4

x0 = rep(1, n)
x1 = sample(seq(1, 10, length = n))
x2 = sample(seq(1, 10, length = n))
X = cbind(x0, x1, x2)
C = solve(t(X) %*% X)

eps = rnorm(n, mean = 0, sd = sigma)
(y = beta_0 + beta_1 * x1 + beta_2 * x2 + eps)
sim_data = data.frame(x1, x2, y)

(beta_hat = C %*% t(X) %*% y)

coef(lm(y ~ x1 + x2, data = sim_data))
c(beta_0, beta_1, beta_2)

(y_hat = X %*% beta_hat)
(s_e = sqrt(sum((y - y_hat) ^ 2) / (n - p)))
