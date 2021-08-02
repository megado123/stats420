gen_exact_collin_data = function(num_samples = 100) {
  x1 = rnorm(n = num_samples, mean = 80, sd = 10)
  x2 = rnorm(n = num_samples, mean = 70, sd = 5)
  x3 = 2 * x1 + 4 * x2 + 3
  y = 3 + x1 + x2 + rnorm(n = num_samples, mean = 0, sd = 1)
  data.frame(y, x1, x2, x3)
}

set.seed(42)
exact_collin_data = gen_exact_collin_data()
head(exact_collin_data)

exact_collin_fit = lm(y ~ x1 + x2 + x3, data = exact_collin_data)
summary(exact_collin_fit)

X = cbind(1, as.matrix(exact_collin_data[,-1]))
solve(t(X) %*% X)

library(faraway)
pairs(seatpos, col = "dodgerblue")

round(cor(seatpos), 2)

hip_model = lm(hipcenter ~ ., data = seatpos)
summary(hip_model)

ht_shoes_model = lm(HtShoes ~ . - hipcenter, data = seatpos)
summary(ht_shoes_model)$r.squared

vif(hip_model)


set.seed(1337)
noise = rnorm(n = nrow(seatpos), mean = 0, sd = 5)
hip_model_noise = lm(hipcenter + noise ~ ., data = seatpos)

coef(hip_model)
coef(hip_model_noise)

plot(fitted(hip_model), fitted(hip_model_noise), col = "dodgerblue", pch = 20,
     xlab = "Predicted, Without Noise", ylab = "Predicted, With Noise", cex = 1.5)
abline(a = 0, b = 1, col = "darkorange", lwd = 2)

#smaller model
hip_model_small = lm(hipcenter ~ Age + Arm + Ht, data = seatpos)
summary(hip_model_small)

vif(hip_model_small)

anova(hip_model_small, hip_model)

ht_shoes_model_small = lm(HtShoes ~ Age + Arm + Ht, data = seatpos)


plot(resid(hip_model_small) ~ resid(ht_shoes_model_small), 
     col = "dodgerblue", pch = 20,
     xlab = "Residuals, Added Predictor", 
     ylab = "Residuals, Original Model")
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
abline(lm(resid(hip_model_small) ~ resid(ht_shoes_model_small)),
       col = "darkorange", lwd = 2)


###############
set.seed(42)
beta_0 = 7
beta_1 = 3
beta_2 = 4
sigma  = 5

sample_size = 10
num_sim     = 2500

x1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
x2 = c(1, 2, 3, 4, 5, 7, 6, 10, 9, 8)

c(sd(x1), sd(x2))

cor(x1, x2)

true_line_bad = beta_0 + beta_1 * x1 + beta_2 * x2
beta_hat_bad  = matrix(0, num_sim, 2)
mse_bad       = rep(0, num_sim)

for (s in 1:num_sim) {
  y = true_line_bad + rnorm(n = sample_size, mean = 0, sd = sigma)
  reg_out = lm(y ~ x1 + x2)
  beta_hat_bad[s, ] = coef(reg_out)[-1]
  mse_bad[s] = mean(resid(reg_out) ^ 2)
}

####################
z1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
z2 = c(9, 2, 7, 4, 5, 6, 3, 8, 1, 10)

c(sd(z1), sd(z2))

cor(z1, z2)


true_line_good = beta_0 + beta_1 * z1 + beta_2 * z2
beta_hat_good  = matrix(0, num_sim, 2)
mse_good       = rep(0, num_sim)

for (s in 1:num_sim) {
  y = true_line_good + rnorm(n = sample_size, mean = 0, sd = sigma)
  reg_out = lm(y ~ z1 + z2)
  beta_hat_good[s, ] = coef(reg_out)[-1]
  mse_good[s] = mean(resid(reg_out) ^ 2)
}

#plotting them
par(mfrow = c(1, 2))
hist(beta_hat_bad[, 1],
     col = "darkorange",
     border = "dodgerblue",
     main = expression("Histogram of " *hat(beta)[1]* " with Collinearity"),
     xlab = expression(hat(beta)[1]),
     breaks = 20)
hist(beta_hat_good[, 1],
     col = "darkorange",
     border = "dodgerblue",
     main = expression("Histogram of " *hat(beta)[1]* " without Collinearity"),
     xlab = expression(hat(beta)[1]),
     breaks = 20)

mean(beta_hat_bad[, 1])

mean(beta_hat_good[, 1])

sd(beta_hat_bad[, 1])

sd(beta_hat_good[, 1])


par(mfrow = c(1, 2))
hist(beta_hat_bad[, 2],
     col = "darkorange",
     border = "dodgerblue",
     main = expression("Histogram of " *hat(beta)[2]* " with Collinearity"),
     xlab = expression(hat(beta)[2]),
     breaks = 20)
hist(beta_hat_good[, 2],
     col = "darkorange",
     border = "dodgerblue",
     main = expression("Histogram of " *hat(beta)[2]* " without Collinearity"),
     xlab = expression(hat(beta)[2]),
     breaks = 20)

#means are close, but higher variance when they are corrlated.
mean(beta_hat_bad[, 2])
mean(beta_hat_good[, 2])

sd(beta_hat_bad[, 2])
sd(beta_hat_good[, 2])

par(mfrow = c(1, 2))
hist(mse_bad,
     col = "darkorange",
     border = "dodgerblue",
     main = "MSE, with Collinearity",
     xlab = "MSE")
hist(mse_good,
     col = "darkorange",
     border = "dodgerblue",
     main = "MSE, without Collinearity",
     xlab = "MSE")

mean(mse_bad)
mean(mse_good)
