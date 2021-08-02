#fit model and display summary
stop_dist_model = lm(dist ~ speed, data = cars)

summary(stop_dist_model)

#extract summary information
names(stop_dist_model)

summary(stop_dist_model)$coefficients

#p-value
(p_val_beta_1_from_summary = summary(stop_dist_model)$coefficients[2,4])
summary(stop_dist_model)$coefficients["speed", "Pr(>|t|)"]

#store test statistics - for significance of regression
# t = beta_1_hat - 0/SE[beta_1_hat]
(beta_0_hat_t = summary(stop_dist_model)$coefficients["(Intercept)", "t value"])
(beta_1_hat_t = summary(stop_dist_model)$coefficients["speed", "t value"])

#verify p-values for beta_1_hat
#probably pt function
(p_val_beta_1_calculated = 2 *pt(abs(beta_1_hat_t), df = nrow(cars) - 2, lower.tail = FALSE))

all.equal(p_val_beta_1_from_summary, p_val_beta_1_calculated)

#function to simulate SLR
sim_slr = function(x, beta_0 = 10, beta_1 = 5, sigma = 1) {
  n = length(x)
  epsilon = rnorm(n, mean = 0, sd = sigma)
  y = beta_0 + beta_1 * x + epsilon
  data.frame(predictor = x, response = y)
}

#simulate SLR
set.seed(1)
x = seq(1, 20, length.out = 21)
(sim_data = sim_slr(x = x, beta_0 = 2, beta_1 = 0, sigma = 1))

#fit SLR, perform significance of regression
(sim_fit = lm(response ~ predictor, data = sim_data))

summary(sim_fit)

#look at beta_1
summary(sim_fit)$coefficients["predictor", "Pr(>|t|)"]


