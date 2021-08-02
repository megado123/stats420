make_poly_data = function(sample_size = 11) {
  x = seq(0, 10)
  y = 3 + x + 4 * x ^ 2 + rnorm(n = sample_size, mean = 0, sd = 20)
  data.frame(x, y)
}

set.seed(1234)
poly_data = make_poly_data()



fit_quad = lm(y ~ poly(x, degree = 2), data = poly_data)
fit_big  = lm(y ~ poly(x, degree = 8), data = poly_data)

plot(y ~ x, data = poly_data, ylim = c(-100, 400), cex = 2, pch = 20)
xplot = seq(0, 10, by = 0.1)
lines(xplot, predict(fit_quad, newdata = data.frame(x = xplot)),
      col = "dodgerblue", lwd = 2, lty = 1)
lines(xplot, predict(fit_big, newdata = data.frame(x = xplot)),
      col = "darkorange", lwd = 2, lty = 2)

#RMSE
sqrt(mean(resid(fit_quad) ^ 2))

sqrt(mean(resid(fit_big) ^ 2))

test = lm(y ~ y, data = poly_data)

#leaving out 1 observation
calc_loocv_rmse = function(model) {
  sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
}

calc_loocv_rmse(fit_quad)

calc_loocv_rmse(fit_big)


#removing the 3rd data point
fit_quad_removed = lm(y ~ poly(x, degree = 2), data = poly_data[-3, ])
fit_big_removed  = lm(y ~ poly(x, degree = 8), data = poly_data[-3, ])

plot(y ~ x, data = poly_data, ylim = c(-100, 400), cex = 2, pch = 20)
xplot = seq(0, 10, by = 0.1)
lines(xplot, predict(fit_quad_removed, newdata = data.frame(x = xplot)),
      col = "dodgerblue", lwd = 2, lty = 1)
lines(xplot, predict(fit_big_removed, newdata = data.frame(x = xplot)),
      col = "darkorange", lwd = 2, lty = 2)

#Model Selection involves quality and search
library(faraway)
hipcenter_mod = lm(hipcenter ~ ., data = seatpos)
coef(hipcenter_mod)

names(summary(hipcenter_mod))
#to get AIC for a model
extractAIC(hipcenter_mod)

n = length(resid(hipcenter_mod))
(p = length(coef(hipcenter_mod)))
n * log(mean(resid(hipcenter_mod) ^ 2)) + 2 * p


#Backwards search which defaults to AIC
hipcenter_mod_back_aic = step(hipcenter_mod, direction = "backward")

summary(hipcenter_mod_back_aic)

#Backwards with BIC
n = length(resid(hipcenter_mod))
hipcenter_mod_back_bic = step(hipcenter_mod, direction = "backward", k = log(n))
summary(hipcenter_mod_back_bic)

summary(hipcenter_mod)$adj.r.squared

summary(hipcenter_mod_back_aic)$adj.r.squared

summary(hipcenter_mod_back_bic)$adj.r.squared

calc_loocv_rmse(hipcenter_mod)

calc_loocv_rmse(hipcenter_mod_back_aic)

calc_loocv_rmse(hipcenter_mod_back_bic)

#prefer the model chosen via BIC if using LOOCV RMSE as the metric

#Forward Search AIX
hipcenter_mod_start = lm(hipcenter ~ 1, data = seatpos)
hipcenter_mod_forw_aic = step(
  hipcenter_mod_start, 
  scope = hipcenter ~ Age + Weight + HtShoes + Ht + Seated + Arm + Thigh + Leg, 
  direction = "forward")

summary(hipcenter_mod_forw_aic)

calc_loocv_rmse(hipcenter_mod_forw_aic)

#forward search BIC
hipcenter_mod_forw_bic = step(
  hipcenter_mod_start, 
  scope = hipcenter ~ Age + Weight + HtShoes + Ht + Seated + Arm + Thigh + Leg, 
  direction = "forward", k = log(n))

#note they didn't select the same model
coef(hipcenter_mod_forw_bic)
coef(hipcenter_mod_back_bic)

summary(hipcenter_mod)$adj.r.squared
summary(hipcenter_mod_forw_aic)$adj.r.squared
summary(hipcenter_mod_forw_bic)$adj.r.squared

calc_loocv_rmse(hipcenter_mod)
calc_loocv_rmse(hipcenter_mod_forw_aic)
calc_loocv_rmse(hipcenter_mod_forw_bic)

hipcenter_mod_both_bic = step(
  hipcenter_mod_start, 
  scope = hipcenter ~ Age + Weight + HtShoes + Ht + Seated + Arm + Thigh + Leg, 
  direction = "both", k = log(n))

#Stepwise Search
hipcenter_mod_both_aic = step(
  hipcenter_mod_start, 
  scope = hipcenter ~ Age + Weight + HtShoes + Ht + Seated + Arm + Thigh + Leg, 
  direction = "both")

coef(hipcenter_mod_both_aic )

hipcenter_mod_both_bic = step(
  hipcenter_mod_start, 
  scope = hipcenter ~ Age + Weight + HtShoes + Ht + Seated + Arm + Thigh + Leg, 
  direction = "both", k = log(n))

#checking all models with leaps

library(leaps)
all_hipcenter_mod = summary(regsubsets(hipcenter ~ ., data = seatpos))

all_hipcenter_mod$which

all_hipcenter_mod$rss

all_hipcenter_mod$adjr2

(best_r2_ind = which.max(all_hipcenter_mod$adjr2))

all_hipcenter_mod$which[best_r2_ind, ]

#p and n are the for the full model
p = length(coef(hipcenter_mod))
n = length(resid(hipcenter_mod))
(hipcenter_mod_aic = n * log(all_hipcenter_mod$rss / n) + 2 * (2:p))

(best_aic_ind = which.min(hipcenter_mod_aic))
all_hipcenter_mod$which[best_aic_ind,]

hipcenter_mod_best_aic = lm(hipcenter ~ Age + Ht + Leg, data = seatpos)
extractAIC(hipcenter_mod_best_aic)
extractAIC(hipcenter_mod_back_aic)
extractAIC(hipcenter_mod_forw_aic)
extractAIC(hipcenter_mod_both_aic)

plot(hipcenter_mod_aic ~ I(2:p), ylab = "AIC", xlab = "p, number of parameters", 
     pch = 20, col = "dodgerblue", type = "b", cex = 2,
     main = "AIC vs Model Complexity")
