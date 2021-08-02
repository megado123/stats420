library(faraway)
pairs(Boston, col = "dodgerblue")

model = lm(medv ~ (1/crim), data = Boston)

summary(model)

plot(medv ~ crim, data = Boston)

plot(medv ~ age, data = Boston)

summary(lm(medv ~ ., data = Boston))

#log(crime), (zn shouldn't be in - if in, make linear)
#log of indus
#chas - handle with a dummy variable - make sure to make it be a factor in the data
#nox to the 5th
#rm is linear
#age should be linear?  proably shouldn't be in the model
#I(dis^-2)



#Including all interaction terms is way better than leaps?
updatedData = Boston
updatedData$chas = as.factor(Boston$chas)
updatedData$rad = as.factor(Boston$rad)
updatedData$tax = as.factor(Boston$tax)

test = lm(medv ~ (.) ^ 2 + I(lstat ^ 2) + log(crim) + I(crim ^ 2) + log(indus), data = Boston)

base_mod_back_aic = step(test, direction = "backward", trace = 0)
calc_loocv_rmse(base_mod_back_aic) 
bptest(base_mod_back_aic)$p.value
#0.005292


test = lm(medv ~ (.) ^ 2 + I(lstat ^ 2) + I(lstat ^ 3) + + I(lstat ^ 4) + log(crim) + I(crim ^ 2) + log(indus), data = Boston)
base_mod_back_aic = step(test, direction = "backward", trace = 0)
calc_loocv_rmse(base_mod_back_aic) 
bptest(base_mod_back_aic)$p.value





View(Boston)
#lstat -  y ~ I(lstat ^ - 1)?
#include all predictors - includes all the interactions


#Do the log of crime
plot(medv ~ crim, data = Boston, cex = 2, pch = 20)
model = lm(medv ~ log(crim), data = Boston)
xplot = seq(0, 80, by = 1)
lines(xplot, predict(model, newdata = data.frame(crim = xplot)),
      col = "dodgerblue", lwd = 2, lty = 1)


#probably shouldn't be in?
plot(medv ~ zn, data = Boston, cex = 2, pch = 20)
model = lm(medv ~ zn, data = Boston)
xplot = seq(0, 80, by = 1)
lines(xplot, predict(model, newdata = data.frame(zn = xplot)),
      col = "dodgerblue", lwd = 2, lty = 1)

#indus - log transformation?
plot(medv ~ indus, data = Boston, cex = 2, pch = 20)
model = lm(medv ~ log(indus), data = Boston)
xplot = seq(0, 80, by = 1)
lines(xplot, predict(model, newdata = data.frame(indus = xplot)),
      col = "dodgerblue", lwd = 2, lty = 1)

plot(medv ~ indus, data = Boston, cex = 2, pch = 20)
model = lm(medv ~ log(indus), data = Boston)

#chas - make sure to make it be a facotr?
data_updated = Boston
data_updated$chas = as.factor(data_updated$chas)
plot(medv ~ chas, data = data_updated, cex = 2, pch = 20)
model = lm(medv ~ chas, data = data_updated)
levels(data_updated$chas)


#nox - 
plot(medv ~ nox, data = Boston, cex = 2, pch = 20)
model7 = lm(medv ~ I(nox ^ -7), data = Boston)
xplot = seq(0, .8, by = .1)
lines(xplot, predict(model, newdata = data.frame(nox = xplot)),
      col = "dodgerblue", lwd = 2, lty = 1)

plot(medv ~ nox, data = Boston, cex = 2, pch = 20)
model5 = lm(medv ~ I(nox ^ -5), data = Boston)
xplot = seq(0, .8, by = .1)
lines(xplot, predict(model, newdata = data.frame(nox = xplot)),
      col = "dodgerblue", lwd = 2, lty = 1)

#rm
plot(medv ~ rm, data = Boston, cex = 2, pch = 20)
model = lm(medv ~ rm, data = Boston)
xplot = seq(0, 8, by = 1)
lines(xplot, predict(model, newdata = data.frame(rm = xplot)),
      col = "dodgerblue", lwd = 2, lty = 1)

#age
plot(medv ~ age, data = Boston, cex = 2, pch = 20)
model = lm(medv ~ age, data = Boston)
xplot = seq(0, 100, by = 1)
lines(xplot, predict(model, newdata = data.frame(age = xplot)),
      col = "dodgerblue", lwd = 2, lty = 1)

#dis
plot(medv ~ dis, data = Boston, cex = 2, pch = 20)
model = lm(medv ~ I(dis^-2), data = Boston)
xplot = seq(0, 12, by = 1)
lines(xplot, predict(model, newdata = data.frame(dis = xplot)),
      col = "dodgerblue", lwd = 2, lty = 1)

#rad
range(Boston$rad)

plot(medv ~ rad, data = Boston, cex = 2, pch = 20)
model = lm(medv ~ I(dis^-2), data = Boston)
xplot = seq(0, 12, by = 1)
lines(xplot, predict(model, newdata = data.frame(dis = xplot)),
      col = "dodgerblue", lwd = 2, lty = 1)
