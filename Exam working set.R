?airquality

#r squared
#wind = y value
model = lm(Wind ~ Temp, data = airquality)

summary(model)

y = airquality$Wind
y_hat = model$fitted.values
SST = sum((y - mean(y)) ^ 2)
SSReg = sum((y_hat - mean(y)) ^ 2)
SSE = sum((y - y_hat) ^ 2)

SSE/SST

model = lm(Wind ~ Temp, data = airquality)

summary(model)
predict(model, newdata = data.frame(Temp = c(40, 80)))

40 %in% unique(airquality$Temp)
80 %in% unique(airquality$Temp)

48 + 16


confint(model, level = 0.90)[1,1]

#for the mean response
new_speeds = data.frame(Temp = 85)
predict(model, newdata = new_speeds,
        interval = c("confidence"), level = 0.90)[1, 2]

model = lm(Wind ~ Temp, data = airquality)
new_speeds = data.frame(Temp = 82)
predict(model, newdata = new_speeds,
        interval = c("prediction"), level = 0.99)[1,3]


model = lm(Wind ~ Temp, data = airquality)
summary(model)$coef
beta_1_hat = summary(model)$coef[2,1]
SE = summary(model)$coef[2,2]
t = (beta_1_hat - (-0.125))/SE
2 * pt(abs(t), df = length(resid(model)) - 2, lower.tail = FALSE)


model = lm(Wind ~ Temp, data = airquality)


model = lm(Girth ~ Height, data = trees)

n = length(summary(model)$residuals)

sum((trees$Girth - model$fitted.values)^ 2) * (1/n)


pt(2, mean = 1, sd = 2)


y = 1 + (1.5 * .5) + (2 * 1) + (3 * 1.5) + (2.5 * 2)
1 - pnorm(14, mean = y, sd = 2)

p = 6
n = 40
f = 2.7

?LifeCycleSavings

model = lm()