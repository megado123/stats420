View(cars)
?cars
str(cars)
dim(cars) ## 50 observations, 2 variables

plot(dist ~ speed, data = cars,
     xlab = "Speed (in miles per hour",
     ylab = "Stopping Distance (in feet)", 
     main = "Stopping Distance vs Speed",
     pch = 20, 
     cex = 2,
     col = "grey")

# simple linear regression

x = cars$speed
y = cars$dist

Sxy = sum((x - mean(x))*(y - mean(y)))
Sxx = sum((x - mean(x))^ 2)
Syy = sum((y - mean(y))^ 2)

beta_1_hat = Sxy/Sxx
beta_0_hat = mean(y) - (beta_1_hat * mean(x))

c(Sxy, Sxx, Syy)
c(beta_0_hat, beta_1_hat)

plot(dist ~ speed, data = cars,
     xlab = "Speed (in miles per hour",
     ylab = "Stopping Distance (in feet)", 
     main = "Stopping Distance vs Speed",
     pch = 20, 
     cex = 2,
     col = "grey")

abline(beta_0_hat, beta_1_hat, col = "darkorange", lwd = 2)

# predictions

unique(cars$speed)

21 %in% unique(cars$speed)

min(cars$speed) < 21 & 21 < max(cars$speed)

range(cars$speed)

range(cars$speed)[1] < 50 & 50 < range(cars$speed)[2]

beta_0_hat + beta_1_hat * 50

# residuals
# true value - fitted value
which(cars$speed == 8)
cars[5, ]

cars[which(cars$speed == 8), ]
16 - (beta_0_hat + beta_1_hat * 8)

y_hat = beta_0_hat + beta_1_hat * x

e = y - y_hat
n = length(e)
#estimate of variance
s2_e = sum(e^ 2)/(n-2)

plot(x, y_hat)
#residual standard error
s_e = sqrt(s2_e)

#variance decomposition

SST = sum((y - mean(y))^2)
SSReg = sum((y_hat - mean(y))^2)
SSE = sum( (y - y_hat)^ 2)
c(SST = SST, SSReg = SSReg, SSE = SSE)


SSE/(n-2)
s2_e == SSE/ (n-2)

R2 = SSReg / SST

R2

#.65107 65% of variation is explained by linear relationship
