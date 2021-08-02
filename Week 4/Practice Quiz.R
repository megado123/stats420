#longley 
View(longley)
?longley
long_model = lm(Employed ~ GNP + Population + Armed.Forces, data = longley)
summary(long_model)
confint(long_model, level = 0.90)[2,1]

long_model = lm(Employed ~ GNP + Population + Armed.Forces, data = longley)
summary(long_model)$coef
summary(long_model)$coef["Population", "Std. Error" ]

long_model = lm(Employed ~ GNP + Population + Armed.Forces, data = longley)
summary(long_model)$coef["Armed.Forces", "Pr(>|t|)" ]

summary(long_model)

anova(long_model)$coef


null_long_model = lm(Employed ~ 1, data = longley)
full_long_model = lm(Employed ~ GNP + Population + Armed.Forces, data = longley)
anova(null_long_model, full_long_model)[2, "F"]


set.seed(42)
x_values = data.frame(
  x1 = runif(15),
  x2 = runif(15),
  x3 = runif(15)
)

temp = as.vector(x_values)
x0 = rep(1, 15)
X =  cbind(x0, temp$x1, temp$x2, temp$x3)
C = solve(t(X) %*% X)
C[1+2, 1+2] * sigma

sqrt(sigma^2 * C[2+1, 2+1])


x0 = rep(1, 15)
X =  cbind(x0, as.vector(x_values))
C = solve(t(X) %*% X)
(C[1+2, 1+2] * sigma)



