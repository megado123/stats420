install.packages("knitr")

library(readr)
library(knitr)

goalies = read_csv("goalies.csv")



test = lm(W ~ MIN, data = goalies)

sim_model_minutes = lm(W ~ MIN, data = goalies)
sim_model_GAA     = lm(W ~ GAA, data = goalies)
sim_model_SO      = lm(W ~ SO,  data = goalies)


fitted_val = as.vector(sim_model_minutes$fitted.values)


get_rmse = function(yi,  yi_hat){
  sqrt(sum((yi-yi_hat)^2) * (1/length(yi)))
}

names(sim_model_minutes)

predictor = c("minutes", "goals against", "shutouts")
r_squared = c(summary(sim_model_minutes)$r.squared, summary(sim_model_GAA)$r.squared, summary(sim_model_SO)$r.squared)

get_rmse(goalies$W,as.vector(sim_model_minutes$fitted.values))

q5.data <- data.frame(predictor, r_squared)

kable(head(iris), format = "latex")

coef(sim_model_minutes)
coef(sim_model_minutes2)

kable(head(mtcars), format = "markdown", padding = 0)


library(MASS)
View(cats)

Sxy = sum((cats$Bwt - mean(cats$Bwt)) * (cats$Hwt - mean(cats$Hwt)))

Sxy

Sxx = sum((cats$Bwt - mean(cats$Bwt))^2)

beta_1_hat = Sxy/Sxx
