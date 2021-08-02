install.packages("knitr")

library(readr)
library("knitr")

goalies = read_csv("goalies.csv")

nrow(goalies)

#goalies <- na.omit(goalies)
#nrow(goalies)

dfMIN = goalies[, c("MIN", "W")]
nrow(dfMIN)
dfMIN = na.omit(dfMIN)
nrow(dfMIN)




sim_model_minutes = lm(W ~ MIN, data = dfMIN)
length(sim_model_minutes$fitted.values)
dfGAA = goalies[, c("GAA", "W")]
sim_model_GAA = lm(W ~ GAA, data = dfGAA)
dfSO = goalies[ , c("SO", "W")]
sim_model_SO = lm(W ~ SO, data = dfSO)

#Question 5 Code
goalies = read_csv("goalies.csv")

get_clean_df = function(dataframe, predictor, response ){
  df = dataframe[, c(predictor, response)]
  df = na.omit(df)
}

get_rmse = function(yi,  yi_hat){
  sqrt(sum((yi-yi_hat)^2) * (1/length(yi)))
}

rmse      = rep(0, 3)
r_squared = rep(0,3)

#did it this way so yi and yi_hat would always match up.
df           = get_clean_df(goalies, "MIN", "W")
model        = lm(W ~MIN, data = df1)
r_squared[1] = summary(model)$r.squared
rmse[1]      = get_rmse(df$W, model$fitted.values)

df           = get_clean_df(goalies, "GAA", "W")
model        = lm(W ~ GAA, data = df)
r_squared[2] = summary(model)$r.squared
rmse[2]      = get_rmse(df$W, model$fitted.values)

df           = get_clean_df(goalies, "SO", "W")
model        = lm(W ~ SO, data = df)
r_squared[3] = summary(model)$r.squared
rmse[3]      = get_rmse(df$W, model$fitted.values)

predictor = c("minutes", "goals against", "shutouts")
q5 =  data.frame(predictor, r_squared, rmse)
kable(q5, format = "markdown", padding = 0)


#try this - ask about in office hours

goalies = read_csv("goalies.csv")

get_rmse = function(yi,  yi_hat){
  sqrt(sum((yi-yi_hat)^2) * (1/length(yi)))
}

get_clean_df = function(dataframe, predictor, response ){
  df = dataframe[, c(predictor, response)]
  df = na.omit(df)
  model = lm(response ~ predictor, data = df)
  get_rmse(df$response, model$fitted.values)
}


try1 = get_clean_df(goalies, "MIN", "W")
#model = lm(W ~MIN, data = df)
#get_rmse(df$W, model$fitted.values)





