#MLR in R

# read the data from the web
autompg = read.table(
  "http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data",
  quote = "\"",
  comment.char = "",
  stringsAsFactors = FALSE)
# give the dataframe headers
colnames(autompg) = c("mpg", "cyl", "disp", "hp", "wt", "acc", "year", "origin", "name")
# remove missing data, which is stored as "?"
autompg = subset(autompg, autompg$hp != "?")
# remove the plymouth reliant, as it causes some issues
autompg = subset(autompg, autompg$name != "plymouth reliant")
# give the dataset row names, based on the engine, year and name
rownames(autompg) = paste(autompg$cyl, "cylinder", autompg$year, autompg$name)
# remove the variable for name, as well as origin
autompg = subset(autompg, select = c("mpg", "cyl", "disp", "hp", "wt", "acc", "year"))
# change horsepower from character to numeric
autompg$hp = as.numeric(autompg$hp)
# check final structure of data
str(autompg)

#response = mpg, as a function of wt and year
#list the variable that we would like to include in the model
mpg_model = lm(mpg ~ wt + year, data = autompg)

summary(mpg_model)

#individual tests

summary(mpg_model)$coef
summary(mpg_model)$coef["year", "Pr(>|t|)"] #p-value when testing for B2
#testing for weight, is not significance of regression
#is weight a signmifcant predictor when year is in the model

#interval estimates
confint(mpg_model, level = 0.99)
#note year does not include 0, so alpha at 0.01, it does not contain 0, so reject

#create data for new cars
new_cars = data.frame( wt = c(3500, 5000), year = c(76, 81))
new_cars

#CI for mean response
predict(mpg_model, newdata = new_cars, interval = "confidence", level = 0.99)

#PI for new observation
predict(mpg_model, newdata = new_cars, interval = "prediction", level = 0.99)

#hidden extrapolation
new_cars$wt
range(autompg$wt)

new_cars$year
range(autompg$year)

plot(year ~ wt, data = autompg, pch = 20, col = "dodgerblue", cex  = 1.5)
points(new_cars, col = "darkorange", cex = 3, pch = "X")

plot(year ~ wt, data = autompg, pch = 20, col = "dodgerblue", cex = 1.5)
points(new_cars, col = "darkorange", cex = 3, pch = "X")

#get critical value at confidence interval

confint(mpg_model, level = 0.99, parm = "wt")

summary(mpg_model)$coef

est = summary(mpg_model)$coef["wt", "Estimate"]
(se  = summary(mpg_model)$coef["wt", "Std. Error"])

(n = nrow(autompg))
(p = length(coef(mpg_model)))
(df = n - p)

#CI = .99
#probabliy (1 - 0.99)/2
#manually getting critical value for CI
crit = abs(qt(0.005, df = df))

c(est - crit * se, est + crit * se)
confint(mpg_model, level = 0.99, parm = "wt")
