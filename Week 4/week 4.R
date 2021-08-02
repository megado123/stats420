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
full_mpg_model = lm(mpg ~ wt + year, data = autompg)
null_mpg_model = lm(mpg ~ 1, data = autompg)
anova(null_mpg_model, full_mpg_model)
anova(null_mpg_model, full_mpg_model)$F[2]

#manually calculating
(p = length(coef(full_mpg_model)))
n = nrow(autompg)
# SSReg
sum((fitted(full_mpg_model) - fitted(null_mpg_model)) ^ 2)
# SSE
sum(resid(full_mpg_model) ^ 2)
(F = (SSReg/(p-1)) / (SSE/ (n-p)))

#grabbed from anova
anova(null_mpg_model, full_mpg_model)$F[2]



nutrition = read_csv("nutrition.csv")
null_nut_model = lm(Calories ~ 1, data = nutrition)
full_nut_model = lm(Calories ~ Carbs + Fat + Protein, data = nutrition)
anova(null_nut_model, full_nut_model)$F[2]

#manually calculating
p = length(coef(full_nut_model))
n = nrow(nutrition)
SSReg = sum((fitted(full_nut_model) - fitted(null_nut_model)) ^ 2)
SSE = sum(resid(full_nut_model) ^ 2)
(F = (SSReg/(p-1)) / (SSE/ (n-p)))
