?swiss
View(swiss)

rm(list=ls())

model = lm(Fertility ~ ., data = swiss)
model_partial = lm(Fertility ~ ., data = swiss)





summary(model)


summary(model)$fstatistic["value"]


new_data = data.frame(Agriculture = 54.0, Examination = 23.0, Education = 13.0, Catholic = 60.0, Infant.Mortality = 24.0 )

predict(model, newdata = new_data, interval = "prediction", level = 0.99)[1,1]


model = lm(Fertility ~ ., data = swiss)
confint(model, level = 0.99)[5,2]


new_data = data.frame(Agriculture = 40.0, Examination = 28.0, Education = 10, Catholic = 42.0, Infant.Mortality = 27.0 )
predict(model, newdata = new_data, interval = "confidence", level = 0.95)[1,2]

model = lm(Fertility ~ ., data = swiss)
info = summary(model)
info$coefficients["Examination", "Pr(>|t|)"]



null_model = lm(Fertility ~ Agriculture + Examination, data = swiss)
full_model = lm(Fertility ~ ., data = swiss)
anova(null_mpg_model, full_mpg_model)[2,6]





null_model = lm(Fertility ~ Education + Catholic + Infant.Mortality , data = swiss)
full_model = lm(Fertility ~ Education + Catholic + Infant.Mortality + Agriculture + Examination, data = swiss)
anova(null_model, full_model)



null_model = lm(Fertility ~ Education + Catholic + Infant.Mortality , data = swiss)
full_model = lm(Fertility ~ ., data = swiss)
anova(null_model, full_model)


n = nrow(swiss)
p = length(coef(full_model))
n-p
1 - pf(3.0891, df1 = 2, df2 = 41)
