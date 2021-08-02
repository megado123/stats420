#Practice Quiz

View(ToothGrowth)
?ToothGrowth

tooth_dose_add = lm(len ~ dose + supp,data = ToothGrowth)

tooth_dose_int = lm(len ~ dose + supp + dose:supp, data = ToothGrowth)

tooth_dose_int2 = lm(len ~  dose * supp,  data = ToothGrowth)

beta_0 = coef(summary(tooth_dose_int))[1,1]
beta_1 = coef(summary(tooth_dose_int))[2,1]
beta_2 = coef(summary(tooth_dose_int))[3,1]
beta_3 = coef(summary(tooth_dose_int))[4,1]

beta_1 + beta_3

(summary(tooth_dose_int))$coefficients[4, 4]

str(ToothGrowth)

ToothGrowth$dose = as.factor(ToothGrowth$dose)

levels(ToothGrowth$dose)

model = lm(len ~ dose + supp, data = ToothGrowth)
beta_0 = coef(summary(model))[1, 1]
beta_1 = coef(summary(model))[2, 1]
beta_2 = coef(summary(model))[3, 1]
beta_3 = coef(summary(model))[4, 1]

beta_1 - beta_2


#Question 5
v1 = 1 * as.numeric(ToothGrowth$dose == "0.5")
v2 = 1 * as.numeric(ToothGrowth$dose == "1")
v3 = 1 * as.numeric(ToothGrowth$dose == "2")
v4 = 1 * as.numeric(ToothGrowth$supp == "VC")

model = lm(len ~ 0 + v1 + v2 + v3 + v4, data = ToothGrowth)
coef(summary(model))[4,1]
