library(readr)
epa = read_csv("epa2015.csv")
epa$type = as.factor(epa$type)
str(epa)
head(epa)
#type is char - need to make into a factor
unique(epa$type)

# change type to a factor variable
epa$type = as.factor(epa$type)

as.numeric(epa$type)

head(epa, 100)

View(epa)
#1a. 

#scatter plot
plot(CO2 ~ horse, data = epa, col = as.numeric(epa$type), pch = as.numeric(epa$type), cex = .5)

plot(CO2 ~ horse, data = epa,
     xlab = "Horsepower foot-lbs/sec",
     ylab = "CO2 g/mi",
     main = "CO2 vs Horsepower",
     pch =  as.numeric(epa$type) + 1,
     cex = .5,
     col = as.numeric(epa$type))
#SLR
epa_slr = lm(CO2 ~ horse, data = epa)

abline(epa_slr, lwd = 2, col = "DodgerBlue")

new_car = data.frame(horse = 148)
predict(epa_slr, newdata = new_car,
        interval = c("prediction"), level = 0.99)

epa_add = lm(CO2 ~ horse + type, data = epa)

summary(epa_add)

beta_0_hat = coefficients(summary(epa_add))[1,1]
beta_1_hat = coefficients(summary(epa_add))[2,1]
beta_2_hat = coefficients(summary(epa_add))[3,1]
beta_3_hat = coefficients(summary(epa_add))[4,1]

int_both  = beta_0_hat
int_car   = beta_0_hat + beta_2_hat
int_truch = beta_0_hat + beta_3_hat

slope_all = beta_1_hat

#1c.
#Interactive Model
epa_int = lm(CO2 ~ horse * type, data = epa)
beta_0_hat = coefficients(summary(epa_int))[1,1]
beta_1_hat = coefficients(summary(epa_int))[2,1]
beta_2_hat = coefficients(summary(epa_int))[3,1]
beta_3_hat = coefficients(summary(epa_int))[4,1]
beta_4_hat = coefficients(summary(epa_int))[5,1]
beta_5_hat = coefficients(summary(epa_int))[6,1]

int_both    = beta_0_hat
int_car     = beta_0_hat + beta_2_hat
int_truck   = beta_0_hat + beta_3_hat
slope_both  = beta_1_hat
slope_car   = beta_1_hat + beta_4_hat
slope_truck = beta_1_hat + beta_5_hat

