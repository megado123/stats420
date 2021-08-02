#Question 2

library(readr)
hospital = read_csv("hospital.csv")

str(hospital)
#care needs to be changed
#race needs to be changed
hospital$Care = as.factor(hospital$Care)
hospital$Race  = as.factor(hospital$Race)

levels(hospital$Care) #levels are high and low
levels(hospital$Race) #non-white, white

#1b
hosp_add = lm(Days ~ Charges + Pressure + Care + Race, data = hospital)


hosp_int = lm(Days ~ Charges + Pressure + Care + Race + Care:Charges + Care:Pressure, data = hospital)
anova(hosp_add, hosp_int)

hosp_int2 = lm(Days ~ Charges + Pressure + Care + Race + Care:Charges + Care:Pressure + Race:Charges + Race:Pressure, 
               data = hospital)

beta_0_hat = coefficients(summary(hosp_int2))[1,1]
beta_1_hat = coefficients(summary(hosp_int2))[2,1]
beta_2_hat = coefficients(summary(hosp_int2))[3,1]
beta_3_hat = coefficients(summary(hosp_int2))[4,1]
beta_4_hat = coefficients(summary(hosp_int2))[5,1]
beta_5_hat = coefficients(summary(hosp_int2))[6,1]
beta_6_hat = coefficients(summary(hosp_int2))[7,1]
beta_7_hat = coefficients(summary(hosp_int2))[8,1]
beta_8_hat = coefficients(summary(hosp_int2))[9,1]

beta_1_hat + beta_5_hat

