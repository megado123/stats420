#Quiz 3
rm(list=ls())

#Question 1:
pt(-2.1, 5) * 2

#Question 2:

#below is totally wrong
qt(0.90, df = 2, n = 10) # no way don't do this'

#this is correct
#there were 10 samples
#1 - 0.90 = 0.10/2 = alpha/2 = .05
#crit = qt(0.95, df = 8)
qt(0.95, df = 8)





model = lm(eruptions ~ waiting, data = faithful)
summary(model)
model_data = summary(model)
model_data$coefficients[2,2]
model_data$coefficients[2, 4]

beta_1_data = model_data$coefficients
beta_1_data[2,3]


#verify the standard error
Sxx = sum((faithful$waiting - mean(faithful$waiting))^2)
s_e = summary(model)$sigma
s_e

#Residual Standard Error RSE, R calls sigma
beta_1_hat_se_calculated = s_e/sqrt(Sxx)
beta_1_hat_se_calculated

#Question 5
model = lm(eruptions ~ waiting, data = faithful)
summary(model)
model_data = summary(model)
model_data$coefficients[2, 3]


#Question 7
summary(model)$coefficients
summary(model)$coefficients[2,]

#Question 8
confint(model, level = 0.90)
confint(model, level = 0.90)[1,2]


#Question 9
#Calc a 95% confidence interval for beta_one
#Report the length of the margin of this interval
model = lm(eruptions ~ waiting, data = faithful)
confint(model, level = 0.95)

beta_1_hat = summary(model)$coefficients[2,1]
beta_1_hat
top_value = confint(model, level = 0.95)[2,2]
top_value
margin = top_value - beta_1_hat
margin

lower_value = confint(model, level = 0.95)[2,1]

beta_1_hat - lower_value

#Question 10
#Create a 90% confidence interval for the mean eruption duration for 
#a waiting time of 81 minutes. Report the lower bound of this interval.
model = lm(eruptions ~ waiting, data = faithful)
new_duration = data.frame(waiting = 81)
predict(model, newdata = new_duration, interval = c("confidence"), level = 0.90)
#should be 4.189899

#Question 11
#Create a 99% prediction interval for a new observation's eruption duration 
#for a waiting time of 72 minutes. Report the upper bound of this interval.
new_duration = data.frame(waiting = 72)
predict(model, newdata = new_duration, interval = c("prediction"), level = 0.99)[1,3]

