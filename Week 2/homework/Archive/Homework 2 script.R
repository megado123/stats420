#Homework 2


library(MASS)


#model = lm(response ~ predictor, data = dataset)
#model = lm(yvalues ~ xvalues, data = dataset)


?cats
# cats were all adults over 2 kg
View(cats)
#includes Sex = F or M, BWT = body weight = kg, Hwt = heart weight in g
# note that body weight is in kg, and heart weight is in g

cats$Sex = NULL
?cats
cat_model = lm(Hwt ~ Bwt, data = cats)
summary(cat_model)
View(cats)

range(cats$Bwt)

coef(cat_model)






## manually calculating beta_0_hat and beta_1_hat
x = cats$Bwt
y = cats$Hwt
Sxy = sum((x - mean(x))*(y - mean(y)))
Sxx = sum((x - mean(x))^ 2)
Syy = sum((y - mean(y))^ 2)
(beta_1_hat = Sxy/Sxx)
beta_0_hat = mean(y) - (beta_1_hat * mean(x))
######################################################


plot(Hwt ~ Bwt, data = cats,
     xlab = "Body Weight (in Kg)",
     ylab = "Heart weight (in grams)",
     main = "Cats Heart Weight vs Body Weight",
     pch  = 20,
     cex  = 1,
     col  = "dodgerblue")

abline(cat_model$coefficients["(Intercept)"], cat_model$coefficients["Bwt"], col = "darkorange", lwd = 2)


#1.f
x = cats$Bwt
y = cats$Hwt

Sxy = sum((x - mean(x))*(y - mean(y)))
Sxx = sum((x - mean(x))^ 2)
Syy = sum((y - mean(y))^ 2)

beta_1_hat = Sxy/Sxx
beta_0_hat = mean(y) - (beta_1_hat * mean(x))
y_hat = beta_0_hat + beta_1_hat * x

SST = sum((y - mean(y))^2)
SSReg = sum((y_hat - mean(y))^2)

R2 = SSReg / SST

#1.f

x = cats$Bwt
y = cats$Hwt

Sxy = sum((cats$Bwt - mean(cats$Bwt))*(cats$Hwt - mean(cats$Hwt)))
Sxx = sum((cats$Bwt - mean(cats$Bwt))^ 2)
Syy = sum((cats$Hwt - mean(cats$Hwt))^ 2)

beta_1_hat = Sxy/Sxx
beta_0_hat = mean(y) - (beta_1_hat * mean(cats$Bwt))
y_hat = beta_0_hat + beta_1_hat * x

SST = sum((y - mean(y))^2)
SSReg = sum((y_hat - mean(y))^2)

R2 = SSReg / SST

R2


if (x > y){
  z = x + y
  print("x is larger than y")
} else{
  z = x + 5 * y
  print("x is les than y")
}

#not sure if it should be the standard deviation of the residuals, or the 
#square root of se^2
get_sd_est = function(model_resid, mle = false){
  if (mle == false){
    s_e = sqrt( sum( model_resid^2)/(length(model_resid) - 2))
  } else{
    sigma_hat = sqrt( (sum(model_resid ^ 2)/length(model_resid)) )
  }
}

##Exercise 3


#3a)
birthday = 19810803
set.seed(birthday)
x = runif(n = 50, 0, 10)

num_obs = 50
beta_0 = -4
beta_1 = 2
sigma = sqrt(6.25)
sigma = 2.5

n = length(x)
epsilon = rnorm(n, mean = 0, sd = 2.5)
epsilon

sim_slr = function(x, beta_0, beta_1, sigma) {
  n = length(x)
  epsilon = rnorm(n, mean = 0, sd = sigma)
  y = beta_0 + beta_1 * x + epsilon
  data.frame(predictor = x, response = y)
}

sim_data = sim_slr(x, beta_0, beta_1, sigma)

sim_fit = lm(sim_data$response ~ sim_data$predictor)

coef(sim_fit)[2]

summary(sim_fit)

coef(sim_fit)
plot(sim_data$response, sim_data$predictor, 
     xlab = "predictor",
     ylab = "response",
     main = "Response vs Predictor for simulated data given /n beta_0 = -4, beta_1 = 2, sigma = 2.5",
     pch  = 20,
     cex  = 2,
     col  = "grey")


names(sim_data)

#3b)

sim_model = lm(response ~ predictor, data = sim_data)
coef(sim_model)

#beta_0_hat = -4.767234     beta_1_hat =       2.01557

summary(sim_fit)$r.squared

#They are pretty close to what I had expected.  The value of R^2 being 0.8300166  shows that the
#proportion of ariation in teh response value is explained in the linear linear relation with the 
#predictor variable being pretty good, recall an R^2 value of 1 means no error.


#i didn't use the stuff below
summary(sim_model)$r.squared
epsilon = rnorm(n = num_obs, mean = 0, sd = sigma)
y = beta_0 + beta_1 * x + epsilon
sim_fit = lm(y ~ x)
coef(sim_fit)
plot(y ~ x)
abline(sim_fit)


#c) Plot the simulated dat

plot(response ~ predictor, data = sim_data,
     xlab = "Simulated Predictor Variable",
     ylab = "Simulated Response Variable",
     main = "Simulated Regression Data",
     pch  = 20,
     cex  = 2,
     col  = "grey")
abline(sim_fit, lwd = 3, lty = 1, col = "darkorange")
abline(beta_0, beta_1, lwd = 3, lty = 2, col = "dodgerblue")
legend("topright", c("Estimate", "Truth"), lty = c(1, 2), lwd = 2,
       col = c("darkorange", "dodgerblue"))

sim_fit
sim_data

#function to run 2000 observations with a predefined set of x values 50 of them, 
#generating new 50 y values 2000 times




beta_0 = -4
beta_1 = 2
sigma = sqrt(6.25)

beta_hat_1 = rep(0, 2000)

for (i in 1:2000){
  n = length(x)
  epsilon = rnorm(n, mean = 0, sd = sigma)
  y = beta_0 + beta_1 * x + epsilon
  sim_data = data.frame(predictor = x, response = y)
  sim_model = lm(response ~ predictor, data = sim_data)
  beta_hat_1[i] = coef(sim_model)[2]
}




sim_beta_hat = function(x, beta_0, beta_1, sigma) {
    beta_hat_1 = rep(0, 2000)
    n = length(x)
    for (i in 1:2000){
    epsilon = rnorm(n, mean = 0, sd = sigma)
    y = beta_0 + beta_1 * x + epsilon
    sim_data = data.frame(predictor = x, response = y)
    sim_model = lm(response ~ predictor, data = sim_data)
    beta_hat_1[i] = coef(sim_model)[2]
  }
  
  return(beta_hat_1)
}

beta_hat_1 = sim_beta_hat(x, beta_0, beta_1, sigma)


# part e
mean(beta_hat_1) # 2.000666

sd(beta_hat_1) # 0.1241644, 0.1233576

#The mean looks familliar

#Exercise 4 (Be a skeptic)
#x will be a vector holding 25 random generated numbers from 0-10, for these random x values
#an observation will be done 1500 time
#Fit the SLR model and store beta_hat_1 noting that beta_hat itself is 0

birthday = 19810803
set.seed(birthday)
x = runif(n = 25, 0, 10)
sigma = 1
beta_0 = 10
beta_1 = 0

sim_beta_hat_1 = function(x, beta_0, beta_1, sigma) {
  beta_hat_1 = rep(0, 1500)
  n = length(x)
  for (i in 1:1500){
    epsilon = rnorm(n, mean = 0, sd = sigma)
    y = beta_0 + beta_1 * x + epsilon
    sim_data = data.frame(predictor = x, response = y)
    sim_model = lm(response ~ predictor, data = sim_data)
    beta_hat_1[i] = coef(sim_model)[2]
  }
  
  return(beta_hat_1)
}

beta_hat_1 = sim_beta_hat_1(x, beta_0, beta_1, sigma)

length(beta_hat_1)
range(beta_hat_1)

#b) Plot a historigram of beta_hat_1
#Comment on the shape
#The shape of the histogram follows a normal distribution.  

#Looking at the histogram, you can see that roughly 400 observations were within the
#range of 0-0.05 and 400 obserations were within the range of 0 to -0.05 which 800/1200 is ~67 percent, 
#this would mean that 1 standard deviation for the balue of beta_hat_1 would be .05
  

hist(beta_hat_1,
     xlab = "Beta_hat_1",
     main = "Histogram of $\\hat\\beta$ Values \n over 1500 Observations",
     #breaks = 12, 
     col = "darkorange",
     border = "dodgerblue")

#c) Import the data from skeptic.csv and fit a SLR model
# extract the fitted coeffienct for beta_1

library(readr)
skeptic = read_csv("skeptic.csv")
sim_model_skeptic = lm(response ~ predictor, data = skeptic)

summary(sim_model_skeptic)


coef(sim_model_skeptic)

#The fitted coeffienct for Beta_1_hat = .154081

#d) Re-plot the histogram from(b), Now add a vertical line at the value of Beta_1_hat from part c)

hist(beta_hat_1,
     xlab = "Beta_hat_1",
     main = "Histogram of $\\hat\\beta$ Values \n over 1500 Observations",
     #breaks = 12, 
     col = "darkorange",
     border = "dodgerblue")
abline(v = sim_model_skeptic$coefficients["predictor"], col = "red")

sim_model_skeptic$coefficients["predictor"]

names(summary(sim_model_skeptic))

#Given the equation does not rely on x





#####
#Homework 2 checking
cat_model = lm(Hwt ~ Bwt, data = cats)
summary(cat_model)
#Beta_0_hat = -.3567
#Beta_1_hat = 4.0341

x = cats$Bwt
y = cats$Hwt
sum((x - mean(x)) * (y - mean(y)))/sum((x - mean(x))^2)


birthday = 19810803
set.seed(birthday)
x = runif(n = 50, 0, 10)


#dfnew4 <- diamonds[,c("carat", "depth", "price")]
library(readr)
goalies = read_csv("goalies.csv")

dfMIN = goalies[, c("MIN", "W")]
dfMIN = na.omit(dfMIN)
sim_model_minutes = lm(W ~ MIN, data = dfMIN)
fitted = as.vector(sim_model_minutes$fitted.values)
rmse_MIN = sqrt((1/length(dfMIN$W)) * sum((dfMIN$W - fitted)^2))


dfGAA = goalies[, c("GAA", "W")]
sim_model_GAA = lm(W ~ GAA, data = dfGAA)

dfSO = goalies[ , c("SO", "W")]
sim_model_SO = lm(W ~ SO, data = dfSO)



rmse = function(df) {
  n = length(x)
  epsilon = rnorm(n, mean = 0, sd = sigma)
  y = beta_0 + beta_1 * x + epsilon
  data.frame(predictor = x, response = y)
}

sim_data = sim_slr(x, beta_0, beta_1, sigma)

#calculate RMSE and R^2

