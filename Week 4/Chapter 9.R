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
p_value_beta_1 = summary(mpg_model)[ , 2 ]


(beta_0 = summary(mpg_model)$coefficients[,4])


coef(mpg_model)


#Bo = the mean miles per gallon for a car that weights 0 lbs and was built in 1900

summary(mpg_model)

(n = nrow(autompg))
(p = length(coef(mpg_model)))
(X = cbind(rep(1, n), autompg$wt, autompg$year))
(y = autompg$mpg)

(beta_hat = solve(t(X) %*% X)  %*% t(X) %*% y)

(y_hat = X %*% beta_hat)

(e = y - y_hat)

(s_e = sqrt( sum(((y - y_hat)^2)/(n-p)))) 

(s_e =  sqrt((t(e) %*% e)/(n-p)))

(s_e = sqrt( sum((e^2)/(n-p))))

(summary(mpg_model)$sigma)





#Simulations section of video.
set.seed(133)
n = 100
beta_0 = 5
beta_1 = -2
beta_2 = 6
sigma  = 4
p = 3

x0 = rep(1,n)


x1 = sample(seq(1,10, length = n))
x2 = sample(seq(1,10, length = n))
X =  cbind(x0, x1, x2)
C = solve(t(X) %*% X)


C[2+1, 2+1]
#variance of B2
sigma^2 * C[2+1, 2+1]

#standard deviation
sqrt(sigma^2 * C[2+1, 2+1])

y = rep(0, n)
num_sims = 10000
beta_hat_2 = rep(0, num_sims)

for(i in 1: num_sims){
  eps = rnorm(n, mean = 0, sd = sigma)
  y   = (beta_0*x0) + (beta_1 * x1) + (beta_2 * x2) + eps
  fit = lm( y ~ x1 + x2)
  #beta_hat 2 is the the 3rd coef B0, B1, B2
  beta_hat_2[i] = coef(fit)[3]
}

#simlated mean = 6, actual value is 6, so very close
(mean(beta_hat_2))

#simulated variance, 
var(beta_hat_2)
#true variance of beta_2 = sigma^2 * C22
sigma^2 * C[2+1, 2+1]

hist(beta_hat_2, prob = TRUE, breaks = 20, 
     xlab = expression(hat(beta)[2]), main = "", border = "dodgerblue")
curve(dnorm(x, mean = beta_2, sd = sqrt(sigma ^ 2 * C[2 + 1, 2 + 1])), 
      col = "darkorange", add = TRUE, lwd = 3)

#function to get a single estimate for beta-hat-2
#then used to replicate
sim_beta_hat_2 = function(){
  eps = rnorm(n, mean = 0, sd = sigma)
  y   = (beta_0*x0) + (beta_1 * x1) + (beta_2 * x2) + eps
  fit = lm( y ~ x1 + x2)
  #beta_hat 2 is the the 3rd coef B0, B1, B2
  beta_hat_2[i] = coef(fit)[3]
}

(sim_beta_hat_2())
beta_hat_2_alt = replicate( n = num_sims, sim_beta_hat_2() )

system.time(for(i in 1: num_sims){
  eps = rnorm(n, mean = 0, sd = sigma)
  y   = (beta_0*x0) + (beta_1 * x1) + (beta_2 * x2) + eps
  fit = lm( y ~ x1 + x2)
  #beta_hat 2 is the the 3rd coef B0, B1, B2
  beta_hat_2[i] = coef(fit)[3]
})

system.time({beta_hat_2_alt = replicate( n = num_sims, sim_beta_hat_2() )})




##Intervals and Test for MLR

