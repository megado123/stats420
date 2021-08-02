
#simulate from known data
#model the data


#set seed
birthday = 19810803
set.seed(birthday)

#set known model parameters
beta_0 = 0
beta_1 = 6
beta_2 = -3.5
beta_3 = 1.7
beta_4 = -1.1
beta_5 = 0.7
#set values of sigma for each simulation
sigma_1 = 1
sigma_2 = 2
sigma_3 = 4

num_sims = 500
sample_size_split = 300
sample_size_total = 600

library(readr)
study_2 = read_csv("study_2.csv")


x0 = rep(1, sample_size_total)
x1 = study_2$x1
x2 = study_2$x2
x3 = study_2$x3
x4 = study_2$x4
x5 = study_2$x5
x6 = study_2$x6
x7 = study_2$x7
x8 = study_2$x8
x9 = study_2$x9
y = rep(0, sample_size_total)


sim_data = data.frame(x1, x2, x3, x4, x5,x6, x7, x8, x9,  y)

get_predict = function(model, test_data){
  test_data$y - predict(model, newdata = test_data)
}

get_RMSE= function(data) {
  RMSE   = sqrt( mean(data ^ 2  ))
}

simulate2 = function(num_sims, sigma){
  RMSE_train   = matrix(0, nrow = num_sims, ncol = 9)
  RMSE_test    =  matrix(0, nrow = num_sims, ncol = 9)
  winner_test  = rep(0, num_sims)
  winner_train = rep(0, num_sims)
  
  for(i in 1:num_sims) {
      
    eps = rnorm(sample_size_total, mean = 0 , sd = sigma)
    
    sim_data$y = beta_0 * x0 + beta_1 * x1 + beta_2 * x2 + beta_3 * x3 + beta_4 * x4 + beta_5 * x5 +   eps
    
    train_index = sample(1:nrow(sim_data), sample_size_split)
    train = sim_data[train_index ,]
    test  = sim_data[-train_index ,]
    
    model_1 = lm( y ~ x1, data = train)
    model_2 = lm( y ~ x1 + x2, data = train)
    model_3 = lm( y ~ x1 + x2 + x3, data = train)
    model_4 = lm( y ~ x1 + x2 + x3 + x4, data = train)
    model_5 = lm( y ~ x1 + x2 + x3 + x4 + x5, data = train)
    model_6 = lm( y ~ x1 + x2 + x3 + x4 + x5 + x6, data = train)
    model_7 = lm( y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7, data = train)
    model_8 = lm( y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, data = train)
    model_9 = lm( y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, data = train)
    
    RMSE_train[i ,] = c(get_RMSE(resid(model_1)), get_RMSE(resid(model_2)), get_RMSE(resid(model_3)), get_RMSE(resid(model_4)),
                      get_RMSE(resid(model_5)), get_RMSE(resid(model_6)), get_RMSE(resid(model_7)), get_RMSE(resid(model_8)),
                      get_RMSE(resid(model_9))
                      )

    RMSE_test[i, ] = c(get_RMSE(get_predict(model_1, test)), get_RMSE(get_predict(model_2, test)), 
                       get_RMSE(get_predict(model_3, test)), get_RMSE(get_predict(model_4, test)), 
                       get_RMSE(get_predict(model_5, test)), get_RMSE(get_predict(model_6, test)), 
                       get_RMSE(get_predict(model_7, test)), get_RMSE(get_predict(model_8, test)), 
                       get_RMSE(get_predict(model_9, test))
                       )
    winner_test[i] = which.min(RMSE_test[i, ])
    winner_train[i] = which.min(RMSE_train[i, ])
    
  }
  result = data.frame(RMSE_train, RMSE_test, winner_test, winner_train) 
  colnames(result) <- c( "TrainRMSE_M1", "TrainRMSE_M2", "TrainRMSE_M3", "TrainRMSE_M4", "TrainRMSE_M5", "TrainRMSE_M6", 
                         "TrainRMSE_M7", "TrainRMSE_M8", "TrainRMSE_M9", 
                        "TestRMSE_M1", "TestRMSE_M2", "TestRMSE_M3", "TestRMSE_M4", "TestRMSE_M5", "TestRMSE_M6", "TestRMSE_M7", 
                        "TestRMSE_M8", "TestRMSE_M9", "TestWinner", "TrainWinner")
  result
}

#run simulations 500 times for values of sigma: 1, 2, 4
sim1_sigma1 = simulate2(500, 1)
sim2_sigma2 = simulate2(500, 2)
sim3_sigma3 = simulate2(500, 4)


get_means = function(data){
  end = ncol(data) -2
  model_means = rep(0, end)
  for(i in 1:end) {
    model_means[i] = mean(sim1_sigma1[, i])
  }
  model_means
}

(sim1_means = get_means(sim1_sigma1))
(sim2_means = get_means(sim2_sigma2))
(sim3_means = get_means(sim3_sigma3))


head(sim1_sigma1)

#This is very important to see this.
sum(sim1_sigma1$TestWinner == 4) #22
sum(sim1_sigma1$TestWinner == 5) #253
sum(sim1_sigma1$TestWinner == 6) #96

sum(sim3_sigma3$TestWinner == 4) #126 <- choosing bad model
sum(sim3_sigma3$TestWinner == 5) #93
sum(sim3_sigma3$TestWinner == 6) #49



#Box plots
boxplotdata = sim1_sigma1
boxplotdata$TestWinner = NULL
boxplotdata$TrainWinner = NULL
# Box plots for all crime rates
boxplot(boxplotdata[,1:9], main="Box Plot for Test RMSE for Model Data")
# Box plots for all crime rates
boxplot(boxplotdata[,10:18], main="Box Plot for Test RMSE for Model Data")

#Histogram
hist(sim1_sigma1$TestWinner, 
     main = "Simulation Model Selected based on Test Data",
     xlab = "Model Selected",
     col = "dodgerblue",
     border = "darkorange", axis(side=1, at=seq(0,10, 1)))

hist(sim1_sigma1$TrainWinner,
     main = "Simulation Model Selected based on Test Data",
     xlab = "Model Selected",
     col = "dodgerblue",
     border = "darkorange", xlim=c(0, 10))




#plot for sigma = 1 RMSE Mean 
plot(log(sim1_means[1:9]), pch = 20, col = "dodgerblue", cex = 1,
     xlab = "Mean RMSE Response for a model with x predictors",
     ylab = "RMSE",
     main = "Simulated Regression Data",)
points(log(sim1_means[10:18]), pch = 20, col = "darkorange", cex = 1)
lines(sim1_means[10:18], col = "darkorange")
lines(sim1_means[1:9], col = "dodgerblue")

#plot for sigma = 2 RMSE Mean 
plot(sim2_means[1:9], pch = 20, col = "dodgerblue", cex = 1,
     xlab = "Mean RMSE Response for a model with x predictors",
     ylab = "RMSE",
     main = "Simulated Regression Data",)
points(sim2_means[10:18], pch = 20, col = "darkorange", cex = 1)
lines(sim2_means[10:18], col = "darkorange")
lines(sim2_means[1:9], col = "dodgerblue")


#plot for sigma = 4 RMSE Mean 
plot(sim3_means[1:9], pch = 20, col = "dodgerblue", cex = 1,
     xlab = "Mean RMSE Response for a model with x predictors",
     ylab = "RMSE",
     main = "Simulated Regression Data",)
points(sim3_means[10:18], pch = 20, col = "darkorange", cex = 1)
lines(sim3_means[10:18], col = "darkorange")
lines(sim3_means[1:9], col = "dodgerblue")

