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

#run simulations 500 times for values of sigma: 1, 2, 4 = 1500 times
sim1_sigma1 = simulate2(500, 1)
sim2_sigma2 = simulate2(500, 2)
sim3_sigma3 = simulate2(500, 4)


#get winners
num_models         = 9
WinnerTest         = rep(0, 9)
WinnerTrain        = rep(0, 9)
AverageTestWinner = rep(0, 9)

summarize_winners  = function(data){
  for(i in 1: num_models){
    WinnerTest[i]         = nrow(data[(data$TestWinner == i), ])
    WinnerTrain[i]        = nrow(data[(data$TrainWinner == i), ])
    AverageTestWinner[i] = mean(WinnerTest[i])
  }
  
  predictorVariables = rep(1:9)
  result = data.frame(predictorVariables, WinnerTest, WinnerTrain, AverageTestWinner)
  colnames(result) = c("Model Selected", "Num of Sims Model Selected base on Lowest Test RMSE", 
                       "Num of Sims Model Selected based on Lowest Train RMSE",
                       "Mean of Model Selected based on Lowest Test RMSE")
  result
}


get_means = function(data){
  end = ncol(data) -2
  model_means = rep(0, end)
  for(i in 1:end) {
    model_means[i] = mean(data[, i])
  }
  model_means
}

sim1_means = get_means(sim1_sigma1)
sim2_means = get_means(sim2_sigma2)
sim3_means = get_means(sim3_sigma3)




knitr::kable(summarize_winners(sim1_sigma1), caption = "Model Winner Based on Lowest RMSE" )

knitr::kable(summarize_winners(sim1_sigma1), caption = "Model Winner Based on Lowest RMSE Value for $\\sigma$ = 2" )

knitr::kable(summarize_winners(sim3_sigma3), caption = "Model Winner Based on Lowest RMSE Value for $\\sigma$ = 3")

plot.new

hist(sim1_sigma1$TrainWinner,
     main   = "Simulation Model Selected based on Train Data",
     xlab   = "Number of predictors in model",
     col    = "dodgerblue",
     border = "darkorange", axis(side=1, at=seq(0,10, 1)))


