
#simulate from known data
#model the data
birthday = 19810803
set.seed(birthday)
beta_0 = 0
beta_1 = 6
beta_2 = -3.5
beta_3 = 1.7
beta_4 = -1.1
beta_5 = 0.7

sample_size = 600
sigma_1 = 1
sigma_2 = 2
sigma_3 = 4

library(readr)
study_2 = read_csv("study_2.csv")

num_sims = 500
sample_size_split = 300
sample_size_total = 600

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

RMSE= function(model, test_data) {
  
  RMSE_train   = sqrt( mean(  resid(model) ^ 2  ))
  RMSE_test    = sqrt(mean( (test_data$y - predict(model, newdata = test_data))^2 ))
  
  values = c(RMSE_train = RMSE_train, RMSE_test = RMSE_test)
} 


simulate2 = function(num_sims, sigma){
  rmse1_train = rep(0, num_sims)
  rmse1_test  = rep(0, num_sims)
  rmse2_train = rep(0, num_sims)
  rmse2_test  = rep(0, num_sims)
  rmse3_train = rep(0, num_sims)
  rmse3_test  = rep(0, num_sims)
  rmse4_train = rep(0, num_sims)
  rmse4_test  = rep(0, num_sims)
  rmse5_train = rep(0, num_sims)
  rmse5_test  = rep(0, num_sims)
  rmse6_train = rep(0, num_sims)
  rmse6_test  = rep(0, num_sims)
  rmse7_train = rep(0, num_sims)
  rmse7_test  = rep(0, num_sims)
  rmse8_train = rep(0, num_sims)
  rmse8_test  = rep(0, num_sims)
  rmse9_train = rep(0, num_sims)
  rmse9_test  = rep(0, num_sims)
  winner      = rep(0, num_sims)
  
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
  
    # should do some loop here later to clean up code instead of doing over and over.
    models = c(model_1, model_2, model_3, model_4, model_5, model_6, model_7, model_8, model_9)
    
    values = RMSE(model_1, test)
    rmse1_train[i] = values["RMSE_train"]
    rmse1_test[i]  = values["RMSE_test"]
    
    values = RMSE(model_2, test)
    rmse2_train[i] = values["RMSE_train"]
    rmse2_test[i]  = values["RMSE_test"]
    
    values = RMSE(model_3, test)
    rmse3_train[i] = values["RMSE_train"]
    rmse3_test[i]  = values["RMSE_test"]
    
    values = RMSE(model_4, test)
    rmse4_train[i] = values["RMSE_train"]
    rmse4_test[i]  = values["RMSE_test"]
    
    values = RMSE(model_5, test)
    rmse5_train[i] = values["RMSE_train"]
    rmse5_test[i]  = values["RMSE_test"]
    
    values = RMSE(model_6, test)
    rmse6_train[i] = values["RMSE_train"]
    rmse6_test[i]  = values["RMSE_test"]
    
    values = RMSE(model_7, test)
    rmse7_train[i] = values["RMSE_train"]
    rmse7_test[i]  = values["RMSE_test"]
    
    values = RMSE(model_8, test)
    rmse8_train[i] = values["RMSE_train"]
    rmse8_test[i]  = values["RMSE_test"]
    
    values = RMSE(model_9, test)
    rmse9_train[i] = values["RMSE_train"]
    rmse9_test[i]  = values["RMSE_test"]
    
 
    
    #find the best one.
    temp = c(rmse1_test[i],rmse2_test[i], rmse3_test[i], rmse4_test[i], rmse5_test[i],
                         rmse6_test[i], rmse7_test[i], rmse8_test[i], rmse9_test[i])
    winner[i] = which.min(temp)
    
  }
  data.frame(rmse1_train = rmse1_train, rmse1_test = rmse1_test,
             rmse2_train = rmse2_train, rmse2_test = rmse2_test,
             rmse3_train = rmse3_train, rmse3_test = rmse3_test,
             rmse4_train = rmse4_train, rmse4_test = rmse4_test,
             rmse5_train = rmse5_train, rmse5_test = rmse5_test,
             rmse6_train = rmse6_train, rmse6_test = rmse6_test,
             rmse7_train = rmse7_train, rmse7_test = rmse7_test,
             rmse8_train = rmse8_train, rmse8_test = rmse8_test,
             rmse9_train = rmse9_train, rmse9_test = rmse9_test,
             winner      = winner
             
             )

}

system.time(simulate2(500, 1))



nrow(myresult[(myresult$winner == 1), ]) #number of times this model was selected, very low
nrow(myresult[(myresult$winner == 2), ])
nrow(myresult[(myresult$winner == 3), ])
nrow(myresult[(myresult$winner == 4), ]) # this one appears to be the correct one with a value over 200
nrow(myresult[(myresult$winner == 5), ])
nrow(myresult[(myresult$winner == 6), ])
nrow(myresult[(myresult$winner == 7), ])
nrow(myresult[(myresult$winner == 8), ])
nrow(myresult[(myresult$winner == 9), ])

