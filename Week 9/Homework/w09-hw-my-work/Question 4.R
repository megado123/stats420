
#Question 4 homework

beta_0  = 1
beta_1  = 0
beta_2  = 1
beta_3  = 0
beta_4  = 2
beta_5  = 0
beta_6  = 1
beta_7  = 0
beta_8  = 2
beta_9  = 0
beta_10 = 1
sigma = 3

not_sig  = c("x_1", "x_3", "x_5", "x_7", "x_9")
signif = c("x_2", "x_4", "x_6", "x_8", "x_10")

set.seed(19810803)
n = 100
x_1  = runif(n, 0, 10)
x_2  = runif(n, 0, 10)
x_3  = runif(n, 0, 10)
x_4  = runif(n, 0, 10)
x_5  = runif(n, 0, 10)
x_6  = runif(n, 0, 10)
x_7  = runif(n, 0, 10)
x_8  = runif(n, 0, 10)
x_9  = runif(n, 0, 10)
x_10 = runif(n, 0, 10)





simulation = function(num_sim){
  
  false_neg_aic = rep(0, num_sim)
  false_pos_aic = rep(0, num_sim)
  false_neg_bic = rep(0, num_sim)
  false_pos_bic = rep(0, num_sim)
  
  for(i in 1:num_sim) {
    sim_data_1 = data.frame(x_1, x_2, x_3, x_4, x_5, x_6, x_7, x_8, x_9, x_10,
                          y = beta_0 + beta_2 * x_2 + beta_4 * x_4 + beta_6 * x_6 + beta_8 * x_8 + 
                            beta_10 * x_10 + rnorm(n, 0 , sigma)
    )
    #fit = lm(y ~ ., data = sim_data_1)
    fit = lm(y ~ x_1 + x_2 + x_3 + x_4 + x_5 + x_6 + x_7 + x_8 + x_9 + x_10, data = sim_data_1)
  
    #Backwards search which defaults to AIC
    model_back_aic = step(fit, direction = "backward", trace = 0)

    false_neg_aic[i] = sum(!(signif %in% names(coef(model_back_aic))))
    false_pos_aic[i] = sum(names(coef(model_back_aic)) %in% not_sig)
  
    #Backwards search for BIC
    model_back_bic = step(fit, direction = "backward", k = log(n), trace = 0)
    
    false_neg_bic[i] = sum(!(signif %in% names(coef(model_back_bic))))
    false_pos_bic[i] = sum(names(coef(model_back_bic)) %in% not_sig)
  }
 
  data.frame(false_neg_aic = false_neg_aic, false_pos_aic = false_pos_aic, false_neg_bic = false_neg_bic, false_pos_bic = false_pos_bic)
}

result = simulation(200)


mean(result$false_neg_aic)
mean(result$false_pos_aic)

mean(result$false_neg_bic)
mean(result$false_pos_bic)

result1 = cbind( "AIC", mean(result$false_pos_aic), mean(result$false_neg_aic))

result2 = cbind("BIC", mean(result$false_pos_bic), mean(result$false_neg_bic))

results = rbind(result1, result2)

results

colnames(results) = c("Method", "Average False Positives for Model", "Average False Negatives for Model")

kable(results, format = "markdown", padding = 3)

knitr::kable(results, caption = "Average Number of False Predictors")





sim_data_1 = data.frame(x_1, x_2, x_3, x_4, x_5, x_6, x_7, x_8, x_9, x_10,
                        y = beta_0 + beta_2 * x_2 + beta_4 * x_4 + beta_6 * x_6 + beta_8 * x_8 + 
                          beta_10 * x_10 + rnorm(n, 0 , sigma))

fit = lm(y ~ x_1 + x_2 + x_3 + x_4 + x_5 + x_6 + x_7 + x_8 + x_9 + x_10, data = sim_data_1)
coef(fit)
#Backwards search which defaults to AIC
model_back_aic = step(fit, direction = "backward")
coef(model_back_aic)
# which are false negatives?
!(signif %in% names(coef(model_back_aic)))


#false negatives
sum(!(signif %in% names(coef(model_back_aic))))

#false positives
sum((names(coef(model_back_aic)) %in% not_sig))

############################################
#Part B
############################################

beta_0  = 1
beta_1  = 0
beta_2  = 1
beta_3  = 0
beta_4  = 2
beta_5  = 0
beta_6  = 1
beta_7  = 0
beta_8  = 2
beta_9  = 0
beta_10 = 1
sigma = 3

not_sig  = c("x_1", "x_3", "x_5", "x_7", "x_9")
signif = c("x_2", "x_4", "x_6", "x_8", "x_10")

set.seed(19810803)
n = 100
x_1  = runif(n, 0, 10)
x_2  = runif(n, 0, 10)
x_3  = runif(n, 0, 10)
x_4  = runif(n, 0, 10)
x_5  = runif(n, 0, 10)
x_6  = runif(n, 0, 10)
x_7  = runif(n, 0, 10)
x_8  = x_6 + rnorm(n, 0, 0.1)
x_9  = x_6 + rnorm(n, 0, 0.1)
x_10 = x_4 + rnorm(n, 0, 0.1)

simulation = function(num_sim){
  
  false_neg_aic = rep(0, num_sim)
  false_pos_aic = rep(0, num_sim)
  false_neg_bic = rep(0, num_sim)
  false_pos_bic = rep(0, num_sim)
  
  for(i in 1:num_sim) {
    sim_data_2 = data.frame(x_1, x_2, x_3, x_4, x_5, x_6, x_7, x_8, x_9, x_10,
                            y = beta_0 + beta_2 * x_2 + beta_4 * x_4 + beta_6 * x_6 + beta_8 * x_8 + 
                              beta_10 * x_10 + rnorm(n, 0 , sigma)
    )
    fit = lm(y ~ x_1 + x_2 + x_3 + x_4 + x_5 + x_6 + x_7 + x_8 + x_9 + x_10, data = sim_data_2)
    
    #Backwards search which defaults to AIC
    model_back_aic = step(fit, direction = "backward", trace = 0)
    
    false_neg_aic[i] = sum(!(signif %in% names(coef(model_back_aic))))
    false_pos_aic[i] = sum(names(coef(model_back_aic)) %in% not_sig)
    
    
    
    #Backwards search for BIC
    model_back_bic = step(fit, direction = "backward", k = log(n), trace = 0)
    
    false_neg_bic[i] = sum(!(signif %in% names(coef(model_back_bic))))
    false_pos_bic[i] = sum(names(coef(model_back_bic)) %in% not_sig)
  }
  
  data.frame(false_neg_aic = false_neg_aic, false_pos_aic = false_pos_aic, false_neg_bic = false_neg_bic, false_pos_bic = false_pos_bic)
}

result = simulation(200)

mean(result$false_neg_aic)
mean(result$false_pos_aic)

mean(result$false_neg_bic)
mean(result$false_pos_bic)

sum(names(coef(model)) %in% not_sig)



#####################
#Exploring part B
####################

sim_data_2 = data.frame(x_1, x_2, x_3, x_4, x_5, x_6, x_7, x_8, x_9, x_10,
                        y = beta_0 + beta_2 * x_2 + beta_4 * x_4 + beta_6 * x_6 + beta_8 * x_8 + 
                          beta_10 * x_10 + rnorm(n, 0 , sigma)
)
fit = lm(y ~ x_1 + x_2 + x_3 + x_4 + x_5 + x_6 + x_7 + x_8 + x_9 + x_10, data = sim_data_2)

#Backwards search which defaults to AIC
model_back_aic = step(fit, direction = "backward", trace = 0)

coef(model_back_aic)
false_neg_aic[i] = sum(!(signif %in% names(coef(model_back_aic))))
false_pos_aic[i] = sum(names(coef(model_back_aic)) %in% not_sig)



#Backwards search for BIC
model_back_bic = step(fit, direction = "backward", k = log(n), trace = 0)
names(coef(model_back_aic))[1]

num_sim = 10
model = step(fit, direction = "backward", trace = 0)
aic_x_1 =  rep(0, num_sim)
aic_x_2 =  rep(0, num_sim)
aic_x_3 =  rep(0, num_sim)
aic_x_4 =  rep(0, num_sim)
aic_x_5 =  rep(0, num_sim)
aic_x_6 =  rep(0, num_sim)
aic_x_7 =  rep(0, num_sim)
aic_x_8 =  rep(0, num_sim)
aic_x_9 =  rep(0, num_sim)
aic_x_10 =  rep(0, num_sim)

for(i in 1:length(coef(model))){
  if (names(coef(model)[i]) == "x_1")
    aic_x_1[i] = coef(model)[i]
  
  if (names(coef(model)[i]) == "x_2")
    aic_x_2[i] = coef(model)[i]
  
  if (names(coef(model)[i]) == "x_3")
    aic_x_3[i] = coef(model)[i]
  
  if (names(coef(model)[i]) == "x_4")
    aic_x_4[i] = coef(model)[i]
  
  if (names(coef(model)[i]) == "x_5")
    aic_x_5[i] = coef(model)[i]
  
  if (names(coef(model)[i]) == "x_6")
    aic_x_6[i] = coef(model)[i]
  
  if (names(coef(model)[i]) == "x_7")
    aic_x_7[i] = coef(model)[i]
  
  if (names(coef(model)[i]) == "x_8")
    aic_x_8[i] = coef(model)[i]
  
  if (names(coef(model)[i]) == "x_9")
    aic_x_9[i] = coef(model)[i]
  
  if (names(coef(model)[i]) == "x_10")
    aic_x_10[i] = coef(model)[i]
}


##############################
beta_0  = 1
beta_1  = 0
beta_2  = 1
beta_3  = 0
beta_4  = 2
beta_5  = 0
beta_6  = 1
beta_7  = 0
beta_8  = 2
beta_9  = 0
beta_10 = 1
sigma = 3

not_sig  = c("x_1", "x_3", "x_5", "x_7", "x_9")
signif = c("x_2", "x_4", "x_6", "x_8", "x_10")

set.seed(19810803)
n = 100
x_1  = runif(n, 0, 10)
x_2  = runif(n, 0, 10)
x_3  = runif(n, 0, 10)
x_4  = runif(n, 0, 10)
x_5  = runif(n, 0, 10)
x_6  = runif(n, 0, 10)
x_7  = runif(n, 0, 10)
x_8  = x_6 + rnorm(n, 0, 0.1)
x_9  = x_6 + rnorm(n, 0, 0.1)
x_10 = x_4 + rnorm(n, 0, 0.1)

simulation = function(num_sim){
  
  false_neg_aic = rep(0, num_sim)
  false_pos_aic = rep(0, num_sim)
  false_neg_bic = rep(0, num_sim)
  false_pos_bic = rep(0, num_sim)
  
  aic_x_1 =  rep(0, num_sim)
  aic_x_2 =  rep(0, num_sim)
  aic_x_3 =  rep(0, num_sim)
  aic_x_4 =  rep(0, num_sim)
  aic_x_5 =  rep(0, num_sim)
  aic_x_6 =  rep(0, num_sim)
  aic_x_7 =  rep(0, num_sim)
  aic_x_8 =  rep(0, num_sim)
  aic_x_9 =  rep(0, num_sim)
  aic_x_10 =  rep(0, num_sim)
  
  for(i in 1:num_sim) {
    sim_data_2 = data.frame(x_1, x_2, x_3, x_4, x_5, x_6, x_7, x_8, x_9, x_10,
                            y = beta_0 + beta_2 * x_2 + beta_4 * x_4 + beta_6 * x_6 + beta_8 * x_8 + 
                              beta_10 * x_10 + rnorm(n, 0 , sigma)
    )
    fit = lm(y ~ x_1 + x_2 + x_3 + x_4 + x_5 + x_6 + x_7 + x_8 + x_9 + x_10, data = sim_data_2)
    
    #Backwards search which defaults to AIC
    model_back_aic = step(fit, direction = "backward", trace = 0)
    
    
    print(coef(model_back_aic))
    
    
    
    false_neg_aic[i] = sum(!(signif %in% names(coef(model_back_aic))))
    false_pos_aic[i] = sum(names(coef(model_back_aic)) %in% not_sig)
    
    
    
    #Backwards search for BIC
    model_back_bic = step(fit, direction = "backward", k = log(n), trace = 0)
    
    false_neg_bic[i] = sum(!(signif %in% names(coef(model_back_bic))))
    false_pos_bic[i] = sum(names(coef(model_back_bic)) %in% not_sig)
  }
  
  data.frame(false_neg_aic = false_neg_aic, false_pos_aic = false_pos_aic, false_neg_bic = false_neg_bic, false_pos_bic = false_pos_bic,aic_x_1, aic_x_2, aic_x_3, aic_x_4, aic_x_5, aic_x_6, aic_x_7, aic_x_8, aic_x_9, aic_x_10 )
}

result = simulation(200)


mean(result$false_neg_aic)
mean(result$false_pos_aic)

mean(result$false_neg_bic)
mean(result$false_pos_bic)

get_loocv_rmse = function(model) {
  sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
}

