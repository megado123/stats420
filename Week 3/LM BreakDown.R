
stop_dist_model = lm(dist ~ speed, data = cars)

names(summary(stop_dist_model))

summary(stop_dist_model)$sigma

summary(stop_dist_model)

stop_dist_model_test_info = summary(stop_dist_model)$coefficients

p_value = stop_dist_model_test_info[2,4]
#1st row of results apply to beta_0

#2nd row of results apply to beta_1
beta_1_hat        = stop_dist_model_test_info[2, 1]
beta_1_hat

#Standard Error SE[beta_1_hat]
beta_1_hat_Std_Err = stop_dist_model_test_info[2, 2]

beta_1_hat_t       = stop_dist_model_test_info[2, 3]

beta_1_hat_Pr      = stop_dist_model_test_info[2, 4]
