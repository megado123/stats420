#preparing data

new_data = data.frame(
  y   = fb$`Lifetime Post Consumers`,
  x1  = fb$`Page total likes`,
  x2  = 1 * as.numeric(fb$Type == "Link"),
  x3  = 1 * as.numeric(fb$Type == "Photo"),
  x4  = 1 * as.numeric(fb$Type == "Status"),
  x5  = 1 * as.numeric(fb$Category == "1"),
  x6  = 1 * as.numeric(fb$Category == "2"),
  x7  = fb$`Post Month`,
  x8 = fb$`Post Weekday`,
  x9 = fb$`Post Hour`,
  x10 = fb$Paid,
  x11 = fb$`Lifetime Post Total Reach`,
  x12 = fb$`Lifetime Post Total Impressions`,
  x13 = fb$`Lifetime Engaged Users`,
  x14 = fb$`Lifetime Post Consumptions`,
  x15 = fb$`Lifetime Post Impressions by people who have liked your Page`,
  x16 = fb$`Lifetime Post reach by people who like your Page`,
  x17 = fb$`Lifetime People who have liked your Page and engaged with your post`,
  x18 = fb$comment,
  x19 = fb$like,
  x20 = fb$share
)

View(new_data)
which(is.na(new_data$x10) == TRUE)
which(is.na(new_data$x19) == TRUE)

head(new_data$x19)

#rows 112, 121, 125, 165
which(is.na(new_data$x20) == TRUE)

new_data[112, ]
new_data[121, ]
new_data[125, ]
new_data[165, ]

cleaned_data = new_data[complete.cases(new_data), ]
head(cleaned_data)


#Looking for collinearaity - issues
model_full = lm(log(y) ~ ., data = cleaned_data)
summary(model_full)
length(resid((model_full))) #there are only 495 rows in the data
vif(model_full)


model_simple = lm(log(y) ~ x13, data = cleaned_data)
length(resid((model_simple))) #there are only 495 rows in the data
vif(model_simple)

#still prefer the larger model, will continue
anova(model_simple, model_full)


#need to improve - adding another predictor - which one, partial corrlation coeffient
#can do a variable added plot
x1_model = lm(x1 ~ . - y - x1, data = cleaned_data)
cor(resid(x1_model), resid(model_simple))

x2_model = lm(x2 ~ . - y - x2, data = cleaned_data)
cor(resid(x2_model), resid(model_simple))

x3_model = lm(x3 ~ . - y - x3, data = cleaned_data)
cor(resid(x3_model), resid(model_simple))

x4_model = lm(x4 ~ . - y - x4, data = cleaned_data)
cor(resid(x4_model), resid(model_simple))

x5_model = lm(x5 ~ . - y - x5, data = cleaned_data)
cor(resid(x5_model), resid(model_simple))

x6_model = lm(x6 ~ . - y - x6, data = cleaned_data)
cor(resid(x6_model), resid(model_simple))

#looks pretty good (.2)
x7_model = lm(x7 ~ . - y - x7, data = cleaned_data)
cor(resid(x7_model), resid(model_simple))

x8_model = lm(x8 ~ . - y - x8, data = cleaned_data)
cor(resid(x8_model), resid(model_simple))

x9_model = lm(x9 ~ . - y - x9, data = cleaned_data)
cor(resid(x9_model), resid(model_simple))

x10_model = lm(x10 ~ . - y - x10, data = cleaned_data)
cor(resid(x10_model), resid(model_simple))

#####
x11_model = lm(x11 ~ . - y - x1, data = cleaned_data)
cor(resid(x11_model), resid(model_simple))

x12_model = lm(x12 ~ . - y - x12, data = cleaned_data)
cor(resid(x12_model), resid(model_simple))

#had the max partial correlation coeffiencent, and also had the lowest RSS :)
x13_model = lm(x13 ~ . - y - x13, data = cleaned_data)
cor(resid(x13_model), resid(model_simple))

summary(model_simple)

x14_model = lm(x14 ~ . - y - x14, data = cleaned_data)
cor(resid(x14_model), resid(model_simple))

x15_model = lm(x15 ~ . - y - x15, data = cleaned_data)
cor(resid(x15_model), resid(model_simple))

x16_model = lm(x16 ~ . - y - x16, data = cleaned_data)
cor(resid(x16_model), resid(model_simple))

#looks pretty good (.2399)
x17_model = lm(x17 ~ . - y - x17, data = cleaned_data)
cor(resid(x17_model), resid(model_simple))

x18_model = lm(x18 ~ . - y - x18, data = cleaned_data)
cor(resid(x18_model), resid(model_simple))

x19_model = lm(x19 ~ . - y - x19, data = cleaned_data)
cor(resid(x19_model), resid(model_simple))

x20_model = lm(x20 ~ . - y - x20, data = cleaned_data)
cor(resid(x20_model), resid(model_simple))


#Should probably be x17 next is my guess

getPartialCorrlationCo = function(current_model, data, parameter_to_add)
{
  strmodel[i]      = paste(parameter_to_add, " ~ . - y - " , parameter_to_add)
  model         = lm(strmodel[i], data = wine)
  
}












length(resid(x1_model))

length(resid(model_simple))








model_simple = lm(log(y) ~ x13, data = new_data)
length(resid(model_simple))
vif(model_simple)

anova(model_simple, model_full)


summary(model)
#RSS = Residual Sum of Squares = Sum of Square Errors
sum(model$residuals^2)

variables = colnames(new_data)

getSingle = function(variables, alpha = 0.01, data_set)
{
  num_predictors  = length(variables)
  p_val           = rep(0, num_predictors)
  bp_p_value      = rep(0, num_predictors)
  shapiro_p_value = rep(0, num_predictors)
  strmodel        = rep(0, num_predictors)
  rss             = rep(0, num_predictors)
  
  for (i in 1:length(variables)) {
    s1            = "y"
    s2            = variables[i]
    strmodel[i]   = paste(s1, " ~ " , s2)
    print(strmodel[i])
    model         = lm(strmodel[i], data = data_set)
    rss[i]           = sum(resid(model)^2) 
  }
  data.frame(strmodel = strmodel, p_val = p_val, bp_p_value)
  
}

predictor_variables = variables[which(variables != "y")]
result = getSingle(predictor_variables, 0.01, new_data)
knitr::kable(result, format = "markdown", padding = 3)


###############################
#do it for the log#

getSingleForLog = function(variables, alpha = 0.01, data_set)
{
  num_predictors  = length(variables)
  p_val           = rep(0, num_predictors)
  bp_p_value      = rep(0, num_predictors)
  shapiro_p_value = rep(0, num_predictors)
  strmodel        = rep(0, num_predictors)
  rss             = rep(0, num_predictors)
  
  for (i in 1:length(variables)) {
    s1            = "log(y)"
    s2            = variables[i]
    strmodel[i]   = paste(s1, " ~ " , s2)
    print(strmodel[i])
    model         = lm(strmodel[i], data = data_set)
    rss[i]           = sum(resid(model)^2) 
  }
  data.frame(strmodel = strmodel, p_val = p_val, bp_p_value)
  
}

predictor_variables = variables[which(variables != "y")]
result = getSingle(predictor_variables, 0.01, new_data)
knitr::kable(result, format = "markdown", padding = 3)


X = matrix(1:20, nrow = 20, ncol = 2)
X
