library(MASS)
library(readr)
library(lmtest)
library(car)
library(leaps)
library(knitr)
library(faraway)
library(tibble)


fb = read.table("dataset_Facebook.csv", sep = ";", header = T)

str(fb)

fb$Type           = as.factor(fb$Type)
fb$Category       = as.factor(fb$Category)
fb$Post.Month     = as.factor(fb$Post.Month)
fb$Post.Hour      = as.factor(fb$Post.Hour)
fb$Post.Weekday   = as.factor(fb$Post.Weekday)
fb$Paid           = as.factor(fb$Paid)

levels(fb$Post.Weekday)

levels(fb$Post.Hour)


full_model = lm(Lifetime.Post.Consumers ~ ., data = fb)
fb$Total.Interactions = NULL
full_model = lm(Lifetime.Post.Consumers ~ ., data = fb)

nrow(fb)
#Cleansing Data
fb = fb[complete.cases(fb), ]
nrow(fb)


length(coef(model))

#this errors out due to perfect collinearity so we need to id and remove these variables
faraway::vif(full_model)

full_model = lm(Lifetime.Post.Consumers ~ ., data = fb)

p = length(coef(full_model))
p
#The variables to consider
variables = read_csv("variables.csv", col_names = FALSE)


new_data = data.frame(
  y   = fb$Lifetime.Post.Consumers,
  x1  = fb$Page.total.likes,
  x2  = fb$Lifetime.Post.Total.Reach,
  x3  = fb$Lifetime.Post.Total.Impressions,
  x4  = fb$Lifetime.Engaged.Users,
  x5  = fb$Lifetime.Post.Consumptions,
  x6  = fb$Lifetime.Post.Impressions.by.people.who.have.liked.your.Page,
  x7  = fb$Lifetime.Post.reach.by.people.who.like.your.Page, 
  x8  = fb$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post,
  x9  = fb$comment,
  x10 = fb$like,
  x11 = fb$share,
  x12 = 1 * as.numeric(fb$Type == "Photo"),
  x13 = 1 * as.numeric(fb$Type == "Status"),
  x14 = 1 * as.numeric(fb$Type == "Video"),
  x15 = 1 * as.numeric(fb$Category == "2"),
  x16 = 1 * as.numeric(fb$Category == "3"),
  x17 = 1 * as.numeric(fb$Post.Month == "2"),
  x18 = 1 * as.numeric(fb$Post.Month == "3"),
  x19 = 1 * as.numeric(fb$Post.Month == "4"),
  x20 = 1 * as.numeric(fb$Post.Month == "5"),
  x21 = 1 * as.numeric(fb$Post.Month == "6"),
  x22 = 1 * as.numeric(fb$Post.Month == "7"),
  x23 = 1 * as.numeric(fb$Post.Month == "8"),
  x24 = 1 * as.numeric(fb$Post.Month == "9"),
  x25 = 1 * as.numeric(fb$Post.Month == "10"),
  x26 = 1 * as.numeric(fb$Post.Month == "11"),
  x27 = 1 * as.numeric(fb$Post.Month == "12"),
  x28 = 1 * as.numeric(fb$Post.Weekday == "2"),
  x29 = 1 * as.numeric(fb$Post.Weekday == "3"),
  x30 = 1 * as.numeric(fb$Post.Weekday == "4"),
  x31 = 1 * as.numeric(fb$Post.Weekday == "5"),
  x32 = 1 * as.numeric(fb$Post.Weekday == "6"),
  x33 = 1 * as.numeric(fb$Post.Weekday == "7"),
  x34 = 1 * as.numeric(fb$Post.Hour == "2"),
  x35 = 1 * as.numeric(fb$Post.Hour == "3"),
  x36 = 1 * as.numeric(fb$Post.Hour == "4"),
  x37 = 1 * as.numeric(fb$Post.Hour == "5"),
  x38 = 1 * as.numeric(fb$Post.Hour == "6"),
  x39 = 1 * as.numeric(fb$Post.Hour == "7"),
  x40 = 1 * as.numeric(fb$Post.Hour == "8"),
  x41 = 1 * as.numeric(fb$Post.Hour == "9"),
  x42 = 1 * as.numeric(fb$Post.Hour == "10"),
  x43 = 1 * as.numeric(fb$Post.Hour == "11"),
  x44 = 1 * as.numeric(fb$Post.Hour == "12"),
  x45 = 1 * as.numeric(fb$Post.Hour == "13"),
  x46 = 1 * as.numeric(fb$Post.Hour == "14"),
  x47 = 1 * as.numeric(fb$Post.Hour == "15"),
  x48 = 1 * as.numeric(fb$Post.Hour == "16"),
  x49 = 1 * as.numeric(fb$Post.Hour == "17"),
  x50 = 1 * as.numeric(fb$Post.Hour == "18"),
  x51 = 1 * as.numeric(fb$Post.Hour == "19"),
  x52 = 1 * as.numeric(fb$Post.Hour == "20"),
  x53 = 1 * as.numeric(fb$Post.Hour == "22"),
  x54 = 1 * as.numeric(fb$Post.Hour == "23"),
  x55 = 1 * as.numeric(fb$Paid == 1)
)


#range(new_data$x10) -> add a really small value


#Look at additive Models so then can do annova tests
variables = colnames(new_data)
predictor_variables = variables[which(variables != "y")]

getAdditiveModels = function(num_predictor_variables, variables, data_set, alpha)
{
  p_val           = rep(0, num_predictor_variables ^ 2)
  bp_p_value      = rep(0, num_predictor_variables ^ 2)
  shapiro_p_value = rep(0, num_predictor_variables ^ 2)
  strmodel        = rep(0, num_predictor_variables ^ 2)
  rss             = rep(0, num_predictor_variables ^ 2)
  beta_parameter  = rep(0, num_predictor_variables ^ 2)
  pass_bp         = rep(0, num_predictor_variables ^ 2)
  pass_shapiro    = rep(0, num_predictor_variables ^ 2)
  adjustedR2      = rep(0, num_predictor_variables ^ 2)
  
  index = 1;
  
  for (j in 1:num_predictor_variables)
  {
    smallestrss = 0
    smallestrssIndex = 0 
    for (i in 1:length(variables))
    {
      
      if(j == 1) #at the beginning
      {
        startpointer          = index
        #could make s1 be log(y)
        s1                    = "y"
        s2                    = variables[i]
        strmodel[index]       = paste(s1, " ~ " , s2)
        
      }
      else
      {
        s2                    = variables[i]
        strmodel[index]       = paste(previousBestModel , " + " , s2)
        
      }
      #print(strmodel[index])
      model                    = lm(strmodel[index], data = data_set)
      rss[index]               = sum(resid(model)^2) 
      bp_p_value[index]        = bptest(model)$p.value
      #print(bptest(model)$p.value)
      shapiro_p_value[index]   = shapiro.test(resid(model))$p.value
      beta_parameter[index]    = j
      adjustedR2[index]        = summary(model)$adj.r.squared
      
      if (shapiro_p_value[i] > alpha) {
        pass_shapiro[index] = TRUE
      }
      
      if (bp_p_value[i] > alpha) {
        pass_bp[index] = TRUE
      }
      
      
      if (smallestrssIndex == 0 || rss[index] < smallestrss){
        
        smallestrssIndex = index
        smallestrss = rss[index]
        print(paste(smallestrssIndex, "!!!"))
      }
      index = index + 1
      
    }
    #for a given number of predictor variables create temp vector and find the min rss
    
    previousBestModel = strmodel[smallestrssIndex]
  }
  result = data.frame(beta_parameter, strmodel, rss, adjustedR2, bp_p_value, shapiro_p_value, pass_bp, pass_shapiro)
  result$strmodel =  as.character(result$strmodel)
  result
  
}


results = getAdditiveModels(55, predictor_variables, new_data, .05)

nrow(results)


head(results, 300)


#get that model
results[which.min(results[results$beta_parameter == 1, ]$rss), "strmodel"]




getModels = function(results, num_predictor_variables){
  bestModelForParamNumber = rep("", num_predictor_variables)
  for (i in 1:num_predictor_variables)
  {
    subset_data = results[results$beta_parameter == i, ]
    
    bestModelForParamNumber[i] = subset_data$strmodel[which.min(subset_data$rss)]
    print(subset_data$strmodel[which.min(subset_data$rss)])
  }
  bestModelForParamNumber
}

bestModels = getModels(results, 55)

bestModels[1]
bestModels[2]
bestModels[3]

anova(lm(bestModels[19], data = new_data), lm(bestModels[50], data = new_data))$"Pr(>F)"[2]



doAnova = function(dataset, models, alpha)
{
  p_val   = rep(0, length(models) - 1)
  keep_old = rep(FALSE, length(models) - 1)
  winning_model = rep(0, length(models) - 1)
  stop = FALSE
  print(length(models))
  for (i in 1:(length(models) -1))
  {
    if (stop == FALSE)
    {
    p_val[i] = anova(lm(bestModels[i], data = new_data), lm(bestModels[i + 1], data = new_data))$"Pr(>F)"[2]
    if (p_val[i] < alpha)
    {
      keep_old[i] = FALSE
      winning_model[i] = bestModels[i + 1]
      stop = FALSE
    }
    else
    {
      keep_old[i] = TRUE
      winning_model[i] = bestModels[i]
      stop = TRUE
    }
    }
    
 
  }
  
  data.frame(p_val, winning_model, keep_old)

}

bestModels[1]
results = doAnova(new_data, bestModels, 0.05 )
View(results)

results[results$keep_old == TRUE, ]

#best additive model based on RSS and Anova Test
#y = x4 + x10 + x11 + x8 + x5 + x27 + x9 + x26 + x2 + x14  x3 + x17

model_add = lm(y ~ x4 + x10 + x11 + x8 + x5 + x27 + x9 + x26 + x2 + x14 + x3 + x17, data = new_data)

car::vif(model_add)

#we have pretty low VIF, however 5 is considered high
#Now need to look at colllinearity

#AIC seems to agree
model_back_aic = step(model_add, direction = "backward", trace = 0)
summary(model_back_aic)
car::vif(model_back_aic)


#BIC check
n = length(resid(model_add))
model_back_bic = step(model_add, direction = "backward", k = log(n), trace = 0)
summary(model_back_bic)

#comparing VIF found in origianl model and BIC backward
car::vif(model_add)
car::vif(model_back_bic)

#since that did reduced the VIF of x4, we will proceed with removing x17

#AIC forward check
fb_mod_start = lm(y ~ 1, data = new_data)
fb_mod_forw_aic = step(fb_mod_start,
                              y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 + x31 + x32 + x33 + x34 + x35 + x36 + x37 + x38 + x39 + x40 + x41 + x42 + x43 + x44 + x45 + x46 + x47 + x48 + x49 + x50 + x51 + x52 + x53 + x54 + x55,
                              direction = "forward", trace = 0)
summary(fb_mod_forw_aic)

#BIC forward check

full_model = lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 + x31 + x32 + x33 + x34 + x35 + x36 + x37 + x38 + x39 + x40 + x41 + x42 + x43 + x44 + x45 + x46 + x47 + x48 + x49 + x50 + x51 + x52 + x53 + x54 + x55, data = new_data)
n = length(resid(full_model))
fb_mod_forw_bic = step(fb_mod_start,
                       y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 + x31 + x32 + x33 + x34 + x35 + x36 + x37 + x38 + x39 + x40 + x41 + x42 + x43 + x44 + x45 + x46 + x47 + x48 + x49 + x50 + x51 + x52 + x53 + x54 + x55,
                              direction = "forward", k = log(n), trace = 0)
summary(fb_mod_forw_bic)

car::vif(model_add)
car::vif(model_back_aic)
car::vif(model_back_bic)
car::vif(fb_mod_forw_aic)
car::vif(fb_mod_forw_bic)

#Given the p-value is at 0.03205, we could choose to use a lower p-value and this does increase the VIF 
#we will exclude this from the model
anova(model_back_bic, model_add)

chosen_model_additive = model_back_bic

current_chosen_model = model_back_bic

#Examine Collinearity
#full model here: model_back_bic
summary(model_back_bic)
#x4, x10, x11, x8, x5, x27, x9, x26, x2, x14, x3

model_small = lm(y ~ x4 + x10, data = new_data)
car::vif(model_small)

x11_model_small = lm(x11 ~ x4 + x10, data = new_data)

cor(resid(model_small), resid(x11_model_small))

#variable added plot
plot(resid(model_small) ~ resid(x11_model_small), col = "dodgerblue", pch = 20,
     xlab = "Residuals, Added Predictor", ylab = "Residuals, Original Model")
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
abline(lm(resid(model_small) ~ resid(x11_model_small)),
       col = "darkorange", lwd = 2)

###############################################################################
#since we do seem to see a relationship, we will continue with x11 in the model
##x4, x10, x11, x8, x5, x27, x9, x26, x2, x14, x3

model_small = lm(y ~ x4 + x10 + x11, data = new_data)
car::vif(model_small)

#here we see a jump in the VIF past 5 suggesting


x11_model_small = lm(x11 ~ x4 + x10, data = new_data)

cor(resid(model_small), resid(x11_model_small))

#variable added plot
plot(resid(model_small) ~ resid(x11_model_small), col = "dodgerblue", pch = 20,
     xlab = "Residuals, Added Predictor", ylab = "Residuals, Original Model")
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
abline(lm(resid(model_small) ~ resid(x11_model_small)),
       col = "darkorange", lwd = 2)

###############################################################################
#next will examine interaction terms
#x4, x10, x11, x8, x5, x27, x9, x26, x2, x14, x3
################################################################################
fb_limited = data.frame(
  y   = new_data$y,
  x4  = new_data$x4,
  x10 = new_data$x10,
  x11 = new_data$x11,
  x8 = new_data$x8,
  x5 = new_data$x5,
  x27 = new_data$x27,
  x9 = new_data$x9,
  x26 = new_data$x26,
  x2 = new_data$x2,
  x14 = new_data$x14,
  x3 = new_data$x3)

pairs(fb_limited)  

model_interact = lm(y ~ (x4 + x10 + x11 + x8 + x5 + x27 + x9 + x26 + x2 + x14 + x3) ^ 2, data = fb_limited)
summary(model_interact)

anova(chosen_model_additive, model_interact)
#above shows some interaction is good. - the overal p value is showing that some interaction is significant


#look at AIC and BIC and adjusted R^2 to help determine which interactions we should explore
#AIC seems to agree
model_back_aic = step(model_interact, direction = "backward", trace = 0)
summary(model_back_aic)
car::vif(model_back_aic)


#BIC check
n = length(resid(model_interact))
model_back_bic = step(model_interact, direction = "backward", k = log(n), trace = 0)
summary(model_back_bic)

#comparing VIF found in origianl model and BIC backward
car::vif(current_chosen_model)
car::vif(model_back_aic)
car::vif(model_back_bic)

#since that did reduced the VIF of x4, we will proceed with removing x17

#Given the high VIF that was introducted, we can see that x27 interactions do not appear to introduce
#high VIF 
model_interact = lm(y ~ (x4 + x10 + x11 + x8 + x5 + x27 + x9 + x26 + x2 + x14 + x3)* x27, data = fb_limited)

summary(model_interact)

#some of the interactiosn are significant
anova(chosen_model_additive, model_interact)

n = length(resid(model_interact))
model_back_bic = step(model_interact, direction = "backward", k = log(n), trace = 0)
summary(model_back_bic)

#did not significantly raise
car::vif(model_back_bic)
car::vif(chosen_model_additive)

anova(chosen_model_additive, model_back_bic)

#update the model to include the additional interactions
current_chosen_model_int = model_back_bic

summary(current_chosen_model_int)
#interactions end - looks like model with interactions is the "best current" model which will need to be validated

#############


##########################################################################
#Transformations on x4
###########################################################################
plot_fitted_resid = function(model, pointcol = "dodgerblue", linecol = "darkorange") {
  plot(fitted(model), resid(model), 
       col = pointcol, pch = 20, cex = 1.5,
       xlab = "Fitted", ylab = "Residuals")
  abline(h = 0, col = linecol, lwd = 2)
}

plot_qq = function(model, pointcol = "dodgerblue", linecol = "darkorange") {
  qqnorm(resid(model), col = pointcol, pch = 20, cex = 1.5)
  qqline(resid(model), col = linecol, lwd = 2)
}

plot_x4_curve = function(model, log_response = TRUE, x_lab = "x4"){
  if (log_response == FALSE){
    plot(y ~ x4, data = fb_limited, xlab = "x4", 
         ylab = "y", col = "dodgerblue", 
         pch = 20, cex =2)    
  }
  else
  {
    plot(log(y) ~ x4, data = fb_limited, xlab = "x4", 
         ylab = "log(y)", col = "dodgerblue", 
         pch = 20, cex =2) 
  }

  xplot = seq(min(fb_limited$x4), max(fb_limited$x4), by = ((max(fb_limited$x4) - min(fb_limited$x4))/100) )
  lines(xplot, predict(model, newdata = data.frame(x4 = xplot)),
        col = "darkorange", lwd = 2, lty = 1)
}

##function
diagnostics = function(model, pcol= 1, lcol = 0, alpha = 0.05, plotit = TRUE, testit = TRUE){
  if (plotit == TRUE){
    par(mfrow = c(1, 2))
    plot(fitted(model), resid(model), col = pcol, pch = 20,
         xlab = "Fitted", ylab = "Residuals", main = "Fitted vs Residuals")
    abline(h = 0, col = lcol, lwd = 2)
    qqnorm(resid(model), main = "Normal Q-Q Plot", col = pcol)
    qqline(resid(model), col = lcol, lwd = 2)
  }
  if (testit == TRUE){
    shapiro_p_value = shapiro.test(resid(model))$p.value
    bp_p_value      = bptest(model)$p.value 
    bp_decision      = ifelse(bp_p_value < alpha, "Reject Equal Variance - Bad", "Fail to Equal Variance-Good")
    shapiro_decision = ifelse(shapiro_p_value < alpha, "Reject Normality - Bad", "Fail to Reject Normality- Good")
    
    return_val = list(shapiro_p_value = shapiro_p_value, bp_p_value = bp_p_value, bp_decision = bp_decision, shapiro_decision = shapiro_decision)
    return_val
  }
}

##################################################################################################################
#Exploring Transformations
##################################################################################################################

#currently we have a model with no interactions, and a model with interactions
#now we would like to look at transformations

#response needs to be transformed for sure.
boxcox(current_chosen_model_int, plotit = TRUE)
boxcox(chosen_model_additive, plotit = TRUE)

#chosen_model_additive
#current_chosen_model_int
y_new = log(fb_limited$y)

fb_withResponseTransform = cbind( fb_limited, y_new)
View(fb_withResponseTransform)
pairs(fb_withResponseTransform)


summary(current_chosen_model_int)
summary(chosen_model_additive)

#current_add_model = x4 + x10 + x11 + x8 + x5 + x27 + x9 + x26 + x2 + x14 + x3
#current_chosen_model_int = x4 + x10 + x11 + x8 + x5 + x27 + x9 + x26 + x2 + x14 + x10:x27 + x5:x27


#Based on pairs plot, x4 is either logrithmic or to a higher power

#Examine x4 transformations
#backward


#AIC seems to agree
model_log_response = lm(log(y) ~ x4 + x10 + x11 + x8 + x5 + x27 + x9 + x26 + x2 + x14 + x3, data = fb_limited)

model_tranformed = lm(log(y) ~ x4 + x10 + x11 + x8 + x5 + x27 + x9 + x26 + x2 + x14 + x3 + log(x4) + I(x4^2), data = fb_limited)

model_back_aic = step(model_tranformed, direction = "backward", trace = 0)
summary(model_back_aic)
car::vif(model_back_aic)
#VIF for x4 ^ 2 is high, will keep the log(x4)
model_tranformed = lm(log(y) ~ x4 + x10 + x11 + x8 + x5 + x27 + x9 + x26 + x2 + x14 + x3 + log(x4), data = fb_limited)
car::vif(model_tranformed)
anova(model_log_response, model_tranformed)
#keep the model transformed

x4_trans = diagnostics(model_tranformed, testit = TRUE, pcol = "darkorange", lcol = "dodgerblue", plotit = TRUE)
x4_trans$shapiro_p_value
x4_trans$bp_p_value

#look at taking out extreme points
infl_value = 4/length(cooks.distance(model_tranformed))

indexes_to_remove = unname(which(cooks.distance(model_tranformed) > infl_value))
length(indexes_to_remove)

remove_influence =  fb_limited[ -indexes_to_remove, ]
model_removed_influence  = lm(log(y) ~ x4 + x10 + x11 + x8 + x5 + x27 + x9 + x26 + x2 + x14 + x3 + log(x4), data = remove_influence)
x4_trans = diagnostics(model_tranformed, testit = TRUE, pcol = "darkorange", lcol = "dodgerblue", plotit = TRUE)
x4_trans$shapiro_p_value
x4_trans$bp_p_value

#even removing points of influence from the model did not allow it to pass the bp or shapiro tests

#####################################
#X10
####################################
#Examine x10 transformations

fb_subset = (fb_limited[log(fb_limited$x10) != "-Inf", ])

model_log_response = lm(log(y) ~ x4 + x10 + x11 + x8 + x5 + x27 + x9 + x26 + x2 + x14 + x3, data = fb_subset)
model_back_aic = step(model_log_response, direction = "backward", trace = 0)

summary(model_back_aic)
vif(model_back_aic)
result = diagnostics(model_back_aic, testit = TRUE, pcol = "darkorange", lcol = "dodgerblue", plotit = TRUE)
result$shapiro_p_value
result$bp_p_value


model_tranformed    = lm(log(y) ~ x4 + x10 + x11 + x8 + x5 + x27 + x9 + x26 + x2 + x14 + x3 + log(x10) + I(x10^2), data = fb_subset)
vif(model_tranformed)
model_back_aic = step(model_tranformed, direction = "backward", trace = 0)
#keep the log of 10
x10_trans = diagnostics(model_back_aic, testit = TRUE, pcol = "darkorange", lcol = "dodgerblue", plotit = TRUE)
result$shapiro_p_value
result$bp_p_value


#something good is here
anova(model_log_response, model_tranformed)


###checking for good stuff

model_back_aic = step(model_tranformed, direction = "backward", trace = 0)
summary(model_back_aic)
#suggests keeping the log(x10)
car::vif(model_back_aic)
anova(model_log_response, model_back_aic)$"Pr(>F)"


#improved the VIF
x10_trans = diagnostics(model_tranformed, testit = TRUE, pcol = "darkorange", lcol = "dodgerblue", plotit = TRUE)
x10_trans$shapiro_p_value
x10_trans$bp_p_value

#need to "transform the data" so we can take the log of the predictor since it includes 0
fb_transform = fb_limited
fb_transform$x10  = fb_transform$x10 + 1
#do note that x10 by itself does not violate the model assumptions
log_log_model_add = lm(log(y) ~ x10 + log(x10), data = fb_transform)
log_log_model_add_results = diagnostics(log_log_model_add, testit = TRUE, pcol = "darkorange", lcol = "dodgerblue")
log_log_model_add_results$shapiro_p_value #normality is still suspect
log_log_model_add_results$bp_p_value      #not as good bp value as seen when just x4 was in the model, suggesting log(x4) could replace x4

log_log_model_add = lm(log(y) ~ x10 + log(x10) + log(x4) + x4, data = fb_subset)
log_log_model_add_results = diagnostics(log_log_model_add, testit = TRUE, pcol = "darkorange", lcol = "dodgerblue")
log_log_model_add_results$shapiro_p_value #normality is still suspect
log_log_model_add_results$bp_p_value      #not as good bp value as seen when just x4 was in the model, suggesting log(x4) could replace x4

#what about if I removed influencatioal points
indexes_to_remove = unname(which(cooks.distance(log_log_model_add) > infl_value))
remove_influence =  fb_subset[ -indexes_to_remove, ]

log_log_model_add = lm(log(y) ~ x10 + log(x10) + log(x4) + x4, data = remove_influence)
log_log_model_add_results = diagnostics(log_log_model_add, testit = TRUE, pcol = "darkorange", lcol = "dodgerblue")
log_log_model_add_results$shapiro_p_value #normality is still suspect
log_log_model_add_results$bp_p_value      #not as good bp value as seen when just x4 was in the model, suggesting log(x4) could replace x4

#so we now say that x4 is removed from the model even though it has the lowest RSS for parameter size 1

###################################################
#x11 on the pairs plot looks prettty simliar to x10
####################################################
fb_subset = (fb_limited[log(fb_limited$x10) != "-Inf", ])
fb_subset = (fb_subset[log(fb_subset$x11) != "-Inf", ])
nrow(fb_subset)
#482 rows

log_log_model_current = lm(log(y) ~ x10 + log(x10), data = fb_subset)

log_log_x11 =  lm(log(y) ~ x10 + log(x10) + log(x11) + x11, data = fb_subset)


x11_trans = diagnostics(log_log_x11, testit = TRUE, pcol = "darkorange", lcol = "dodgerblue", plotit = TRUE)
x11_trans$shapiro_p_value
x11_trans$bp_p_value
vif(log_log_x11)

#get rid of x11?
log_log_x11_new =  lm(log(y) ~ x10 + log(x10) + log(x11), data = fb_subset)
vif(log_log_x11_new) #looks really good

anova(log_log_x11, log_log_x11_new)
#prefer the old model
currently_preferred =  lm(log(y) ~ x10 + log(x10) + log(x11), data = fb_subset)

summary(currently_preferred)$adj.r.squared
summary(chosen_model_additive)$adj.r.squared
summary(current_chosen_model_int)$adj.r.squared


calc_loocv_rmse = function(model) {
  sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
}


calc_loocv_rmse_with_log_response = function(data, model) {
  sqrt((mean(data$y) - exp(fitted(model)))^2)
}



calc_loocv_rmse(currently_preferred)
calc_loocv_rmse(chosen_model_additive)
calc_loocv_rmse(current_chosen_model_int)


############################
#x8 analysis
############################
model = lm(log(y) ~ x10 + log(x10) + log(x11) + log(x8) + I(x8 ^ 2) + x8, data = fb_subset)
model_back_aic = step(model, direction = "backward", trace = 0)
summary(model_back_aic)
vif(model_back_aic)

model = lm(log(y) ~ x10 + log(x10) + log(x11) + log(x8) + I(x8 ^ 2), data = fb_subset)
model_back_aic = step(model, direction = "backward", trace = 0)
summary(model_back_aic)
vif(model_back_aic)

model = lm(log(y) ~ x10 + log(x10) + log(x11)  + I(x8 ^ 2), data = fb_subset)
model_back_aic = step(model, direction = "backward", trace = 0)
summary(model_back_aic)
vif(model_back_aic)

#like the low vif, now check against assumptions of the model, this doesn't work
x8_trans = diagnostics(model_back_aic, testit = TRUE, pcol = "darkorange", lcol = "dodgerblue", plotit = TRUE)
x8_trans$bp_p_value
x8_trans$shapiro_p_value

#####checking with the log
model = lm(log(y) ~ x10 + log(x10) + log(x11)  + log(x8), data = fb_subset)
model_back_aic = step(model, direction = "backward", trace = 0)
summary(model_back_aic)
vif(model_back_aic)

#like the low vif, now check against assumptions of the model, this doesn't work
x8_trans = diagnostics(model_back_aic, testit = TRUE, pcol = "darkorange", lcol = "dodgerblue", plotit = TRUE)
x8_trans$bp_p_value
x8_trans$shapiro_p_value
#x8 appears to make us 


####################################################################################


anova(model_log_response, model_back_aic)
#keep the model transformed

x4_trans = diagnostics(model_tranformed, testit = TRUE, pcol = "darkorange", lcol = "dodgerblue", plotit = TRUE)
x4_trans$shapiro_p_value
x4_trans$bp_p_value

#look at taking out extreme points
infl_value = 4/length(cooks.distance(model_tranformed))

indexes_to_remove = unname(which(cooks.distance(model_tranformed) > infl_value))
length(indexes_to_remove)

remove_influence =  fb_limited[ -indexes_to_remove, ]
model_removed_influence  = lm(log(y) ~ x4 + x10 + x11 + x8 + x5 + x27 + x9 + x26 + x2 + x14 + x3 + log(x4), data = remove_influence)
x4_trans = diagnostics(model_tranformed, testit = TRUE, pcol = "darkorange", lcol = "dodgerblue", plotit = TRUE)
x4_trans$shapiro_p_value
x4_trans$bp_p_value





linear_model  = lm(y ~ x10, data = fb_limited)
linear_results = diagnostics(linear_model, testit = TRUE, pcol = "darkorange", lcol = "dodgerblue")
linear_results$shapiro_p_value
linear_results$bp_p_value

#remove_influence
linear_model  = lm(y ~ x10, data = remove_influence)
linear_results = diagnostics(linear_model, testit = TRUE, pcol = "darkorange", lcol = "dodgerblue")
linear_results$shapiro_p_value
linear_results$bp_p_value


log_response  = lm(log(y) ~ x10, data = fb_limited, na.action=na.exclude)
log_response_results = diagnostics(log_response, testit = TRUE, pcol = "darkorange", lcol = "dodgerblue")
log_response_results$shapiro_p_value
log_response_results$bp_p_value

log_response  = lm(log(y) ~ x10, data = remove_influence, na.action=na.exclude)
log_response_results = diagnostics(log_response, testit = TRUE, pcol = "darkorange", lcol = "dodgerblue")
log_response_results$shapiro_p_value
log_response_results$bp_p_value


pairs(fb_limited)

log_log_model = lm(log(y) ~ log(x10), data = fb_limited,  na.action=na.omit)
log_log_model_results = diagnostics(log_log_model, testit = TRUE, pcol = "darkorange", lcol = "dodgerblue")
log_log_model_results$shapiro_p_value #normality is still suspect
log_log_model_results$bp_p_value      #pretty good bp value

#required to 
fb_subset = (fb_limited[log(fb_limited$x10) != "-Inf", ])
nrow(fb_subset)

View(fb_subset)
(fb_limited[log(fb_limited$x10) == "Inf", ])

fb_limited[ ,log(fb_limited$x10)  == "Inf" ]

#this looks gerat, but requires subsetting the data.
log_log_model_add = lm(log(y) ~ x10 + log(x10), data = fb_subset)
log_log_model_add_results = diagnostics(log_log_model_add, testit = TRUE, pcol = "darkorange", lcol = "dodgerblue")
log_log_model_add_results$shapiro_p_value #normality is still suspect
log_log_model_add_results$bp_p_value      #not as good bp value as seen when just x4 was in the model, suggesting log(x4) could replace x4


#removig the points of influence
log_log_model_add = lm(log(y) ~ x10 + log(x10), data = remove_influence)
log_log_model_add_results = diagnostics(log_log_model_add, testit = TRUE, pcol = "darkorange", lcol = "dodgerblue")
log_log_model_add_results$shapiro_p_value #normality is still suspect
log_log_model_add_results$bp_p_value      #not as good bp value as seen when just x4 was in the model, suggesting log(x4) could replace x4



log_quad_model = lm(log(y) ~ x10 + I(x10 ^ 2), data = fb_limited)
log_quad_model_add_results = diagnostics(log_quad_model, testit = TRUE, pcol = "darkorange", lcol = "dodgerblue")
log_quad_model_add_results$shapiro_p_value #normality is still suspect
log_quad_model_add_results$bp_p_value      #not as good bp value as seen when just x4 was in the model, suggesting log(x4) could replace x4

#required to subset the data.
fb_subset = (fb_limited[log(fb_limited$x10) != "-Inf", ])
log_log_quad_model = lm(log(y) ~ x10 + I(x10 ^ 2) + log(x10), data = fb_subset)
log_log_quad_model_results = diagnostics(log_log_model_add, testit = TRUE, pcol = "darkorange", lcol = "dodgerblue")
log_log_quad_model_results$shapiro_p_value #normality is still suspect
log_log_quad_model_results$bp_p_value      #not as good bp value as seen when just x4 was in the model, suggesting log(x4) could replace x4
result = rbind(c("y ~ x4",                            linear_results$shapiro_p_value, linear_results$bp_p_value ),
               c("log(y) ~ x4",                       log_response_results$shapiro_p_value, log_response_results$bp_p_value ),
               c("log(y) ~ log(x4)",                  log_log_model_results$shapiro_p_value, log_log_model_results$bp_p_value ),
               c("log(y) ~ x4 + log(x4)",             log_log_model_add_results$shapiro_p_value, log_log_model_add_results$bp_p_value ),
               c("log(y) ~ x4 + I(x4 ^ 2)",           log_quad_model_add_results$shapiro_p_value, log_quad_model_add_results$bp_p_value ),
               c("log(y) ~ x4 + I(x4 ^ 2) + log(x4)", log_log_quad_model_results$shapiro_p_value, log_log_quad_model_results$bp_p_value )
)

colnames(result) = c("Transformation", "Shapiro p Value", "BP p value")
knitr::kable(result, format = "markdown", padding = 3)

###transformed data.
test = lm(log(y) ~ x10 + I(x10 ^ 2) , data = fb_subset)
test_result = diagnostics(test, testit = TRUE, pcol = "darkorange", lcol = "dodgerblue")
test_result$shapiro_p_value #normality is still suspect
test_result$bp_p_value      #not as good bp value as seen when just x4 was in the model, suggesting log(x4) could replace x4

#I have found that if I remove an additional 5 data points things look much better
#after adding polynomials

##################################################################################


par(mfrow = c(6, 2))
plot_fitted_resid(linear_model)
plot_qq(linear_model)

plot_fitted_resid(log_response)
plot_qq(log_response)

plot_fitted_resid(log_log_model)
plot_qq(log_log_model)

plot_fitted_resid(log_log_model_add)
plot_qq(log_log_model_add)

plot_fitted_resid(log_quad_model)
plot_qq(log_quad_model)

plot_fitted_resid(log_log_quad_model)
plot_qq(log_log_quad_model)

#plot_x4_curve(linear_model)

plot_fitted_resid(log_response)
plot_x4_curve(log_response)

par(mfrow = c(2, 2))
plot(log(y) ~ log(x4), data = fb_limited, col = "dodgerblue", pch = 20, cex = 1.5)
y_x4_loglog = lm(log(y) ~ log(x4), data = fb_limited)
abline(y_x4_loglog, col = "darkorange", lwd = 2)
plot(fitted(y_x4_loglog), resid(y_x4_loglog), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

plot_x4_curve(log_log_model, TRUE, "log(x4)")
plot_fitted_resid(log_log_model)


plot_fitted_resid(log_log_model_add)
plot_x4_curve(log_log_model_add)

plot_fitted_resid(log_quad_model)
plot_x4_curve(log_quad_model)

plot_fitted_resid(log_log_quad_model)
plot_x4_curve(log_log_quad_model)

plot(y ~ x4, data = fb_limited, col = "dodgerblue", pch = 20, cex = 1.5)
y_x4 = lm(y ~ x4, data = fb_limited)
abline(y_x4, col = "darkorange", lwd = 2)
plot(fitted(y_x4), resid(y_x4), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

#increasing variance so attempt log transform which is also suggested by box-cox
#par(mfrow = c(2, 2))
plot(log(y) ~ x4, data = fb_limited, col = "dodgerblue", pch = 20, cex = 1.5)
y_x4_log = lm(log(y) ~ x4, data = fb_limited)
abline(y_x4_log, col = "darkorange", lwd = 2)
plot(fitted(y_x4_log), resid(y_x4_log), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

#now try a log-transform on the predictor
#par(mfrow = c(1, 2))

plot(log(y) ~ log(x4), data = fb_limited, col = "dodgerblue", pch = 20, cex = 1.5)
y_x4_loglog = lm(log(y) ~ log(x4), data = autompg)
abline(y_x4_loglog, col = "darkorange", lwd = 2)
plot(fitted(y_x4_loglog), resid(y_x4_loglog), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)






#now try log-transform on response and quadractic on predictor
#now try a log-transform on the predictor
#par(mfrow = c(1, 2))
plot(log(y) ~ x4 + I(x4 ^ 2), data = fb_limited, col = "dodgerblue", pch = 20, cex = 1.5)
y_x4_log_x2 = lm(log(y) ~ x4 + I(x4 ^ 2), data = fb_limited)
abline(y_x4_log_x2, col = "darkorange", lwd = 2)
plot(fitted(y_x4_log_x2), resid(y_x4_log_x2), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)


################################################################################
#good small addidive model, so now will look at pairs plot and box cox to 
#determine transformations
################################################################################
model = model_back_bic
boxcox(model, plotit = TRUE)
#indicates that we should be taking the log of response
model_log = lm(log(y) ~ x4  + x10 +  x11 + x8 + x5 + x27 + x9 + x26 +  x2 +  x14 + x3, data = new_data)
model     = lm(y ~ x4  + x10 +  x11 + x8 + x5 + x27 + x9 + x26 +  x2 +  x14 + x3, data = new_data)
car::vif(model_log)
car::vif(model)








####################
#looking at data
####################

#look at points with high leverage
sum(hatvalues(chosen_model_additive) > 7 * mean(hatvalues(chosen_model_additive)))

sum(hatvalues(current_chosen_model_int) > 7 * mean(hatvalues(current_chosen_model_int)))


which((hatvalues(chosen_model_additive) > 7 * mean(hatvalues(chosen_model_additive))))

which((hatvalues(current_chosen_model_int) > 7 * mean(hatvalues(current_chosen_model_int))))


#identify outlier
rstandard(chosen_model_additive)[abs(rstandard(chosen_model_additive)) > 2]

rstandard(current_chosen_model_int)[abs(rstandard(current_chosen_model_int)) > 2]

#32 influential points
infl_value_additive = 4/length(cooks.distance(chosen_model_additive))

which((cooks.distance(chosen_model_additive) > infl_value_additive))


infl_value_int = 4/length(cooks.distance(current_chosen_model_int))

indexes_int = unname(which(cooks.distance(current_chosen_model_int) > infl_value_int))
indexes_add = unname(which(cooks.distance(chosen_model_additive) > infl_value_additive))


indexes_int
indexes_add
ptsToRemove = c(140, 165, 169, 241, 443, 457)

remove_influence =  fb_limited[ -ptsToRemove, ]
nrow(remove_influence)
nrow(fb_limited)