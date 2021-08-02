#linear model

#trees$Volume <- NULL (removing a column from a dataset)
?cars
cars
?lm
stop_dist_model = lm(dist~ speed, data = cars)
stop_dist_model

names(stop_dist_model)

stop_dist_model$coefficients
stop_dist_model$fitted.values
stop_dist_model$residuals

coef(stop_dist_model)
fitted(stop_dist_model)
resid(stop_dist_model)

cars$dist == fitted(stop_dist_model) + resid(stop_dist_model)

all.equal(cars$dist - stop_dist_model$fitted.values, stop_dist_model$residuals)

summary(stop_dist_model)

names(summary(stop_dist_model))

summary(stop_dist_model)$r.squared

summary(stop_dist_model)$sigma

#predict
#plugging in x values into a fitted model
predict(stop_dist_model, newdata = data.frame(speed = 1))

predict(stop_dist_model, newdata = data.frame(speed = c(1,3)))

#get fitted values - these are all the same
predict(stop_dist_model, newdata = cars)
predict(stop_dist_model)
fitted(stop_dist_model)

##fitting a model continued
waiting_model = lm(eruptions ~ waiting, data = faithful)
fitted_data = predict(waiting_model, newdata = data.frame(faithful))

names(waiting_model)

faithful$fitted.values

(sum( (faithful$eruptions  - faithful$fitted.values )^ 2))/ (length(fitted.values) - 2)

temp = (sum( (faithful$eruptions  - fitted_data )^ 2))/270

summary(waiting_model)

#more notes
#linear model

trees
trees$Volume <- NULL

str(trees)
?trees
View(trees)

#x = trees$Height
#y = trees$Girth
#t = c(x,y)

girth_model = lm(Girth ~ Height, data = trees)

girth_model

names(girth_model)

girth_model$coefficients
girth_model$fitted.values
girth_model$residuals

summary(girth_model)

coef(girth_model)
fitted(girth_model)
resid(girth_model)



cars$dist == fitted(stop_dist_model) + resid(stop_dist_model)

all.equal(cars$dist - stop_dist_model$fitted.values, stop_dist_model$residuals)

summary(stop_dist_model)

names(summary(stop_dist_model))

summary(stop_dist_model)$r.squared

summary(stop_dist_model)$sigma

#predict
#plugging in x values into a fitted model
predict(stop_dist_model, newdata = data.frame(speed = 1))

predict(stop_dist_model, newdata = data.frame(speed = c(1,3)))

#get fitted values - these are all the same
predict(stop_dist_model, newdata = cars)
predict(stop_dist_model)
fitted(stop_dist_model)
