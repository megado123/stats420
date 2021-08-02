#import dataset from ggplot package
mpg = ggplot2::mpg
View(mpg)
?mpg
mpg  ## by default it is a tibble

mean(mpg$cty)  #average

median(mpg$cty)

#Measuring Spread
var(mpg$cty)
sd(mpg$cty)     ## average deviation, how far away from mean
range(mpg$city) ## min and max
IQR(mpg$cty)

summary(mpg$cty)
