mpg = ggplot2::mpg

hist(mpg$cty)

mean(mpg$cty)
sd(mpg$cty)

hist(mpg$cty,
     xlab = "Miles Per Gallon (City)",
     main = "Histogram of MPG (City)",
     breaks = 12, 
     col = "darkorange",
     border = "dodgerblue")

#barplots (categorial)
mpg$drv

?barplot

barplot(mpg$drv)
barplot(table(mpg$drv))

mpg$drv

table(mpg$drv)

#summarize single variable
barplot(table(mpg$drv))

#boxlpt

boxplot(hwy ~ drv, data = mpg)
