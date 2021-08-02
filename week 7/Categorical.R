

mtcars

plot(mpg ~ hp, data = mtcars, cex = 2)
plot(mpg ~ hp, data = mtcars, col = am + 1, pch = am + 1, cex = 2)
abline(mpg_hp_slr)

(mpg_hp_slr = lm(mpg ~ hp, data = mtcars))

summary(mpg_hp_slr)

#x2 = numeric variable, but represents categorical

mpg_hp_add = lm(mpg ~ hp + am, data = mtcars)

int_auto = coef(mpg_hp_add)[1]
int_man  = coef(mpg_hp_add)[1] + coef(mpg_hp_add)[3]

slope_auto = coef(mpg_hp_add)[2]
slope_man  = coef(mpg_hp_add)[2]

summary(mpg_hp_add)


plot(mpg ~ hp, data = mtcars,  col = am + 1, pch = am + 1, cex = 2)
abline(int_auto, slope_auto, col = 1, lty = 1, lwd = 2)
abline(int_man, slope_man, col = 2, lty =2, lwd = 2)

#H0: B2 = 0, b2 <> 0
#testing 1 line vs 2 lines

anova(mpg_hp_slr, mpg_hp_add)
summary(mpg_hp_add)$coefficients["am", ]

#options(scipen = 1, digits = 2)

