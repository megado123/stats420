#2 ways to specify the same model

#additive model
mpg_disp_add = lm(mpg ~ disp + domestic, 
                  data = autompg)

mpg_disp_int = lm(mpg ~ disp + domestic + disp:domestic, 
                  data = autompg)

mpg_disp_int2 = lm(mpg ~  disp * domestic, 
                  data = autompg)


coef(mpg_disp_int)
coef(mpg_disp_int2)

#do we need parrel lines or different slopes?
#disp:domestic or additive vs integarction

summary(mpg_disp_int)[1,1]

#t-test or anova f test
anova(mpg_disp_add, mpg_disp_int)
summary(mpg_disp_int)

int_for = coef(mpg_disp_int)[1]
int_dom = coef(mpg_disp_int)[1] + coef(mpg_disp_int)[3]

#2 lines with 2 different slopes, 
slope_for = coef(mpg_disp_int)[2]
slope_dom = coef(mpg_disp_int)[2] + coef(mpg_disp_int)[4]
plot(mpg ~ disp, data = autompg, col = domestic + 1, pch = domestic + 1)
abline(int_for, slope_for, col = 1, lty = 1, lwd = 2) #line for foreign cars
abline(int_for, slope_dom, col = 1, lty = 1, lwd = 2) #line for foreign cars

#2 numeric variables
mpg_disp_add_hp = lm(mpg ~ disp + hp, data = autompg)
mpg_disp_int_hp = lm(mpg ~ disp * hp, data = autompg)

summary(mpg_disp_int_hp)

#Ho = beta_3 = 0

anova(mpg_disp_add, mpg_disp_int_hp)
summary(mpg_disp_int_hp)

#example with cylnders = factor variables = 3 dummy variables
#additive model, only 1 slope
mpg_disp_add_cyl = lm(mpg ~ disp + cyl, data = autompg)

summary(mpg_disp_add_cyl)

#look at interaction between disp and cylc
mpg_disp_int_cyl = lm(mpg ~ disp * cyl, data = autompg)
#could also use mpg ~ disp + cyl + disp:cyl
summary(mpg_disp_int_cyl)

#y = beta_0 + beta_1 x + beta_2 v_2 + beta_3 v3 + \gamma2 x v_2 + \gamma_3 x v_3 + epsilon

int_4cyl = coef(mpg_disp_int_cyl)[1]
int_6cyl = coef(mpg_disp_int_cyl)[1] +  coef(mpg_disp_int_cyl)[3]
int_8cyl = coef(mpg_disp_int_cyl)[1] +  coef(mpg_disp_int_cyl)[4]

slope_4cyl = coef(mpg_disp_int_cyl)[2]
slope_6cyl = coef(mpg_disp_int_cyl)[2] + coef(mpg_disp_int_cyl)[5]
slope_8cyl = coef(mpg_disp_int_cyl)[2] + coef(mpg_disp_int_cyl)[6]

plot_colors = c("Darkorange", "Darkgrey", "Dodgerblue")
plot(mpg ~ disp, data = autompg, col = plot_colors[cyl], pch = as.numeric(cyl))
abline(int_4cyl, slope_4cyl, col = plot_color[1], lty = 1, lwd = 2)
abline(int_6cyl, slope_6cyl, col = plot_color[2], lty = 1, lwd = 2)
abline(int_8cyl, slope_8cyl, col = plot_color[3], lty = 1, lwd = 2)

#compare interactions, so gamma 2 and gamma 3 are equal to 0
#difference of 2 parameters

anova(mpg_disp_add_cyl, mpg_disp_int_cyl)
#prefer different slopes

#parameterization
#when we give disp * cylc


lm(mpg ~ disp * cyl, data = autompg)
lm(mpg ~ 0 + cyl * disp, data = autompg)
