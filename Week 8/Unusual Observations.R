#leverages

hatvalues(fit_1)
hatvalues(fit_2)
hatvalues(fit_3)

#standard residuals
rstandard(fit_1)
rstandard(fit_2)
rstandard(fit_3)

#large standarized residuals
rstandard(fit_1)[abs(rstandard(fit_1)> 2)]
rstandard(fit_2)[abs(rstandard(fit_2)> 2)]
rstandard(fit_3)[abs(rstandard(fit_3)> 2)]

#cooks distance incluence
cooks.distance(fit_1)[11] > 4 /length(cooks.distance(fit_1))

cooks.distance(fit_2)[11] > 4 /length(cooks.distance(fit_2))


#looking at mtcars dataset

mpg_hp_add = lm(mpg ~ hp + am, data = mtcars)

#how many are considered large leverages
(sum(hatvalues(mpg_hp_add) > 2 * mean(hatvalues(mpg_hp_add))))

#how many have large residual, maybe influential point
sum(abs(rstandard(mpg_hp_add)) > 2)


cd_mpg_hp_add = cooks.distance(mpg_hp_add)

#there appear to be 2 points
sum(cd_mpg_hp_add > 4 / length(cd_mpg_hp_add))


large_cd_mpg = cd_mpg_hp_add > 4 / length(cd_mpg_hp_add)

cd_mpg_hp_add[large_cd_mpg]

coef(mpg_hp_add)

#remove 2 influential points
mpg_hp_add_fix = lm(mpg ~ hp + am, data = mtcars,
                    subset = cd_mpg_hp_add <= 4 / length(cd_mpg_hp_add))

coef(mpg_hp_add_fix)

par(mfrow = c(2,2))
plot(mpg_hp_add)
