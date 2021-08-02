plot(mpg ~ hp, data = mtcars, col = am + 1, pch = am + 1, cex = 2)
legend("topright", c("Automatic", "Manual"), col = c(1, 2), pch = c(1, 2))

mpg_hp_slr = lm(mpg ~ hp, data = mtcars)

plot(mpg ~ hp, data = mtcars, col = am + 1, pch = am + 1, cex = 2)
abline(mpg_hp_slr, lwd = 3, col = "grey")
legend("topright", c("Automatic", "Manual"), col = c(1, 2), pch = c(1, 2))

#dummy variable
#x2   1 = manual
#     0 = automatic

#x2 term = 0, x2 just goes away, so then it is automatic
#
#Y = (Bo + B2) + b1x1 + e manual
#Y =  Bo       + b1x1 + e  automatic
#would obtain estimates
#beta_0_hat = estimate ave fuel eff when x1 and x2 = 0
#beta_1_hat = estimate ave fule eff changes as hp increases (independent of transmission)
#beta_2_hat = estimated difference in average ful effiency between manual and automatic
#beta_0_hat + beta_2_hat = ave fuel efficincy when tranmssion is manual
#1 model, but 2 lines

#H0: B2 = 0 vs H1 B2 <> 0
#null y = bo + b1x1  y = bo + b1x1 + b2x2
# a dummy variable allows for 2 regressions

#what if there are more than 1 cateogories
#x2 = automatic, manual, cvt?
#what about different slopes?

#Cat