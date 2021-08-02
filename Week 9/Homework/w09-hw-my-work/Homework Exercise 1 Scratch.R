?longley

cor(longley)




indexs_to_check = (cor(longley(which(cor(longley) > cor(longley$Year, longley$GNP)))))


blah = cor(longley) != 1


#subset that does not


subset = cor(longley)[cor(longley) != 1]
which.max(subset)
#highest correlation
subset[11]
which(cor(longley) == data[11]) #13 and 37

correlations = cor(longley)
correlations = as.data.frame(correlations)
rownames(correlations)
c(colnames(correlations)[2], rownames(correlations)[1])


model = lm(Employed ~ ., data = longley)

blah = vif(model)
str(blah)

which.max(vif(model))

which(vif(model) > 5)

model_vif = as.data.frame(vif(model))

model_vif[which.max(model_vif[ , 1]) ,]

model_vif[ ,2]



model = lm(Employed ~ ., data = longley)
model_vif = as.data.frame(vif(model))
#maximum variance
model_vif[which.max(model_vif[ , 1]) ,]

#GNP is the largest:
row.names(model_vif)[which.max(model_vif[ , 1]) ]
#with a value of:
model_vif[which.max(model_vif[ , 1]) ,]


result = rbind(row.names(model_vif)[which(model_vif[ , 1] > 5) ], model_vif[which(model_vif[ , 1] > 5) ,])
kable(result, format = "markdown", padding = 3)

model_vif[model_vif$`vif(model)` > 5, ]

model_vif

#large variation is explained by other predictors (with a high R(4)^2)
#so then the variance of Beta_4 is going to be very lare

#1f
model = lm(Employed ~ ., data = longley)
model_new = lm(Employed ~ Unemployed + Armed.Forces + Year, data = longley)


summary(model_new)

summary(model_new)$fstatistic

anova(model_new, model)

#1f"
anova(model_new, model)$`Pr(>F)`[2]
