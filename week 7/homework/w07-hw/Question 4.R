#Question 4

n = 20

as.numeric("DodgerBlue")

sim_data = data.frame(
  groups = c(rep("A", n / 2), rep("B", n / 2)),
  values = rep(0, n))
#str(sim_data)

#View(sim_data)
options(digits=7)

as.numeric(epa$type) + 1


sim_data$values = rnorm(n, mean = 5, sd = 2.2) # simulate response data

#View(sim_data)

summary(lm(values ~ groups, data = sim_data))

model = lm(values ~ groups, data = sim_data)

(p_value = summary(model)$coefficients[2, 4])
(t_value = summary(model)$coefficients[2, 3])



names(summary(lm(values ~ groups, data = sim_data)))

t.test(values ~ groups, data = sim_data, var.equal = TRUE)$statistic

t.test(values ~ groups, data = sim_data, var.equal = TRUE)$p.value



