# P | X > 1.3 |
x = 1.3
n = 7
pt(1.3, 6, lower.tail = TRUE)

#Question 3
standard_dev =  sqrt(4/1.5)
pnorm(q = 4.2, mean = 2, sd = standard_dev, lower.tail = FALSE)


View(faithful)

#Question 7
model = lm(eruptions ~ waiting, data = faithful)

