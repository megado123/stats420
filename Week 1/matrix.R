x = 1:9
X = matrix(x, nrow = 3, ncol = 3)
X
Y = matrix(x, nrow = 3, ncol =3, byrow = TRUE)
Y
Z = matrix(0, 2, 4)
Z

X[1,2]
X[1, ]
X[ , 2]
X[2, c(1,2)]

## column bind
cbind( col1 = x, col2 = rev(x), col3 = rep(1, 9))

cbind( col1 = x, col2 = rev(x))


X + Y
