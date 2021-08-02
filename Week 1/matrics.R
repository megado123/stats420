## matrices
x = 1:9
x
X = matrix(x, nrow = 3, ncol = 3)
X

Y = matrix(x, nrow = 3, ncol = 3, byrow = TRUE)
Y

Z = matrix(0, 2,4) ##zeros matrix row =2, col = 4
Z

X[1, 2] ## row 1, column 2
X[1, ]  ## vector of size 3, first row
X[ , 2 ]  ## vecotr of size 3, 2nd column
X[2, c(1,3)] ## vector of size 2, second row, value 2,8

##column bind
x
cbind(col1 = x, col2 = rev(x), col3 = rep(1,9))

X + Y
X-Y
X*Y ## not matrix mulitiplication, element by element
X %*% Y ##matrix multiplication

Z = matrix( c(9,2, -3, 2, 4, -2, -3, -2, 16), 3, byrow = TRUE)
Z
solve(z) ## get it's inverse

solve(Z) %*% Z

all.equal(solve(Z) %*% Z, diag(3))

X
solve(X) ## X is singular so no inverse?

dim(X) ## dimensions (3 x 3)

nrow(X) # number of rows

ncol(X) ## number of columns

rowSums(X) ## summation for each row

colMeans(X)
