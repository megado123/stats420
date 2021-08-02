#vectorization
x = c(1, 3, 5, 7, 8, 9)
y = 1:100

x + 2
log(x)
sqrt(x)
x ^ 2
x + rep(2, 6)
x
all(x + y == rep(x, 10) + y)
y = 1:60
all(x + y == rep(x, 10) + y)
identical(x + y, rep(x, 10) + y)
any( x + y  == rep(x, 10) + y)
all.equal(x + y, rep(x, 10) + y)
x + y == rep(x, 10) + y
