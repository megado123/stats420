#programming basics

##if, else

x  = c(1,3)
x
y = c(3, 1)
y

if (x > y){
  z = x + y
  print("x is larger than y")
} else{
  z = x + 5 * y
  print("x is les than y")
}
z

## ifelse
ifelse(4>3, 1,0)

fib = c(1, 1, 2, 3, 5, 8, 13, 21)
ifelse(fib > 6, "Foo", "Bar")


## for loops
x = 11:15
for (i in 1:5){
  x[i] = x[i] * 2
}
x

## better
x = 11:15
x = x * 2
x

# functions

## using
seq(from = 1.5, to = 4.2, by = 0.1)
seq(1.5, 4.2, 0.1)

#writing

silly_fun = function(arg1, arg2, arg3 = 42){
  a = arg1 + arg2 - 3
  b = a* arg3
  c(a, b, a + b, 0)
}

silly_fun(arg1 = 2, arg2 = 3, arg3 = 42)

silly_fun(arg1 = 2, arg2 = 3)

silly_fun(2, 3)

## style
seq(from = 1.5, to = 4.2, by = 0.1)

