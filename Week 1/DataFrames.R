#Ties list and vectors
#Data Frame
#list of vectors


example_data = data.frame(
  x = c(1, 3, 5, 7, 9, 1, 3, 5, 7, 9),
  y = c(rep("Hello", 9), "Goodbye"),
  z = rep(c(TRUE, FALSE), 5)
)

example_data

View(example_data)
## Actual List of vectors
list(
  x = c(1, 3, 5, 7, 9, 1, 3, 5, 7, 9),
  y = c(rep("Hello", 9), "Goodbye"),
  z = rep(c(TRUE, FALSE), 5)
)

example_data$x #returns a vector


all.equal(length(example_data$x),
          length(example_data$y),
          length(example_data$z))

##structure of data frame
str(example_data) #note x is numeric, y is a factor, and z is logical
