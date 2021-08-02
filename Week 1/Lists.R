##list, 1 dimensional data struture of different data types
## list of vectors, matrixes, and functions

list(42, "hello", TRUE)

ex_list = list(
  a = c(1, 2, 3, 4),
  b = TRUE, 
  c = "Hello!",
  d = function(arg = 42) {print("Hello World")},
  e = diag(5)
)

ex_list

## subset the list

ex_list$e

ex_list$d("blah")   # makes a function call

#returns list of length 2 - with the first 2 elements
ex_list[1:2]

ex_list[1] # This is still a list since it has the named element - first element which is a vector

ex_list[[1]] #This is a vector not the list


ex_list[c("e", "a")] # This returns a list

ex_list["e"] # returns a list  - the matrix stored in e

ex_list[["e"]] # returns the matrix value inside e
