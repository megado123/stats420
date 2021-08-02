set.seed(42)
a_vector = rpois(250, lambda = 6)

a_vector
sum(as.numeric(a_vector >= 3))

###################
x = 1:100
x
##  Create a new vector y, 
##  which adds 5 to the elements stored in odd indices of x 
##  and subtracts 10 from the elements stored in even indices of x.
##
##

## this is improper, but would like to understand why
if (x %% 2 == 0){
  z = x - 10
} else{
  z = 5 + x
}
sd(z)

## this is the right answer, but how is this different than above?
z = ifelse(x %% 2 == 0, x - 10, 5 + x)
sd(z)

#Which of the following options would return the third element of the list quiz_list
quiz_list = list(
  x = c(1, 2),
  y = "Hello Quiz Taker",
  z = "z"
)

## unselected - this returns a list
quiz_list[3]

##selected
quiz_list[[3]] - #returns a vector of size 1 with the value

## unselected, this is not logical
quiz_list["3"]

## unselected, this is not logical
##quiz_list$3

##this should be selected - but this does no
quiz_list$z

# Create a histogram of age in the Melanoma dataset 
# from the MASS package. 
# How would you describe this data?
library(MASS)
mel = MASS::Melanoma
View(mel)
str(mel)

##left scew
hist(mel$age, breaks = 50)

##right scew
hist(rchisq(1500, df = 5), breaks = 20)
