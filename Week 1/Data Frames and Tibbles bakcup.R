## Data Frames and Tibbles

##load from package
install.packages("mosaicData")
Galton = mosaicData::Galton
Galton
View(Galton)
head(Galton, 10)
str(Galton)              #structure of datframe.  Factors are how R deals with categorical data, Vector can only take F and M values.  Characters become factors
head(Galton$sex, 10)
levels(Galton$sex)

##subset like a matrix
Galton[7,3]   #Vector of size 1
Galton[ ,2]   #Vector - extract column, vector
names(Galton) # get names of variables
Galton[1, ]   # Get back datarows of get rows of dataset

##subset like a list
Galton[5]     ##Dataframe - height of children, gives a dataframe, not in brackets
Galton[1:2]   ##Dataframe - first 2 columns, gives a dataframe
Galton$father ##Vector
Galton[2]     ##Dataframe
Galton["father"]  ## dataframe
Galton[["father"]]  ##vector returned

Galton[Galton$sex == "F",]$height ## heihts of femailes
head(subset(Galton, subset = height > 70, n = 10))

#tibble - prints same manner, subsets different
library(tibble)
Galton = as_tibble(Galton)
Galton
Galton[7,3]
Galton[5]
Galton[1:2]
Galton["height"] ## returns tibble
Galton[["height"]] ## returns tibble
Galton$height    ## returns vector

Galton[ ,5]      ## returns tibble
Galton[1, ]
Galton[1,5]      ## returns a tibble

Galton = as.data.frame(Galton)
Galton["height"] ## returns data frame
Galton$height    ## returns vector
Galton[ ,5]      ## returns vector
Galton[1,5]      ## returns vector

Galton[ Galton$father == 78.5, ]
