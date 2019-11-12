###############################################################
# R data structures
###############################################################
# Q: What are the four common types of atomic vectors? What are the two rare types?

## let's create some objects (for now only vectors) in R
vNum <- 1:9
vNum <- c(1:9, 5)
vNumIn <- c(1L, 2L, 1e03L, 0xcafeL)
vNumDb <- c(0, 1, 14, -14, 0.145, 1e03, 0xcafe, Inf, -Inf, NaN)
vWord <- c("hello", "world", "Let's", "review", "R", ' now', '*', "1")
vChar <- c("M", "F", "F", "M", "M")
vBoo <- c(TRUE, FALSE, T, F)
vBoo <- vNumIn >= 2

# Every vector can also have attributes, which you can think of as a named list of arbitrary metadata. Two attributes are particularly important. The dimension attribute turns vectors into matrices and arrays and the class attribute powers the S3 object system.

## Q: check what type of atomic vector each object is, what is their class attribute? what is their length?
# HINT: use typeof, class functions. What is the difference between them?
# HINT: you can find vector length with length function.

# we can convert data from one type to another:
## Q: try and see what happens if you convert bolean or numeric values to character
# HINT: use as.character

## Q: character or bolean values to numeric
# HINT: use as.numeric

## Q: character or numeric values to boolean
# HINT: use as.logical

## Q: convert "vNumDb" object to integer type and check if the conversion was successfull
# HINT: use as.integer


# we can also define a class for an atomic vector 
## Q: convert "vChar" object to factor class
# HINT: use as.factor or factor

# Q: check "vChar object type, class and attributes
# HINT: use attributes function to find out the attributes

# most of data structures in R are based on vectors. 
# there are a lot of different R data classes and each new package can have it's own specific classes.
# However, base R has 5 main data structure classes -- let's create each of them. 
# We already have atomic vectors, so let's create other homogenous data objects
dX <- matrix(1:100, ncol = 20)
dA <- array(c('green', 'yellow'), dim = c(3, 3, 2))
dA <- array(c(5,9,3,10,11,12,13,14,15), dim = c(3, 3, 2))
dA <- array(c(5,9,3,10,11,12,13,14,15), dim = c(3, 2, 3)) # arrays are very specific and reraly used, so we will not go in depth about them.

# now let's create heterogenous data objects:
listA <- list(int = vNum, word = vWord, log = vBoo, test = "a")
df <- data.frame(
  int = vNum,
  db = vNum2,
  word = vWord, 
  fact = rep(vFact, 2),
  log = vNum2 > 1)

# Q: check the classes of created objects

# those are the main base R data structures and we use them a lot (except array...)
# however other packages can have their own specific object classes (we will see examples later)
# it is always usefull to ckeck what type of object are we dealing with because most functions work with only some specific data structure and you will get errors if you provide incorrect data structure. 
# other functions can work on different types -- they recognise them internally and select an appropriate method. However the output can be unexpected if you do not follow with what type of structure you are working with


###############################################################
# data subseting
###############################################################
## MAIN IDEAS
# * There are six ways to subset atomic vectors.
# 1) positive integers 
# 2) negative integers
# 3) logical vectors
# 4) nothing
# 5) zero 
# 6) character vector (if your vector is named!) 
# ways 4-6 are not very common.
# * There are three subsetting operators, [, [[, and $.
# * Subsetting operators interact differently with different vector types (e.g., atomic vectors, lists, factors, matrices, and data frames)
# * Subsetting can be combined with assignment.

####################### ATOMIC VECTORS ######################
# e.g. atomic vector subseting
vNum[c(1, 4)]
vNum[-c(1, 4)]
vNum[vNum < 0]
vNum[]
vNum[0]

# Quiz:
# Q1) What is the result of subsetting a vector with positive integers, negative integers, a logical vector, or a character vector? what about empty brackest [] i.e. nothing or zerp value [0]?

# Q2) What operators (and functions) can we use to genrate logical or numeric vector to select only specific values from a vector?
# HINT: ==, >, %in%, grep, which (allows you to convert a Boolean representation to an integer representation)

# Q3) how to select only NA values? How to select only finite/infinite values? How to exclude those values?

####################### LISTS ######################
# Subsetting of a list works in the same way as subsetting an atomic vector. However here we have more operators. In addition with [ we can use [[ and $.  
listA[1]
listA[[1]]
listA$int
# Q4) What’s the difference between [, [[, and $ when applied to a list?
# Q5) What’s the difference between [, [[, and $ when applied to a data.frame? Compare the results from data.frame and list -- what does it suggest?

# Q6) how to select a value from a nested list? e.g. extract "3rd level" value from this nested list:
listNested <- list(1:10, 
                   "a", 
                   15:4, 
                   list("nested One", seq(1, 10, by = 0.2), TRUE), 
                   list(c("nested", "even", "more"), 1:10, list("3rd level", "number", 3)))
# HINT: for more complex structures like lists, data.frames, matrices -- it is usefull to use structure i.e. str() funtion to see compact structure representation and then stepwise select the elements leading to the one you need
# Q7)  Given a linear model, e.g., mod <- lm(mpg ~ wt, data = mtcars), extract the residual degrees of freedom. Then extract the R squared from the model summary (summary(mod))
# HINT: use str() or class() to figure out what is the structure of mod and summary(mod) 

####################### 2D structures ######################
# data.frames and matrices have 2 dimensions -- rows and columns. Therefore when subseting them we have to define numbers for rows and columns separately.
df[1, 4]   # reports value in first row and 4th column cell.
df[1:5, c(1, 5)]   # can select several rows or columns and this will result in a subset dataset (smaller then original)
df[df$int > 5, c(1, 5)]   # can also use logical vectors to subset the same as in atomic vectors
df[, 1:3]   # if you omit values for rows (or columns) -- all rows (columns) will be printed.
df[35]   # if no comma is specified R automatically assumes that the vector/number is for columns in  data.frame
dX[35]   # a matrix is essentially one long vector -- so with no comma R returns 35 position value
dX[, 35]   # no such column 
# we can check info about the data.frame using functions: dim, ncol, nrow, str
df[, 5]   # when you want to select only one column from a data.frame or matrix and still retain data.frame/matrix form instead of an atomic vector use argument drop = FALSE e.g.
df[, 5, drop = FALSE] 
dX[, 5, drop = FALSE] 



# Q: Fix each of the following common data frame subsetting errors:
mtcars[mtcars$cyl = 4, ]
mtcars[-1:4, ]
mtcars[mtcars$cyl <= 5]
mtcars[mtcars$cyl == 4 | 6, ]
# Q: Why does mtcars[1:20] return an error? How does it differ from the similar mtcars[1:20, ]?
# Q: What does upper.tri() return? How does subsetting a matrix with it work? Do we need any additional subsetting rules to describe its behaviour?
x <- outer(1:5, 1:5, FUN = "*")
x[upper.tri(x)]
# Q: Brainstorm as many ways as possible to extract the third value from the cyl variable in the mtcars dataset.
# Q: How would you randomly permute the columns of a data frame? Can you simultaneously permute the rows and columns in one step?
# HINT: use sample() function to randomly select the columns (column number can be defined with ncol(). Use ?sample for more information about the function
sample(ncol(df))
# Q: How would you select a random sample of m = 6 rows from a mtcars data frame? 
# What if the sample had to be contiguous (i.e., with an initial row, a final row, and every row in between)?
# HINT: for second question -- we need to randomly select only the start position and then create a contiguous interval till the end.
m <- 6

# Q: How could you put the columns in a data frame df in alphabetical order?
# HINT: use order() function
order(names(df))



####################### subseting and asignments ######################
# All subsetting operators can be combined with assignment to modify selected values of an input vector: this is called subassignment. The basic form is x[i] <- value

# e.g. 
vNum[1] <- 115   # change first element value
vNum[is.na(vNum)] <- 115   # change values selected with logic vector 
vNum[length(vNum):(length(vNum) + 2)] <- seq(100, 300, length.out =  3)   # create new 3 values at the end of the vector
vNum["a"] <- 14  # create a named value


# this also works for data.frames and lists
df["newCol"] <- 1:nrow(df)
listA$NewElement <- "test"  # add first level element
listA$NewElement[[2]] <- "c"   # add a specific value to already existing element
listA$NewElement[["b"]] <- "b"
listA$NewElement <- NULL   # delete the element



# Q1: Given the following data frame, how do I create a new column called “3” that contains the sum of 1 and 2? You may only use $, not [[. What makes 1, 2, and 3 challenging as variable names?
dTest <- data.frame(runif(3), runif(3))
names(dTest) <- c(1, 2)
# Q2: What does df[is.na(df)] <- 0 do? How does it work?
# Q3: If x is a matrix, what does x[] <- 0 do? How is it different from x <- 0?
x <- matrix(1:10, nrow = 2)
# Q4: rename df fact column by a given lookup vector 
lookup <- c(m = "Male", f = "Female", u = NA)
# an example:
x <- c("m", "f", "u", "f", "f", "m", "m")
lookup[x]
unname(lookup[x])  # to remove the names of vector values

