# We will work on a small dataset on blood pressure
#
# Blood pressure (mm Hg)
# Age (years)
# Weight (grams)
#
# The systolic blood pressure is divided into 4 classes:
# Low: <90
# Ideal: 90-120
# Pre-high blood pressure: 120-140
# High bp: >140
#
#################################################
# load needed packages 
library(tidyverse)  
library(reshape2)

# load "blood_pressure.csv" from data directory
df <- read_csv(file = "../data/blood_pressure.csv")
# as it is written now, line 19 will produce error, that R cannot find blood_pressure.csv data. 
# adjust either filepath in read_csv function or your working directory, so your Rstudio can find the file
# you can check where is your working directory with
getwd()
# then either find relative path to blood_pressure.csv data and write it in read_csv funtion or change your working directory with setwd()
#################################################
# First let's familiarize ourselves with new data
# Q: check data structure, how many observations? any missing values?
# Q: get a summary of the data
# Q: Describe the response variable (blood pressure) and the explanatory variables (predictors). E.g. number of missing values, distribution of values, mean, sd, etc.
# HINT: plot densities or histograms
# Q: check how many observations (absolute/relative amount) fall under blood pressure risk (bp > 120) and high risk (bp > 140) groups. 

# Q: Which of the two predictors do you think has the strongest effect on bp?
# HINT: check correlations and scatterpots
# first you can check correlation matrix. Use na.omit() function to deal with na values. What can you say about relatioship of predictors with target value and between themselves?
# However, correlation can be misleading -- maybe we have non-linear relationship? So let's plot predictors with target variable
# we don't have many predictors, so we can use simple plot function for quick inspection:
plot(df)
# for more thorough visualization we can use ggplot (as it is more flexible)
df %>%
  mutate(ID = 1:nrow(df), weight = weight / 1000) %>%   # convert weight to kg for easier interpretation and create unique ID for each row
  melt(., id.vars = c("ID", "bp")) %>%   # converts table to "long" format. we need it for faceting
  ggplot(., aes(value, bp)) + 
  geom_point() +
  facet_wrap( ~ variable, scale = "free") +
  geom_smooth() +
  geom_hline(yintercept = 120, lty = "dashed", color = "#FFFF00") +    # pressure tresholds for fun
  geom_hline(yintercept = 140, lty = "dashed", color = "#FF0000") 

# Q: now with geom_smooth we fit a smooth spline to data. Let's try to fit linear regression. add argument 'method = "lm"' to geom_smooth() (do not use single quotes)
# Q: also plot weight and age scatterplot with ggplot and add the response
# to the plot (size? color?)

#### quick interlude about melt ####
# melt comes from reshape package and allows us an easy way to change table format from "wide" (many columns, few observations) to "long" (many rows, few columns)
# melt -> converts to "long" format
# dcast -> converts to "wide" format
# use this quick tutorial to get the gist of it https://seananderson.ca/2013/10/19/reshape/ (other resources also work :))
#### back to out analysis ####

# Q: Describe with words what happens with bp as function of age and weight

# for now let's clean up our data from NA values. 
# Q: Filter them out
# make a simple linear regression with age as explanatory variable
bp_model_age <- lm(bp ~ age, data = df)

# Q: Is there a significant linear relationship between age and bp? What is the strength  of it?
# HINT: check summary function output
# Q: Plot the data and the fitted line - does it look ok?
plot(df$age, df$bp)
abline(bp_model_age, col = 'red')

# Q: Plot the residuals - does it look ok? Can you explain what is on y and x axes?
plot(predict(bp_model_age), residuals(bp_model_age))
abline(h = 0, col = "red")
# residual plot is usefull for model diagnostics -- If the points in a residual plot are randomly dispersed around the horizontal axis, a linear regression model is appropriate for the data; otherwise, a non-linear model is more appropriate. 
# also simetimes a particular group of observations (acoording to some other feature) have very high residuals -- this indicates that you should look into that feature, why they are predicted worse, what can be done for that.
# check other diagnostics plots for linear model that are built in plot function for object with class "lm"
plot(bp_model_age)
plot(bp_model_age, which = 1)  # to select specific plot from all the available

# Q: Do the same for weight.

# Q: Do a full regression model without interaction.
bp_both <- lm(bp ~ age + weight, data = df)

# Q: How much of the variation is explained?
# Q: Do you see anything wrong with the model?

# Q: Do a full regression model with interaction.
bp_with_int <- lm(bp ~ age + weight + weight*age, data = df)
# Q: Does it explain more? (even a tiny bit more)
# Q: Does it explain significantly more?

# Q: What is the predicted bp of an typical 35 kg 12 year old boy and a 60 year old fat man (250 kg)?
predict(bp_both, 
        data.frame(age = c(12, 60), weight = c(35000, 250000)), 
        interval = "prediction")



#################################################
# we had 4 models. All of them were linear models -- and they were used for a long time and have multiple statistics that were created to evaluate models. For example adjusted R^2 statistics is adjusted for the number of predictors and we can use it to compare models with different number of predictos. (each predictor brings a bit (even a tiny bit) of more variance/info and increases how much of the variance is explained. Rsquared statistics always increases when more predictors are included -- can you explain why?) 

# but let's not get lazzy and also calculate MSE for our model.
trainMSE <- mean(bp_both$residuals^2)
# this is training MSE and that is not very informative about how our model will perform on unseen data. So we will randomly split our dataset to train and validation sets (70%-30%) 
# when working with functions that generate random numbers use function set.seed() to ensure reproducibility
set.seed(1)
trainSetSize <- round(nrow(df)*0.7)
trainID <- sample(nrow(df), size = trainSetSize)
trainSet <- df[trainID, ]
validateSet <- df[-trainID, ]
# fit model on training set
bp_both <- lm(bp ~ age + weight, data = trainSet)
# now we will want to calculate MSE a few times -- so it's usefull to just create a function that outputs MSE if we give it predicted and true values. Ask me if you are unfamiliar with functions
getMSE <- function(predValue, trueValue) {
  mean((predValue - trueValue)^2)
}
# so, out train MSE is:
getMSE(predict(bp_both), trainSet$bp)
# and MSE for validation set i.e, out test MSE estimate is 
getMSE(predict(bp_both, newdata = validateSet), validateSet$bp)
# Q: how much does our train and validation set MSE differ?

# now let's do k-fold cross validation 
# there are multiple ways how to do cross validation -- we will first try one example where we write it ourselves and then use caret package to do it
# we will need foreach and caret libraries.
# all libraries should be loaded in the begining of the script, but for learning consistency we will do it now i.e. load it when we actually need it. That's how it usually is at first -- in the process you understand that you need more libraries and add them in the begining
library(foreach)
library(caret)

# define how many folds to use.
nFolds <- 10
foldsID <- sample(rep(1:nFolds, length.out = nrow(df)))  # this fold ID vector and then we randomly mix it
cvResult <- foreach(k = 1:nFolds, .combine = rbind) %do% {   
  # foreach is an enhanced form of "for" cycle. Runs faster because it parellelizes the iterations and allows for some aditional functionality.
  testID <- which(foldsID == k)
  traindf <- df[-testID, ]
  validatedf <- df[testID, ]
  bp_bothCV <- lm(bp ~ age + weight, data = traindf)
  data.frame(
    fold = k,
    trainMSE = getMSE(predict(bp_bothCV), traindf$bp),
    testMSEest = getMSE(predict(bp_bothCV, newdata = validatedf), validatedf$bp))
}

# we can plot to see how our test MSE estimate differs between different folds
# Q: what do we see?
cvResult %>%
  melt(., id.vars = "fold") %>%
  ggplot(., aes(fold, value, color = variable)) +
  geom_point() +
  geom_line() +
  ylab("MSE")

# So out testMSE estimate after 10-fold cross validation is
mean(cvResult$testMSEest)
# Q: compare it with testMSE estimate from only 1 validation set. What can you say about our initial testMSE estimation?

# Now we wrap our for loop in a function for more convenient use. e.g. we want to use it multiple times on different linear models, try testMSE estimate changes when we use different fold number
performCV <- function(dataset, modelFormula, nFolds) {
  foldsID <- sample(rep(1:nFolds, length.out = nrow(dataset)))
  cvResult <- foreach(k = 1:nFolds, .combine = rbind) %do% {   
    testID <- which(foldsID == k)
    traindf <- dataset[-testID, ]
    validatedf <- dataset[testID, ]
    bp_bothCV <- lm(modelFormula, data = traindf)
    data.frame(
      fold = k,
      trainMSE = getMSE(predict(bp_bothCV), traindf$bp),
      testMSEest = getMSE(predict(bp_bothCV, newdata = validatedf), validatedf$bp))
  }
  return(mean(cvResult$testMSEest))   
  # for more complex functions it is good to explicitly define what object should be returned as function output
}
modelFormula <- formula(bp ~ age + weight)
# formula creates a class "formula" object that we can directly submit to lm and other functions 
res <- performCV(df, modelFormula, nFolds = 5)

# now let's see how changes test MSE estimate with different fold values
# we could again use foreach (try to write it yourself) but now I'll use a simple for loop. 
resultList <- list()  # initiale a data structure where to store the result. 
# foreach function is also usefull because it has ".combine = rbind" argument that saves us from hastle to explicitly create a data structure where loop will store each iteration results
cvRange <- c(2, 3, 5, 10, 20, nrow(df))   # define fold values for cv
for (i in cvRange) {
  resultList[[as.character(i)]] <- performCV(df, formula(bp ~ age + weight), nFolds = i)
  # store the results to named list
}
# convert named list to to data frame
differentCVresults <- data.frame(
  testMSEest = unlist(resultList),   # unlist first converts list to atomic vector. Get's complicated with nested lists
  cv = names(resultList)  # extract names of list elements
)
# plot the results. groups = 1 argument is needed for geom_line. Try this the plot without group argument. Try to find out what does group argument do.
ggplot(differentCVresults, aes(cv, testMSEest, group = 1)) +
  geom_point() +
  geom_line()
# cv is not in the order we want... because it sorts it  let's change it.
# if we will convert it to factor this will help to keep the order we want with levels argument
differentCVresults <- mutate(differentCVresults, 
                             cv = factor(cv, levels = cvRange))
ggplot(differentCVresults, aes(cv, testMSEest, group = 1)) +
  geom_point() +
  geom_line()
# Q: what can you say about how testMSE estimate changes compared with different cv fold? What is the range of variation? compare it to testMSE estimate from single split.


#########################
# now... we see that we can quite easily build cross validation ourselves and customise to our needs.
# of course cross validation is a common procedure and there are r packages who already have wrappers for all that we did here.
# one of those packages ir caret. Sometimes it's good to use it and it easily simplifies our code. However, sometimes it wraps too much and then we can go back to building custom functions.

# main function to fit model with caret package is train
# basic structure of train function:
model_caret <- train(bp ~ age + weight,                  # model formula to fit
                     data = df,                          # data
                     trControl = data_ctrl,              # our training strategy
                     method = "lm")                      # specifying model. See what other methods are available
# data_ctrl is an onject you create with caret trainControl function. It defines your training strategy. We have to define before training
data_ctrl <- trainControl(method = "cv", number = 10)
# Q: check what other methods are available

# fit lm with 10-fold cv
model_caret <- train(bp ~ age + weight,                  # model formula to fit
                     data = df,                          # data
                     trControl = data_ctrl,              # our training strategy
                     method = "lm")                      # specifying model. See what other methods are available. Check what R manual page has to say about this questions, check https://topepo.github.io/caret/available-models.html site.

# can see info about the model with 
model_caret 
summary(model_caret)
# also, note that model_caret is a essentially a list
typeof(model_caret)
# therefore we can subset it like a list
model_caret$finalModel
str(model_caret, max.level = 1)   # it is a nested list, so use max.level argument to restrict how many levels to show

# We can also examine model predictions for each fold.
model_caret$resample
# Q: what does RMSE stand for? 
# we can calculate it from out cross validation results:
differentCVresults[differentCVresults$cv == 10, 1] %>% sqrt
# Q: compare 10-fold cross validation results from our function and caret.
# Q: run train function one more time and again compare RMSE. 
# Q: Do you have any idea why does it change?
# Q: read here on how can we control that https://topepo.github.io/caret/model-training-and-tuning.html#repro

# caret package also has a usefull function to easily create data partitions
set.seed(3456)
trainIndex <- createDataPartition(df$bp, p = .7, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)
nrow(trainIndex)
# more about it in https://topepo.github.io/caret/data-splitting.html


#################################################
#### Knn prediction ####

# However let's say we have another model -- for example K-nearest neighbors (KNN).
# The k-Nearest-Neighbors (kNN) method is classically used for clasification, but can also be used for regression. 
# It is one of the simplest methods in machine learning, and is a great way to introduce yourself to machine learning and classification in general.
# The gist of the kNN method is, for each classification query, to:
# 1. Compute a distance value between the item to be classified and every item in the training data-set. 
# 2. Pick the k closest data points (the items with the k lowest distances)
# 3. Conduct a "majority vote" among those data points — the dominating classification in that pool is decided as the final classification (in case of regression -- the only difference is that we take not the "majority vote", but mean)
# k is the hyperparameter of KNN method that regulates for flexible our model is.
# if we take only one nearest neighbour, then our classification (regression) is very local -- we model each point closely and therefore our f^ is wiggly. The more neighbours we include, the more we generalize -- our variance decreases (try to relate that to central limit theory for the mean).
# so for knn model we have one hyperparameter that we have to chose. Let's use cross validation to choose that up.

library(FNN)  # library to use KNN with regression cases

# because knn is based on euclidian distance -- our variable value scale is very inportant. 
# the larger the value, the more influence it will have on distance. 
# for example:
# Q: Calculate the euclidean distance from the data points to some specific person (age 25, weight = 85) and see distance correlation to other predictors. which predictor has the most influence?
df %>%  
  mutate(e.dist = sqrt((weight - 85)^2 + (age - 25)^2)) %>%
  arrange(e.dist) %>%
  cor()
# Q: now calculate the distance when weight is in kg, not g. Is there a difference?
df %>%  
  mutate(e.dist = sqrt(((weight / 1000) - 85)^2 + (age - 25)^2)) %>%
  arrange(e.dist) %>%
  cor()

# so what can we do about different value scales of predictors?
# right -- normalize. 
# as always, there are several ways to do that, but 2 most popular are normalization by range i.e force all values to be between 0 and 1 and normalization by mean and sd, so that data mean becomes o mean = 0 and sd = 1

# Q: normalization by mean and sd can be conveniently done with scale function. Scale df and check if all mean column means are 0 and sd is 1.
# means for all columns can be conveniently calculated with base R colMeans function (check it's help page for other related convenient functions). Unfortanately base R does not have colSds... however matrixStats package has such function :) 
# when we need a single function from specific package we don't need to load full package. matrixStats::colSds() notation allows to find and use some specific function from package without loading whole package

# we'll do normalization by range by ourselves. 
# Q: do you know how normalization by mean and sd is done? could you do it manually?
rangeW <- range(df$weight)
rangeA <- range(df$age)
dfNormRange <- df %>%  
  mutate(weight = (weight - rangeW[1]) / (rangeW[2] - rangeW[1]), 
         age = (age - rangeA[1]) / (rangeA[2] -  rangeA[1])) 
summary(dfNormRange)  # checking if we succeeded

# let's use range normalized data to fit knn
# split to train and validation set using trainID we defined some time ago
# we also separate them to predictor table and target value vector
targetcolID <- which(names(dfNormRange) == "bp")
dTrainKNN <- dfNormRange[trainID, -targetcolID]
dTestKNN <- dfNormRange[-trainID, -targetcolID]
bpTest <- dfNormRange[-trainID, targetcolID, drop = TRUE]
bpTrain <- dfNormRange[trainID, targetcolID, drop = TRUE]

knnModel <- knn.reg(train = dTrainKNN, 
                    test = dTestKNN,
                    y = bpTrain, 
                    k = 1)
print(knnModel)
# let's say we want more info and try summary function on knnModel
summary(knnModel)
# we got a results different then what we are used with lm. But we see that some rownames are called residuals, call and possibly others that are interesting to us. So we can check what class and type of object it is
class(knnModel)
typeof(knnModel)   # so, essentially a list ! we can subset it as a list
str(knnModel)

# Q: let's find MSE for out model using out function getMSE and compare it to cross-validated linear model MSE. How is it different? Is this difference something we should have anticipated (note what k value did we use !)? Could it be better?

# Q: select a cross-validation strategy and use foreach loop with different k values to select optimal k value.


###########################
# try to apply KNN with our blood presure data using caret package. 
# as an example you can look through this tutorial https://quantdev.ssri.psu.edu/sites/qdev/files/kNN_tutorial.html

