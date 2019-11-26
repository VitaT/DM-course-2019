# week2 R exercises
library(data.table)
library(dplyr)
library(ggplot2)
library(ISLR)
library(MASS)

library(C50)   # for simple tree (clasification, regression)
library(randomForest)   # for randomForest and bagging
library(gbm)   # for boosting


## data
data("Carseats")
# Q: inspect what data you have
summary(Carseats)
dim(Carseats)

#################################################
# simple tree
#################################################    

## WE want to convert  continiuos variable Sales as a binary one: High Sales (yes/no)
High <- ifelse(Carseats$Sales <= 8, "No", "Yes")
Carseats <- Carseats[, -which(colnames(Carseats) == "Sales")]
Carseats <- data.frame(Carseats, High)

## fitting a tree model
tree.fit <- C5.0(High ~ ., data = Carseats)   

summary(tree.fit)
plot(tree.fit)

## Now we vant some more reliable statistics about model performance
## spliting data to train-validate
set.seed(2)
trainID <- sample(1:nrow(Carseats), round(nrow(Carseats)*0.6, digits = 0))  
tree.fit <- C5.0(High ~ ., data = Carseats, subset = trainID)   # some functions allow to give us to give only training ID vector and full data -- the subseting to create training dataset happens inside function

## model results
plot(tree.fit)
summary(tree.fit)

# prediction
predClass <- predict(tree.fit, Carseats[-trainID, ], type = "class")
res <- table(predClass, Carseats[-trainID, ]$High)  
# Q: how many FALSE positive and FALSE negative predictions do you have?

# custom way to check classification accuracy. Could do the same with caret::confusionMatrix
accuracyFullTree <- sum(diag(res)) / sum(res)   



## we created a full tree model -- maybe it's too large and we can select better level of complexity
## cost-complexity prunning with cross validation

# some packages include functions to run cross validation. Let's try that. (we can always build it ourselves, but why increase your work)
# NOTE: custom cross-validation functions are usefull when you want to understand what is happening e.g. you have to explain for your supervisor what you are doing in detail. Sometimes it's easier to debug 
set.seed(3)
cv.tree.fit <- cv.tree(tree.fit, FUN = prune.misclass)
# FUN=cv.tree means deviance determines how to prune (default)
# FUN=prune.misclass means we want the classification error rate to guide the cross-validation and pruning process
# So we care more about accurate prediction than node purity. It's a choise depending on a goal
# Q: what is the default value for the minimum number of observations to include in either child node?
# Q: what other parameters could be used to stop tree growth? check out tree.control documentation
# Q; what type of k-fold validation are we performing i.e. what is the k?


str(cv.tree.fit)
# dev corresponds to the cross-validation error rate in this instance.
# size -- the size of different trees consideres, corresponds to model complexity
# some diagnostics plots
plot(cv.tree.fit$size, cv.tree.fit$dev, type = "b")
plot(cv.tree.fit)   
# Q: what size tree would be the best to prune trees?


## pruning, let's do it with parameter best = 6.
prune.fit <- prune.misclass(tree.fit, best = 6)
# Q: what is the alternative, more generic function that we could use instead of prune.misclass? Check dokumentation to find that out

# now out tree looks like this
summary(prune.fit)
plot(prune.fit)
text(prune.fit, pretty = 0, cex = 0.75)

# let's find accuracy of pruned tree
pred <- predict(prune.fit, Carseats[ -trainList, ], type = "class")
res <- table(pred, High[ -trainList])
accuracyPrunedTree <- sum(diag(res)) / sum(res)   
# Q:did pruning help? Why/Why not?

# Q: try to plot how validation accuracy (i.e. test accuracy estimate) changes using different tree pruning size.
# HINT: create a plot where x axis is size parameter used for pruning and y is the validation accuracy

# Q: fit a regression tree using Boston data. 
# target column is medv (median value of owner-occupied homes in \$1000s)
# use all other columns as predictors
# check ?Boston for more info
data("Boston")
dim(Boston)
summary(Boston)

#################################################
# bagging
#################################################    
# bagging and random forest are very similar. We can perform bagging with random forest function if we set mtry to all possible predictors
trainID <- sample(1:nrow(Boston), round(nrow(Boston) / 2, digits = 0))
trueMedv <- Boston$medv
## bagging
bag.B <- randomForest(medv ~ ., data = Boston, subset = trainID, mtry = 13, importance = TRUE)
# Q: what does importance = TRUE do?
importance(bag.B)  # check saved values for predictors
varImpPlot(bag.B)  # visualize them
# Q: what is the MSE for this model? what about % of variance explained? From where do we get these values? are they the test or train MSE? again check bag.B model

# let's try to calculate test error estimate from held out dataset
pred.bag <- predict(bag.B, Boston[-trainID, ])
mean((pred.bag - trueMedv[-trainID]) ^ 2)
# Q: is it higher/lower then MSE reported in model? why?
# but how do the predictions acctually look?
plot(pred.bag, trueMedv[-trainID])  
abline(0, 1)
# Q: what can we say from this plot?


# Let's try to see how number of generated trees influences MSE. change ntree argument to 12, 50, 100, 500, 1000
# just rerun those tree lines with different ntree values. OR wrap it in a function and put it in a cycle and create a nice plot where x is the number of trees fitted and  
bag.B <- randomForest(medv ~ ., data = Boston, subset = trainID, mtry = 13, ntree = 500)
mean((pred.bag - trueMedv[-trainID]) ^ 2)
# Q: do you see any tendency? how does MSE change when we increase ntree used?
# check how out-of-bag error changes with number of trees
plot(bag.B)

#################################################
# random forest
#################################################  
# basicly the same as bagging just using not all of the predictors, but a subset of them
# Let's try mtry = 4.  typically we choose m ~ sqrt(p)— that is, the number of predictors considered at each split is approximately equal to the square root of the total number of predictors
# This is not a set rule and we should also try different mtry parameters to find the optimal model
bag.B <- randomForest(medv ~ ., data = Boston, subset = trainID, mtry = 4, importance = TRUE)

pred.bag <- predict(bag.B, Boston[-trainID, ])
mean((pred.bag - trueMedv[-trainID]) ^ 2)
plot(pred.bag, trueMedv[-trainID])
abline(0, 1)

# again we can investigate which features have most influence on the target vector y
importance(bag.B)
varImpPlot(bag.B)


#################################################
# boosting
################################################# 
# just to see that it exist. Because it get's more complicated than random forest and maybe we will talk about it next time
set.seed(1)
# let's fit boosting model
boost.B <- gbm(medv ~ ., data = Boston[ -trainID, ], distribution = "gaussian",
               n.trees = 5000, interaction.depth = 4)
# Q: explain what each parameter does

# Inspecat the model.
# Q: which predictor has the highest relative influence? the least? 
summary(boost.B)

# predict values and estimate validation error
pred.boost <- predict(boost.B, Boston[-trainID, ], n.trees = 5000)
mean((pred.boost - trueMedv[-trainID]) ^ 2)

# You can always find more information about function technical details and possibly the theory behind then using vignettes from package page on CRAN https://cran.r-project.org/web/packages/gbm/. 
# Or with command utils::browseVignettes("gbm") that evokes the same document 
# 