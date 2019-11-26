#################################################
# load needed packages 
install.packages("pacman")
library(pacman)
p_load("dplyr", "ggplot2","foreach", "ggplot2", "datasets", "MASS", 
       "e1071", "class", "C50", "caret")  # should install and load all the needed packages

library(dplyr)
library(ggplot2)
library(foreach)
library(datasets)
library(MASS)    # MASS has lda and qda
library(e1071)   # e1071 has NaiveBayes
library(class)   # class has knn
library(C50)     # decision trees
library(foreach)
library(caret)
#################################################
# KNN
#################################################
# first we should inspect data, but we already know it -- iris
## Here 
## We have to modify script to work with our own data set
data(iris)
dim(iris)
    k <- 5
cvKNN <- function(iris, kcv = 5, kNN = 1) {
    # creating index to separate train and validation sets for k-fold validation 
    set.seed(1) 
    idx <- sample(rep_len(1:kcv, nrow(iris)))
    # loop to fit model for each fold
    res <- foreach (i = 1:kcv, .combine = rbind) %do% {
        # spliting datsets
        dTrain <- iris[idx != i, which(names(iris) != "Species")]
        dTest <- iris[idx == i, which(names(iris) != "Species")]
        vTrain <- iris[idx != i, which(names(iris) == "Species")]
        vTest <- iris[idx == i, which(names(iris) == "Species")]
        predictionKNN <- knn(train = dTrain, test = dTest, cl = vTrain, k = kNN)
        # it's also good to look at confusion table
        # print(caret::confusionMatrix(predictionKNN, vTest)$overall[1])
        data.frame(
            foldID = i, 
            kNN = kNN, 
            validationAccuracy = caret::confusionMatrix(predictionKNN, vTest)$overall[1])
    }
    return(res)
}
# loop to find best k-neighbor
resultCVKNN <- foreach(kNN = 1:10, .combine = rbind) %do% {
    print(kNN)
    cvKNN(iris, kcv = 5, kNN = kNN)
}
resultCVKNN %>%
    group_by(kNN) %>% 
    summarise(meanAcc = mean(validationAccuracy)) %>%
    ggplot(., aes(as.factor(kNN), meanAcc, group = 1)) +
        geom_point() +
        geom_line() +
        theme_bw()
# select k=9
resultCVKNN <- cvKNN(iris, kcv = 5, kNN = 9)


#################################################
# decision tree
#################################################
cvDT <- function(iris, kcv = 5) {
    # creating index to separate train and validation sets for k-fold validation 
    set.seed(1) 
    idx <- sample(rep_len(1:kcv, nrow(iris)))
    # loop to fit model for each fold
    res <- foreach (i = 1:kcv, .combine = rbind) %do% {
        # spliting datsets
        dTrain <- iris[idx != i, which(names(iris) != "Species")]
        dTest <- iris[idx == i, which(names(iris) != "Species")]
        vTrain <- iris[idx != i, which(names(iris) == "Species")]
        vTest <- iris[idx == i, which(names(iris) == "Species")]
        modelDT <- C5.0(x = dTrain, y = vTrain)
        predictionDT <- predict(modelDT, dTest)
        # it's also good to look at confusion table
        print(paste0("foldID ", i))
        print(caret::confusionMatrix(predictionDT, vTest))
        data.frame(
            foldID = i, 
            validationAccuracy = caret::confusionMatrix(predictionDT, vTest)$overall[1])
    }
    return(res)
}
resultCVDT <- cvDT(iris, kcv = 5)


#################################################
# naive bayes
#################################################
cvNBC <- function(iris, ModelFormula, kcv = 5) {
    # creating index to separate train and validation sets for k-fold validation 
    set.seed(1) 
    idx <- sample(rep_len(1:kcv, nrow(iris)))
    # loop to fit model for each fold
    res <- foreach (i = 1:kcv, .combine = rbind) %do% {
        # spliting datsets
        dTrain <- iris[idx != i, ]
        dTest <- iris[idx == i, ]
        modelNBClass <- naiveBayes(ModelFormula, data = dTrain)
        predictionMBClass <- predict(modelNBClass, dTest)
        # it's also good to look at confusion table
        print(paste0("foldID ", i))
        print(caret::confusionMatrix(predictionMBClass, dTest$Species))
        data.frame(
            foldID = i, 
            validationAccuracy = caret::confusionMatrix(
                    predictionMBClass, dTest$Species)$overall[1])
    }
    return(res)
}
resultCVNBC <- cvNBC(iris, ModelFormula = formula(Species ~ .), kcv = 5)

# method 5-fold CV mean accuracy
rbind(
    mutate(resultCVKNN, class = "KNN", kNN = NULL), 
    mutate(resultCVDT, class = "DT"), 
    mutate(resultCVNBC, class = "NBC")) %>%
    group_by(class) %>% 
    summarise(meanValidationAccuracy = mean(validationAccuracy))


#################################################
# LDA
#################################################         

# this is just a basic form of lda function
# modelLDA <- MASS::lda(ModelFormula, data = dTrain, prior = c(1,1,1)/3)  
# or prior can be estimated from training dataset

cvLDA <- function(iris, ModelFormula, kcv = 5) {
    # creating index to separate train and validation sets for k-fold validation 
    set.seed(1) 
    idx <- sample(rep_len(1:kcv, nrow(iris)))
    # loop to fit model for each fold
    res <- foreach (i = 1:kcv, .combine = rbind) %do% {
        # spliting datsets
        dTrain <- iris[idx != i, ]
        dTest <- iris[idx == i, ]
        modelLDA <- MASS::lda(ModelFormula, data = dTrain)  
        predictionLDA <- predict(modelLDA, dTest)
        # it's also good to look at confusion table
        #print(paste0("foldID ", i))
        #print(caret::confusionMatrix(predictionMBClass, dTest$Species))
        data.frame(
            foldID = i, 
            validationAccuracy = caret::confusionMatrix(
                    predictionLDA$class, dTest$Species)$overall[1])
    }
    return(res)
}
resultCVLDA <- cvLDA(iris, ModelFormula = formula(Species ~ .), kcv = 5)

# method 5-fold CV mean accuracy
rbind(
    mutate(resultCVKNN, class = "KNN", kNN = NULL), 
    mutate(resultCVDT, class = "DT"), 
    mutate(resultCVLDA, class = "LDA"), 
    mutate(resultCVNBC, class = "NBC")) %>%
    group_by(class) %>% 
    summarise(meanValidationAccuracy = mean(validationAccuracy))
