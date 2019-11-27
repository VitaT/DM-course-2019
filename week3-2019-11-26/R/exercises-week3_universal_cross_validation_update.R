#################################################
# load needed packages 
#install.packages("pacman")
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
library(readr)
#################################################

# KNN

#################################################

# first we should inspect data, but we already know it -- dt

## Here 

## Prepare  each data as below and use as dtb
## get the banana data
banana <- read_csv("banana.csv", col_types = cols(X1 = col_skip()))

dtb <-banana %>% dplyr::select(x1,x2,y ) %>% 
  dplyr::mutate(target=as.factor(y) ) %>%
  dplyr::select(x1,x2,target)

dim(dtb)

################################ Banana - 20 and 30 real features
################################ other features are gaussian white noise
banana_29_30_noise_othercols <- read_csv("banana_29_30_noise_othercols.csv")
dtb<-banana_29_30_noise_othercols %>%
  dplyr::mutate(target=as.factor(target) )

cvKNN <- function(dt, kcv = 5, kNN = 1) {
  # creating index to separate train and validation sets for k-fold validation 
  
  set.seed(1) 
  idx <- sample(rep_len(1:kcv, nrow(dt)))
  # loop to fit model for each fold
  res <- foreach (i = 1:kcv, .combine = rbind) %do% {
    # spliting datsets
    dTrain <- dt[idx != i, which(names(dt) != "target")]
    dTest <- dt[idx == i, which(names(dt) != "target")]
    vTrain <- dt[idx != i, which(names(dt) == "target"), drop = TRUE] 
    vTest <- dt[idx == i, which(names(dt) == "target"), drop = TRUE]
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


#################################################

# decision tree

#################################################

cvDT <- function(dt, kcv = 5) {
  
  # creating index to separate train and validation sets for k-fold validation 
  
  set.seed(1) 
  idx <- sample(rep_len(1:kcv, nrow(dt)))
  # loop to fit model for each fold
  res <- foreach (i = 1:kcv, .combine = rbind) %do% {
    # spliting datsets
    dTrain <- dt[idx != i, which(names(dt) != "target")]
    dTest <- dt[idx == i, which(names(dt) != "target")]
    vTrain <- dt[idx != i, which(names(dt) == "target"), drop = TRUE]
    vTest <- dt[idx == i, which(names(dt) == "target"), drop = TRUE]
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


#################################################

# naive bayes

#################################################

cvNBC <- function(dt, ModelFormula, kcv = 5) {
  # creating index to separate train and validation sets for k-fold validation
  set.seed(1) 
  idx <- sample(rep_len(1:kcv, nrow(dt)))
  # loop to fit model for each fold
  res <- foreach (i = 1:kcv, .combine = rbind) %do% {
    # spliting datsets
    dTrain <- dt[idx != i, ]
    dTest <- dt[idx == i, ]
    modelNBClass <- naiveBayes(ModelFormula, data = dTrain)
    predictionMBClass <- predict(modelNBClass, dTest)
    # it's also good to look at confusion table
    print(paste0("foldID ", i))
    print(caret::confusionMatrix(predictionMBClass, dTest$target))
    data.frame(
      foldID = i, 
      validationAccuracy = caret::confusionMatrix(
        predictionMBClass, dTest$target)$overall[1])
  }
  
  return(res)
  
}


# method 5-fold CV mean accuracy
#################################################

# LDA

#################################################         



# this is just a basic form of lda function

# modelLDA <- MASS::lda(ModelFormula, data = dTrain, prior = c(1,1,1)/3)  

# or prior can be estimated from training dataset



cvLDA <- function(dt, ModelFormula, kcv = 5) {
  # creating index to separate train and validation sets for k-fold validation 
  set.seed(1) 
  idx <- sample(rep_len(1:kcv, nrow(dt)))
  # loop to fit model for each fold
  res <- foreach (i = 1:kcv, .combine = rbind) %do% {
    # spliting datsets
    dTrain <- dt[idx != i, ]
    dTest <- dt[idx == i, ]
    modelLDA <- MASS::lda(ModelFormula, data = dTrain)
    predictionLDA <- predict(modelLDA, dTest)
    # it's also good to look at confusion table
    #print(paste0("foldID ", i))
    #print(caret::confusionMatrix(predictionMBClass, dTest$target))
    data.frame(
      foldID = i, 
      validationAccuracy = caret::confusionMatrix(
        predictionLDA$class, dTest$target)$overall[1])
  }
  return(res)
}

cvSVM <- function(dt, ModelFormula, kcv = 5, Kernel ="radial") {
  # creating index to separate train and validation sets for k-fold validation 
  set.seed(1) 
  idx <- sample(rep_len(1:kcv, nrow(dt)))
  # loop to fit model for each fold
  res <- foreach (i = 1:kcv, .combine = rbind) %do% {
    # spliting datsets
    dTrain <- dt[idx != i, ]
    dTest <- dt[idx == i, ]
    modelSVM <- svm(ModelFormula, data = dTrain, kernel = Kernel)
    predictionSVM <- predict(modelSVM, dTest)
    # it's also good to look at confusion table
    #print(paste0("foldID ", i))
    #print(caret::confusionMatrix(predictionMBClass, dTest$target))
    data.frame(
      foldID = i, 
      kernel=Kernel,
      validationAccuracy = caret::confusionMatrix(
        predictionSVM, dTest$target)$overall[1])
  }
  return(res)
}

################### loop to find best k-neighbor
resultCVKNN <- foreach(kNN = 1:10, .combine = rbind) %do% {
  print(kNN)
  cvKNN(dtb, kcv = 5, kNN = kNN)
}
################### Plot kNN accuracy result
resultCVKNN %>%
  group_by(kNN) %>% 
  summarise(meanAcc = mean(validationAccuracy)) %>%
  ggplot(., aes(as.factor(kNN), meanAcc, group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw()
# select k=9
################################################


################### loop to find best Kernel
K<-c("linear","radial","sigmoid","polynomial")
resultCVSVM <- foreach(i= 1:4 , .combine = rbind) %do% {
  print(K[i])
  cvSVM(dtb, ModelFormula = formula(target ~ .), kcv = 5, Kernel = K[i])
}
################### Plot kNN accuracy result
resultCVSVM %>%
  group_by(kernel) %>% 
  summarise(meanAcc = mean(validationAccuracy)) %>%
  ggplot(., aes(kernel, meanAcc, group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw()
# select k=9
################################################



resultCVKNN <- cvKNN(dtb, kcv = 5, kNN = 5)
resultCVDT <- cvDT(dtb, kcv = 5)
resultCVNBC <- cvNBC(dtb, ModelFormula = formula(target ~ .), kcv = 5)
resultCVLDA <- cvLDA(dtb, ModelFormula = formula(target ~ .), kcv = 5)
resultCVSVM <- cvSVM(dtb, ModelFormula = formula(target ~ .), kcv = 5, Kernel = "radial")

# method 5-fold CV mean accuracy

rbind(
  mutate(resultCVKNN, class = "KNN", kNN = NULL), 
  mutate(resultCVDT, class = "DT"), 
  mutate(resultCVLDA, class = "LDA"), 
  mutate(resultCVSVM, class = "SVM", kernel=NULL), 
  mutate(resultCVNBC, class = "NBC")) %>%
  group_by(class) %>% 
  summarise(meanValidationAccuracy = mean(validationAccuracy))