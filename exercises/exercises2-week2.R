library(datasets)
library(MASS)
library(e1071)
library(class)
library(C50)
library(dplyr)
library(ggplot2)

## take the iris data
data(iris)

## create segments for 5 -fold validation
ind<-sample(150)
f1_tr<-c(1:119)
f1_te<-c(120:150)

f2_tr<-c(31:150)
f2_te<-c(1:30)

f3_tr<-c(1:30,61:150)
f3_te<-c(31:60)

f4_tr<-c(1:60,91:150)
f4_te<-c(61:90)

f5_tr<-c(1:90,121:150)
f5_te<-c(91:120)

iris_train<-iris[ind[f1_tr],]
iris_test<-iris[ind[f1_te],]

# apply knn classifer
knn1_model<-knn(iris_train[,1:4],iris_test[,1:4],iris_train$Species,k=1)
# confusion table
table(knn1_model,iris_test$Species)
# true sample numbers = row.names(iris_test)

knn3_model<-knn(iris_train[,1:4],iris_test[,1:4],iris_train$Species,k=3)
# confusion table
table(knn3_model,iris_test$Species)


# different fold
#iris_train<-iris[ind[f5_tr],]
#iris_test<-iris[ind[f5_te],]

## Which samples classified how
#cbind(row.names(iris_test), paste(iris_test$Species), paste(knn1_model) )
## Visualize classifications
## scatter plot of points


##### Decision tree C50
##################################### C50 control parameters
#C5.0Control(subset = TRUE, bands = 0, winnow = FALSE,
#noGlobalPruning = FALSE, CF = 0.25, minCases = 2,
#fuzzyThreshold = FALSE, sample = 0, seed = sample.int(4096, size = 1) -
#  1L, earlyStopping = TRUE, label = "outcome")
##############################################################

c50_model<-C5.0(iris_train[,1:4],iris_train$Species)
## summary on training data
summary(c50_model)
plot(c50_model)
c50_model_predict<-predict(c50_model, iris_test[,1:4] )
## confusion table for C50 decision tree
table(c50_model_predict, iris_test$Species)

###################### Change parameters

c50_model_1<-C5.0(iris_train[,1:4],iris_train$Species, 
                  control=C5.0Control(winnow=TRUE, minCases=10))
## summary on training data
summary(c50_model_1)
plot(c50_model_1)
c50_model_predict_1<-predict(c50_model_1, iris_test[,1:4] )
## confusion table for C50 decision tree
table(c50_model_predict_1, iris_test$Species)

###################### Naive Bayes
nb_model<-naiveBayes(Species~.,data=iris_train)

## predict on the test data
nb_model_prediction_prob<-predict(nb_model,iris_test[,1:4],type="raw")
## Probabilities of the classes
head(nb_model_prediction_prob)

nb_model_prediction_class<-predict(nb_model,iris_test[,1:4],type="class")
table(nb_model_prediction_class, iris_test$Species)

################## Linear discriminant ####
## note- Lda is best suited for the 2 class problem 
## SVM is also 2 class problem

## Prepare data for Setosa versus Versicolor
i<-iris$Species!="virginica"
iris_setver <- iris[i,]  

## and Versicolor versus virginica
i<-iris$Species!="setosa"
iris_virver <- iris[i,]  

## Here we train on all data - exercise
## create training ant test set for 

## apply lda to iris_setver
lda_iris_setver<-lda(Species ~ ., data = iris_setver)
lda_iris_setver_prediction<-predict(lda_iris_setver, iris_setver)

## prediction has $class, $posterior, $projection LDA1
## confusion table for LDA
table(lda_iris_setver_prediction$class, iris_setver$Species)
## investigate drop levels, for empty level is included into result

########################### iris_vervis has very bad separation
## apply lda to iris_virver
lda_iris_virver<-lda(Species ~ ., data = iris_virver)
lda_iris_virver_prediction<-predict(lda_iris_virver, iris_virver)

## prediction has $class, $posterior, $projection LDA1
## confusion table for LDA
table(lda_iris_virver_prediction$class, iris_virver$Species)
## investigate drop levels, for empty level is included into result

## The same two classes investigate with svm, default is radial kernel
## virver do nor separate well
svm_iris_virver<-svm(Species ~ ., data = iris_virver)
svm_iris_virver_prediction<-predict(svm_iris_virver, iris_virver)
## accuracy of virver_prediction
table(svm_iris_virver_prediction, iris_virver$Species)

## change kernel
svm_iris_virver_1<-svm(Species ~ ., data = iris_virver, kernel="polynomial")
svm_iris_virver_prediction_1<-predict(svm_iris_virver_1, iris_virver)
## accuracy of virver_prediction
table(svm_iris_virver_prediction_1, iris_virver$Species)

### Try different data 
