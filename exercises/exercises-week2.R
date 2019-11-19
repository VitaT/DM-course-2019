library(datasets)
library(MASS)
# MASS has lda and qda
library(e1071)
# e1071 has NaiveBayes
library(class)
# class has knn
library(dplyr)
library(ggplot2)
library(C50)

# to see the contents
# ls(package:class)
data(iris)
# Visualize 
d1<-iris %>% select(contains("Sepal"),Species)
ggplot(iris_test,aes(x=Sepal.Length,
              y=Sepal.Width, 
              color=Species))+geom_point()
table(iris$Species[itrain])
train<-iris_train %>% select(-Species)
dim(train)
trainclass<-iris_train %>% select(Species)
dim(trainclass)
test<-iris_test %>% select(-Species)
dim(test)

testprediction<-knn(as.data.frame(train), 
    as.data.frame(test), 
    trainclass$Species, k=5)

# confusion table
table(testprediction,iris_test$Species)

## decision tree
library(C50)
C5.0(as.data.frame(train),
     as.factor(trainclass$Species) )

plot(C5.0(as.data.frame(train),
          as.factor(trainclass$Species) ) )







### from KNN stocks train
prediction <- knn(predictors[stocksTrain, ], predictors[!stocksTrain, ], stocks$Increase[stocksTrain], k = 1)
table(prediction, stocks$Increase[!stocksTrain])
i<-sample(150)
itrain=i[1:74]
itest=i[75:150]
iris_train<-iris[itrain,]
iris_test<-iris[itest,]

