library(datasets)
library(MASS)
# MASS has lda and qda
library(e1071)
# e1071 has NaiveBayes
library(class)
# class has knn
library(dplyr)
library(ggplot2)
# to see the contents
# ls(package:class)
data(iris)
# Visualize 
d1<-iris %>% select(contains("Sepal"),Species)
ggplot(d1,aes(x=Sepal.Length,
              y=Sepal.Width, 
              color=Species))+geom_point()
