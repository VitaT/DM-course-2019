library(tidyverse)
library(ggplot2)
library(tree)
library(randomForest)
library(gbm)
library(gridExtra)

#### Reading data ####

in_data   = read_rds("data/insurance.train.rds")
predict = read_rds("data/insurance.prediction.rds") 
# This is the unknown data to submit predictions for!
cat(read_file("data/insurance.README.md"))


#### Splitting the train data into training and test set ####
set.seed(1)
trainIndex       = sample(1:nrow(in_data), round(0.7*nrow(in_data)))

train_df <- in_data[trainIndex, ]
test_df <- in_data[-trainIndex, ]
