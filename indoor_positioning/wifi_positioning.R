# load libraries ####
# set working directory
wd <- file.path('~',
                'indoor_positioning')
setwd(dir= wd)

# load libraries
library(readr)
library(stringr)
library(ggplot2)
library(dplyr)
library(magrittr)
library(corrplot)
library(caret)
library(ggpubr)
library(doMC)
library(kknn)
library(tictoc)

# parallelization
registerDoMC(cores = 4)

# read dataset ####
input_file <-
    file.path('.',
              'UJIndoorLoc',
              'trainingData.csv')

if( !file.exists(input_file) ) {
  print('File:')
  print(input_file)
  print('not found!. Current dir:')
  getwd()
}
wifi_data <- read.csv(input_file, sep = ',', dec = '.')

# check feature data types ####
# RSSI levels : int [-104, 0], +100
# LON, LAT : num
# FLOOR : int [0, 1, 2, 3, 4]
# BuildingID : int [0, 1, 2]
# SpaceID : int -> not in the validation set
# Rel. pos. : int [1, 2] -> not in the validation set
# UserID : int [1, 18] -> not in the validation set
# PhoneID : int
# POSIX timestamp : int
str(wifi_data, list.len=ncol(wifi_data))

# new shorter feature names ####
newcolnames <- c('LON', 'LAT', 'FLOOR', 'BUILD',
                 'SPACE', 'RELPOS', 'USER_ID',
                 'PHONE_ID', 'TIME')
colnames(wifi_data)[521:529] <- newcolnames

# data visualization ####
# distribution of longitude and latitude
floor_map <- ggplot(data = wifi_data, aes(x = LON, y = LAT)) +
  geom_point(color= "black") + theme_bw()
floor_map + geom_point(data = wifi_data[wifi_data$FLOOR==4, ],
                       aes(x = LON, y = LAT), color = "red")
floor_map + geom_point(data = wifi_data[wifi_data$FLOOR==3, ],
                       aes(x = LON, y = LAT), color = "red")
floor_map + geom_point(data = wifi_data[wifi_data$FLOOR==2, ],
                       aes(x = LON, y = LAT), color = "red")
floor_map + geom_point(data = wifi_data[wifi_data$FLOOR==1, ],
                       aes(x = LON, y = LAT), color = "red")
floor_map + geom_point(data = wifi_data[wifi_data$FLOOR==0, ],
                       aes(x = LON, y = LAT), color = "red")

# convert to categorical variables (factors) ####
wifi_data$BUILD <- as.factor(wifi_data$BUILD)
wifi_data$SPACE <- as.factor(wifi_data$SPACE)
wifi_data$RELPOS <- as.factor(wifi_data$RELPOS)
wifi_data$USER_ID <- as.factor(wifi_data$USER_ID)
wifi_data$PHONE_ID <- as.factor(wifi_data$PHONE_ID)
wifi_data$FLOOR <- as.factor(wifi_data$FLOOR)

# change level names, convert floor to factor ####
revalue(wifi_data$BUILD,
        c("0"="B0", "1"="B1", "2"="B2")) -> wifi_data$BUILD
revalue(wifi_data$FLOOR,
        c("0"="F0", "1"="F1",
          "2"="F2", "3"="F3", "4"="F4")) -> wifi_data$FLOOR

# find and remove duplicate rows ####
wifi_data %>% unique(.) %>% nrow(.)  # 637 duplicate rows
wifi_data <- unique(wifi_data)
# rescale values: +100 converted to -105
wifi_data[wifi_data==100] = -105
# drop user id and time
rm_features <- subset(wifi_data, select = c(USER_ID, TIME))
wifi_data %>% subset(., select = -c(USER_ID, TIME)) -> wifi_data

# sample dataset: use 10% of observations ####
# make 4 different train sets to train 4 models
# building, floor, lon, lat
set.seed(1987)
dataset <- wifi_data
sample_indices_1 <- createDataPartition(y = dataset$BUILD,
                                        p = 1/20,
                                        list = F)
dataset <- dataset[-sample_indices_1, ]
sample_indices_2 <- createDataPartition(y = dataset$BUILD,
                                        p = 1/19,
                                        list = F)
dataset <- dataset[-sample_indices_2, ]
sample_indices_3 <- createDataPartition(y = dataset$BUILD,
                                        p = 1/18,
                                        list = F)
dataset <- dataset[-sample_indices_3, ]
sample_indices_4 <- createDataPartition(y = dataset$BUILD,
                                        p = 1/17,
                                        list = F)

# set train controls for classification ####
fitControl_cls <-
  trainControl(method = "repeatedcv",
               number = 10,
               repeats = 5,
               classProbs = T,
               summaryFunction = multiClassSummary)

# Model 1: K Nearest Neighbors (weighted) ####
# fit model to predict building
wifi_build <- wifi_data[sample_indices_1, ]

knnGrid <-
  expand.grid(kmax = 10,
              distance = 1,
              kernel = "optimal")

train_set <-
  subset(wifi_build, select = -c(LON, LAT, FLOOR))

knnFit_build <-
  train(BUILD ~ .,
        data = train_set,
        method = "kknn",
        trControl = fitControl_cls,
        tuneGrid = knnGrid,
        preProc = c("center", "scale"),
        metric = "Kappa",
        verbose = T)

# predict building on new dataset
# append predictions, then predict floor
wifi_floor <- wifi_data[sample_indices_2, ]
wifi_floor %>%
  predict(knnFit_build, .) -> wifi_floor$BUILD_knn_pred
# check prediction accuracy
confusionMatrix(wifi_floor$BUILD_knn_pred, wifi_floor$BUILD)

# train model to predict floor
# performance metric to optimize: Kappa
train_set <-
  subset(wifi_floor, select = -c(LON, LAT, BUILD))

knnFit_floor <-
  train(FLOOR ~ .,
        data = train_set,
        method = "kknn",
        trControl = fitControl_cls,
        tuneGrid = knnGrid,
        preProc = c("center", "scale"),
        metric = "Kappa",
        verbose = T)

# predict building and floor
wifi_lon <- wifi_data[sample_indices_3, ]
wifi_lon %>%
  predict(knnFit_build, .) -> wifi_lon$BUILD_knn_pred
wifi_lon %>%
  predict(knnFit_floor, .) -> wifi_lon$FLOOR_knn_pred
# check prediction accuracy
confusionMatrix(wifi_lon$FLOOR_knn_pred, wifi_lon$FLOOR)
# check: any conflicting (building, floor) pairs in predictions?
wifi_lon %>%
  filter(., FLOOR_knn_pred=="F4" &
            BUILD_knn_pred %in% c("B0", "B1")) %>%
  nrow(.)
# no wrong pairs.

# train model to predict longitude
fitControl_rgr <-
  trainControl(method = "repeatedcv",
               number = 10,
               repeats = 5,
               summaryFunction = defaultSummary)

train_set <-
  subset(wifi_lon, select = -c(LAT, FLOOR, BUILD))

knnFit_lon <-
  train(LON ~ .,
        data = train_set,
        method = "kknn",
        trControl = fitControl_rgr,
        tuneGrid = knnGrid,
        preProc = c("center", "scale"),
        metric = "RMSE",
        verbose = T)

# predict building, floor, and longitude
wifi_lat <- wifi_data[sample_indices_4, ]
wifi_lat %>%
  predict(knnFit_build, .) -> wifi_lat$BUILD_knn_pred
wifi_lat %>%
  predict(knnFit_floor, .) -> wifi_lat$FLOOR_knn_pred
wifi_lat %>%
  predict(knnFit_lon, .) -> wifi_lat$LON_knn_pred

# check prediction accuracy
confusionMatrix(wifi_lat$BUILD_knn_pred, wifi_lat$BUILD)
# check prediction accuracy
confusionMatrix(wifi_lat$FLOOR_knn_pred, wifi_lat$FLOOR)
# check accuracy of longitude predictions
accuracy <- postResample(pred = wifi_lat$LON_knn_pred,
                             obs = wifi_lat$LON)
print(accuracy)
# check: any non-existing (building, floor) pairs in predictions?
wifi_lat %>%
  filter(., FLOOR_knn_pred=="F4" &
            BUILD_knn_pred %in% c("B0", "B1")) %>%
  nrow(.)

# train model to predict latitude
train_set <-
  subset(wifi_lat, select = -c(LON, FLOOR, BUILD))

knnFit_lat <-
  train(LAT ~ .,
        data = train_set,
        method = "kknn",
        trControl = fitControl_rgr,
        tuneGrid = knnGrid,
        preProc = c("center", "scale"),
        metric = "RMSE",
        verbose = T)

# define test set
wifi_test <- wifi_data[-c(sample_indices_1, sample_indices_2,
                          sample_indices_3, sample_indices_4), ]
# test model on remaining observations
wifi_test %>%
  predict(knnFit_build, .) -> wifi_test$BUILD_knn_pred
wifi_test %>%
  predict(knnFit_floor, .) -> wifi_test$FLOOR_knn_pred
wifi_test %>%
  predict(knnFit_lon, .) -> wifi_test$LON_knn_pred
wifi_test %>%
  predict(knnFit_lat, .) -> wifi_test$LAT_knn_pred
# check: any conflicting (building, floor) pairs in predictions?
wifi_test %>%
  filter(., FLOOR_knn_pred=="F4" &
            BUILD_knn_pred %in% c("B0", "B1")) %>%
  nrow(.)
# no wrong pairs.

# check prediction accuracy
# classifiers: confusion matrix
confusionMatrix(wifi_test$BUILD_knn_pred, wifi_test$BUILD)
confusionMatrix(wifi_test$FLOOR_knn_pred, wifi_test$FLOOR)
# regressors: RMSE, R squared, MAE
accuracy_lon <-
  postResample(pred = wifi_test$LON_knn_pred, obs = wifi_test$LON)
print(accuracy_lon)
accuracy_lat <-
  postResample(pred = wifi_test$LAT_knn_pred, obs = wifi_test$LAT)
print(accuracy_lat)

# SENSITIVITY (RECALL): proportion of correctly identified users on a floor
#                       relative to all users on that floor.
# SPECIFICITY: proportion of users correctly identified as not on that floor,
#              relative to all users not on that floor.

# Model 2: Support Vector Machines ####
# train SVM model to predict building
wifi_build <- wifi_data[sample_indices_1, ]

svmGrid_cls <-
  expand.grid(C = 0.1)  # optimal C for building

train_set <-
  subset(wifi_build, select = -c(LON, LAT, FLOOR))

svmFit_build <-
  train(BUILD ~ .,
        data = train_set,
        method = "svmLinear",
        trControl = fitControl_cls,
        tuneGrid = svmGrid_cls,
        preProc = c("center", "scale"),
        metric = "Kappa",
        verbose = T)

# append predictions
wifi_floor <- wifi_data[sample_indices_2, ]
wifi_floor %>%
  predict(svmFit_build, .) -> wifi_floor$BUILD_svm_pred
# check prediction accuracy
confusionMatrix(wifi_floor$BUILD_svm_pred, wifi_floor$BUILD)

# train model to predict floor
svmGrid_cls <-
  expand.grid(C = 1.0)  # optimal C for floor

train_set <-
  subset(wifi_floor, select = -c(LON, LAT, BUILD))

tic()
svmFit_floor <-
  train(FLOOR ~ .,
        data = train_set,
        method = "svmLinear",
        trControl = fitControl_cls,
        tuneGrid = svmGrid_cls,
        preProc = c("center", "scale"),
        metric = "Kappa",
        verbose = T)
toc()

# predict building and floor
wifi_lon <- wifi_data[sample_indices_3, ]
wifi_lon %>%
  predict(svmFit_build, .) -> wifi_lon$BUILD_svm_pred
wifi_lon %>%
  predict(svmFit_floor, .) -> wifi_lon$FLOOR_svm_pred
# check prediction accuracy
confusionMatrix(wifi_lon$BUILD_svm_pred, wifi_lon$BUILD)
confusionMatrix(wifi_lon$FLOOR_svm_pred, wifi_lon$FLOOR)
# check: any non-existing (building, floor) pairs in predictions?
wifi_lon %>%
  filter(., FLOOR_svm_pred=="F4" &
            BUILD_svm_pred %in% c("B0", "B1")) %>%
  nrow(.)

# train model to predict the longitude
svmGrid_rgr <-
  expand.grid(C = 10)  # optimal C for longitude

train_set <-
  subset(wifi_lon, select = -c(LAT, FLOOR, BUILD))

tic()
svmFit_lon <-
  train(LON ~ .,
        data = train_set,
        method = "svmLinear",
        trControl = fitControl_rgr,
        tuneGrid = svmGrid_rgr,
        preProc = c("center", "scale"),
        metric = "RMSE",
        verbose = T)
toc()

# predict building, floor, and longitude
wifi_lat <- wifi_data[sample_indices_4, ]
wifi_lat %>%
  predict(svmFit_build, .) -> wifi_lat$BUILD_svm_pred
wifi_lat %>%
  predict(svmFit_floor, .) -> wifi_lat$FLOOR_svm_pred
wifi_lat %>%
  predict(svmFit_lon, .) -> wifi_lat$LON_svm_pred

# check prediction accuracy
confusionMatrix(wifi_lat$BUILD_svm_pred, wifi_lat$BUILD)
# check prediction accuracy
confusionMatrix(wifi_lat$FLOOR_svm_pred, wifi_lat$FLOOR)
# check accuracy of longitude predictions
accuracy <- postResample(pred = wifi_lat$LON_svm_pred,
                         obs = wifi_lat$LON)
print(accuracy)

# train model to predict latitude
svmGrid_rgr <-
  expand.grid(C = 1.0)  # optimal C for latitude

train_set <-
  subset(wifi_lat, select = -c(LON, FLOOR, BUILD))

svmFit_lat <-
  train(LAT ~ .,
        data = train_set,
        method = "svmLinear",
        trControl = fitControl_rgr,
        tuneGrid = svmGrid_rgr,
        preProc = c("center", "scale"),
        metric = "RMSE",
        verbose = T)

# define test set
wifi_test <- wifi_data[-c(sample_indices_1, sample_indices_2,
                          sample_indices_3, sample_indices_4), ]
# test model on remaining observations
wifi_test %>%
  predict(svmFit_build, .) -> wifi_test$BUILD_svm_pred
wifi_test %>%
  predict(svmFit_floor, .) -> wifi_test$FLOOR_svm_pred
wifi_test %>%
  predict(svmFit_lon, .) -> wifi_test$LON_svm_pred
wifi_test %>%
  predict(svmFit_lat, .) -> wifi_test$LAT_svm_pred
# check: any non-existing (building, floor) pairs in predictions?
wifi_test %>%
  filter(., FLOOR_svm_pred=="F4" &
           BUILD_svm_pred %in% c("B0", "B1")) %>%
  nrow(.)
# no wrong pairs.
# check prediction accuracy
confusionMatrix(wifi_test$BUILD_svm_pred, wifi_test$BUILD)
confusionMatrix(wifi_test$FLOOR_svm_pred, wifi_test$FLOOR)
accuracy_lon <-
  postResample(pred = wifi_test$LON_svm_pred, obs = wifi_test$LON)
print(accuracy_lon)
accuracy_lat <-
  postResample(pred = wifi_test$LAT_svm_pred, obs = wifi_test$LAT)
print(accuracy_lat)

# try tree-based model

# visualize misclassified instances
