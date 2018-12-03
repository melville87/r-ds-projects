# load libraries
library(readr)
library(caret)
library(dplyr)
library(doMC)

# parallelization
registerDoMC(cores = 4)

# set feature data types prior to loading
classes <- c('numeric', 'numeric', 'factor',
           'factor', 'factor', 'numeric',
           'factor')
# load dataset
cust <- read.csv('../survey_complete.csv',
                 dec= '.', colClasses= classes)

# change level names of target variable
levels(cust$brand) <- c('Acer', 'Sony')

# set seed for random number generator
set.seed(1987)

# split data into train and test sets
# target variable: "brand"
train_indices <-
    createDataPartition(y= cust$brand,
                        p= 0.75,
                        list= F,
                        times= 1)

custTrain <- cust[ train_indices, ]
custTest <- cust[ -train_indices, ]

# cross validation: repeated 10-fold CV
ctrl <-
    trainControl(method= 'repeatedcv',
                 number= 10,
                 repeats= 10,
                 summaryFunction = defaultSummary)

# Model: KNN classifier (simple, unweighted)
knn_param_grid <-
    expand.grid(k= seq(70, 100, 5))

# train model without scaling
knnCls <-
    train(brand ~ .,
          data= custTrain,
          method= 'knn',
          trControl= ctrl,
          preProcess= NULL,
          tuneGrid= knn_param_grid,
          metric= 'Accuracy')

# print results, including accuracy for each resampling step
summary(knnCls)

# apply model to predict brand on test set
test_pred_knn <- predict(knnCls, custTest)

# make dataframe with observed and predicted values
results_knn <-
    data.frame(obs= custTest$brand,
               pred= test_pred_knn)

# calculate accuracy on predictions
postResample(pred = results_knn$pred, obs = results_knn$obs)

# display confusion matrix
confusionMatrix(results_knn$pred, results_knn$obs)

# get other important metrics for binary classifiers:
# - area under ROC curve
# change summary function
ctrl_twoClass <-
    trainControl(method= 'repeatedcv',
                 number= 10,
                 repeats= 10,
                 classProbs= T,
                 summaryFunction= twoClassSummary)

# re-fit model
knnCls <-
    train(brand ~ .,
          data = custTrain,
          method= 'knn',
          trControl= ctrl_twoClass,
          preProcess= NULL,
          tuneGrid= knn_param_grid,
          metric= 'ROC')

# predict test values
test_pred_knn <- predict(knnCls, custTest)

# add predictions
results_knn <-
    data.frame(obs= custTest$brand,
               pred= test_pred_knn)

# get area under ROC
twoClassSummary(results_knn$obs,
                lev= levels(results_knn$pred),
                model = knnCls)

# display ROC area vs. number of neighbors
plot(knnCls, print.thres= 0.5, type= 'S')

# load dataset with incomplete survey
custIncomplete <-
    read.csv('../survey_incomplete.csv',
             dec= '.', colClasses= classes)

# make predictions on unseen data
unseen_pred <-
    predict(knnCls, custIncomplete)

# merge predictions with other customer features
custPredictedPref$brand_pred_knn <- unseen_pred

# Model: Random Forest
# mtry = number of randomly sampled predictors per tree
rf_param_grid <- data.frame(mtry= c(4, 5, 6))

# use Random Forest 'rf'
rf <-
    train(brand ~ .,
          data = custTrain,
          method= 'rf',
          trControl= ctrl_twoClass,
          preProcess= NULL,
          tuneGrid= rf_param_grid,
          metric= 'ROC')

# predict test values
test_pred <- predict(rf, custTest)

# get performance metrics on resamples
postResample(pred = test_pred,
             obs= custTest$brand)

# use model to predict unseen data
unseen_pred <- predict(rf_noscaling, custIncomplete)

# add predictions to data frame
custPredictedPref$brand_pred_rf <- unseen_pred

# model selection using data from resamples
# statistical analysis is performed on the distribution
# of perf. metric (area under ROC)
# for each group of resamples
sig_test <-
    resamples(list('KNN'= knnCls, 'Rf'= rf))

# summary of statistics
summary(sig_test)

# visualize statistics to pick
# best model
bwplot(sig_test)
dotplot(sig_test)
