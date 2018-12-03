## Analyzing Datasets and Applying Machine Learning Models using R
The repository contains R scripts and R Markdown documents on the following projects:   


* __Predictive Analytics to estimate sales performance of new products__: sales volumes of a anumber of new products to be launched on the market is estimated using data on similar products that the company already sells on the market. Three different regression models are fitted to the dataset:   
  1. Gradient-boosted trees (`gbm`). 
  2. Support Vector Machines (`svmLinear`). 
  3. Regularized Random Forest (`rf`). 
The best model is selected after comparing the results on cross-validation samples, and used to make predictions on the set of new products. Results are displayed via `ggplot` charts. The analysis has been laid out on an R Markdown notebook (.rmd); a pdf version including charts has been uploaded.   

* __Predictive Analytics on customer preferences__: brand preferences of ~10k customers of a company selling electronic items have been collected through a market survey. Part of the dataset is however missing: a machine learning model fitted on the complete dataset (survey_complete.csv) helps fill the gap by predicting the customer preferences on the incomplete survey dataset (survey_incomplete.csv). A breakdown of customers' preferences by customer feature (e.g.: income, location, etc.) can be achieved by visualizing the data with the `ggplot` package.  

* __Indoor positioning by WiFi localization__: a dataset that includes a set of WAP (wireless access point) signal intensities (RSSI) along with information about the position of the phone that recorded them (which consists of four attributes: building, floor, longitude and latitude) is leveraged to train a model that will predict the phone location. The model is deployed to predict the phone location on a different validation dataset. Tested models include: 
  1. K Nearest Neighbors (KNN) with distance-dependent weights (`kknn`). 
  2. Support Vector Machines (`svmLinear`). 
  3. Gradient-boosted trees with XGBoost (`xbgtree`). 
 Further analysis is conducted to determine which of the many predictors are most significant using principal component analysis (in progress). 

* __Finding association rules__: the package `arules` provides a convenient tool to investigate customer buying patterns in a database of transactions. Association rules between groups of products bought by customers can be constructed and evaluated to give a picture of the type of items that customers most frequently buy together. 
