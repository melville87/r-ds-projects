## Analyzing Datasets and Applying Machine Learning Models using R
The repository contains R scripts and R Markdown documents on the following projects:   


* __Predictive Analytics to estimate sales performance of new products__: sales volumes of a anumber of new products to be launched on the market is estimated using data on similar products that the company already sells on the market. Three different regression models are fitted to the dataset:   
  1. Gradient-boosted trees (`gbm`). 
  2. Support Vector Machines (`svmLinear`). 
  3. Regularized Random Forest (`rf`). 
The best model is selected after comparing the results on cross-validation samples, and used to make predictions on the set of new products. Results are displayed via `ggplot` charts. The analysis has been laid out on an R Markdown notebook (.rmd); a pdf version including charts has been uploaded. 
