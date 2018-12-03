# some plots with ggplot2
# color histograms: salary
ggplot() +
  geom_histogram(data = cust,
                 aes(x= cust$salary, fill= cust$brand),
                 bins = 20) +
  labs(x= 'salary', fill= 'Brand')

# age
ggplot() +
  geom_histogram(data = cust,
                 aes(x= cust$age, fill= cust$brand),
                 bins = 20) +
  labs(x= 'age', fill= 'Brand')

# credit
ggplot() +
  geom_histogram(data = cust,
                 aes(x= cust$credit, fill= cust$brand),
                 bins = 20) +
  labs(x= 'credit', fill= 'Brand')

# barplots (categories): education level
ggplot() +
  geom_bar(data = cust,
                 aes(x= cust$elevel, fill= cust$brand)) +
  labs(x= 'education level', fill= 'Brand')

# car brand
ggplot() +
  geom_bar(data = cust,
           aes(x= cust$car, fill= cust$brand)) +
  labs(x= 'car brand', fill= 'Brand')

# zipcode
ggplot() +
  geom_bar(data = cust,
           aes(x= cust$zipcode, fill= cust$brand)) +
  labs(x= 'zip code', fill= 'Brand')

# multi-factor barplot: elevel and zipcode
ggplot() +
  geom_bar(data = cust, aes(x= cust$zipcode, fill= cust$elevel)) +
  labs(x= 'zip code', fill= 'elevel')

# detect outliers with boxplots: salary
ggplot() +
  geom_boxplot(data = cust,
               aes(x= cust$brand,
                   y= cust$salary,
                   fill= cust$brand))

# age
ggplot() +
  geom_boxplot(data = cust,
               aes(x= cust$brand,
                   y= cust$age,
                   fill= cust$brand))

# credit
ggplot() +
  geom_boxplot(data = cust,
               aes(x= cust$brand,
                   y= cust$credit,
                   fill= cust$brand))

# final KNN prediction chart for unseen data
ggplot() +
  geom_bar(data = custPredictedPref,
                    aes(x= custPredictedPref$brand_pred_knn,
                        fill=custPredictedPref$brand_pred_knn)) +
  labs(x='Predicted Preference, KNN',
       fill='Brand',
       y='Number of Customers') +
  theme_gray(base_size = 20) + ylim(0, 3400)

# final rf prediction chart for unseen data
ggplot() +
  geom_bar(data = custPredictedPref,
           aes(x= custPredictedPref$brand_pred_rf,
               fill=custPredictedPref$brand_pred_rf)) +
  labs(x='Predicted Preference, Rf',
       fill='Brand',
       y='Number of Customers') +
  theme_gray(base_size = 20) + ylim(0, 3400)

# plot bars with final preferences by brand prediction
ggplot() +
  geom_bar(data = total,
           aes(total$brand_total),
           #show.legend = T,
           width = 0.6,
           color= 'black',
           fill= 'blue') +
  geom_bar(data = pred,
           aes(pred$predicted),
           #show.legend = T,
           width = 0.6,
           color= 'black',
           fill= 'violet') +
  ylim(0, 10000) +
  labs(x='', y='Number of Customers') +
  theme_bw(base_size = 18) +
  ggtitle(label= 'Brand Preference') +
  theme(plot.title = element_text(size= 18))

# nice violin plot with ggpubr
# preferences by income
ggviolin(data = total,
         x='brand_total',
         y='salary',
         fill='brand_total',
         width = 0.6) +
  scale_fill_manual(values=c("yellow", "red")) +
  labs(x='', y='Salary ($)') +
  theme_pubclean(base_size = 16) +
  rremove('legend')
