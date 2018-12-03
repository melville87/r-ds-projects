# load header file
source('./basket_analysis_header.R')

# explore transaction database
# inspect first 6 items
inspect(basket[1:6])

# store item labels
ilabels <- itemLabels(basket)

# get summary of transactional database
summary(basket)

# get size of every transaction
t_sizes <- size(basket)

# plot boxplot and histogram of sizes
# get item (relative) frequencies of first 5 items
itemFrequency(basket[ , 1:5], type= 'relative')

# get absolute frequencies
itemFrequency(basket[ , 1:5], type= 'absolute')

# top 10 most traded items
itemFrequencyPlot(basket, topN= 20, type= 'relative')
itemFrequencyPlot(basket, topN= 20, type= 'absolute')

# visualize sparse matrix: first 100 transactions
image(basket[ 1:100, ])

# 100 randomly chosen transactions
t_subset <- sample(nrow(basket), 100, replace = F)
image(basket[ t_subset, ])

# make cross-table of transactions
# pairs of items grouped by "measure":
# count (item count in database)
# support (item frequency in database)
# confidence (conditional probability: P(A|B))
# lift
# chi squared (significance test for association
# between two categorical variables)
tbl <- crossTable(basket, measure= 'count', sort= T)
tbl <- crossTable(basket, measure= 'support', sort= T)
tbl <- crossTable(basket, measure= 'confidence', sort= T)
tbl <- crossTable(basket, measure= 'lift', sort= T)
tbl <- crossTable(basket, measure= 'chi', sort= T)

# mine rules with apriori algorithm
controls <- new('APcontrol',
                verbose= T)
parameters <- list(supp= 0.002,
                   conf= 0.6,
                   minlen= 2,
                   target= 'rules')
rules <- apriori(data = basket,
                 parameter = parameters,
                 control = controls)

# view rules
inspect(rules[1:5])

# view top n rules sorted by confidence
rules %>%
  head(., n= 5, by= 'confidence', decreasing= T) %>%
  inspect(.)

# write mined rules to a file
# initial filter: minimum support
write(rules, file = "rules_minSupp.csv",
      sep=",", row.names=F, append= F)

# filter using confidence: minimum confidence
write(rules, file = "rules_minConf.csv",
      sep=",", row.names=F, append= F)

# filter redundant rules
rules %>%
  subset(., subset= !is.redundant(.)) -> rules

# check if rules are statistically significant
rules %>%
  subset(., is.significant(., basket,
                              method = "fisher",
                              alpha = 0.01,
                              adjust = "bonferroni")) -> rules

# sort rules by lift
rules %>%
  sort(., by= 'lift', descending= T) -> rules

# are there rules with lift ~ 1 ? No.
# no need to perform statistical significance test
# write to file sorted rules (excluding redundancies)
write(rules, file = "sorted_rules_minConf.csv",
      sep=",", row.names=F, append= F)

# visualize rules with package arulesViz
plot(rules, jitter = 0,
     main= 'Scatter Plot for 147 Association Rules in Basket')

# select most robust rules:
# order by lift
rules %>%
  sort(., by= 'lift') %>%
  head(., n= 100) -> r_rules

# alternatively, select rules with confidence > 0.6:
# rules %>%
#   subset(., quality(.)$confidence > 0.6) -> r_rules
# store rules in csv file
write(r_rules, file = "sorted_r_rules_lift.csv",
      sep=",", row.names=F, append= F)

# plot pruned rules and inspect them
# get right hand side (rhs) of rules
r_rules %>% rhs(.) %>% inspect(.)
# there are only 3 different rhs sets

# analyze selected rules
# find common patterns
for( i in 1:length(lenovo_rhs) ) {
  if( grepl("Desktop", labels(lhs(lenovo_rhs))[i], fixed = T) ) {
    print(i)
  }
}

# create null vector, fill it with instances
v <- c()
for( i in 1:length(hp_laptop_rhs) ) {
  if( grepl("Desktop", labels(lhs(hp_laptop_rhs))[i], fixed = T) ) {
    v <- c(v, i)
  }
}
