# set working directory
wd <- file.path('~',
                'mining_rules')
setwd(dir= wd)

# load libraries
library(readr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(plyr)
library(dplyr)
library(magrittr)
# association mining rules
library(arules)
library(arulesViz)

# parallelization
library(doMC)
registerDoMC(cores = 4)

# read dataset (if exists)
input_file <-
    file.path('..',
              'ElectronidexTransactions2017.csv')
if( !file.exists(input_file) ) {
    print('File:')
    print(input_file)
    print('not found!. Current dir:')
    getwd()
}

basket <-
    read.transactions(input_file,
                      format= c('basket'),
                      sep= ",",
                      rm.duplicates = T)
