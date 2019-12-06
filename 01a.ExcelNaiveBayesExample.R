rm(list=ls())
# This script replicates the spreadsheet named
# ClassificationIllustrations.xlsx, sheet NaiveBayesExample

##########################################################################################
### Functions
##########################################################################################
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
##############################
### Load required packages ###
##############################
# 
# e1071 contains the naiveBayes() function, which assumes a
# Gaussian dirstibution for continuous predictors and a
# Bernoulli distribution for predictors that are factors.
# 
 
needed  <-  c('e1071')      
installIfAbsentAndLoad(needed)
##############################################################
### Use the naiveBayes function in package e1071 to perform
### Naive Bayes Classification 
##############################################################
# Get the data into a data frame 
data <- read.table("NaiveBayesExampleData.txt",
                   sep="\t",
                   header=T)
# Make the variables factors
newdata <- data.frame(apply(data, 2, factor))
contrasts(newdata$purchaser)
mymodel <- naiveBayes(purchaser ~ ., data = newdata)
x.0 <- data.frame('sports.com'=c('1'), 
                  'gossip.com'=c('1'), 
                  'finance.com'=c('1'), 
                  'health.com'=c('1'),
                  'cooking.com'=c('0'))
y.hat.x0 <- predict(mymodel, newdata=x.0, type='raw')
y.hat.x0
