rm(list=ls())
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
needed  <-  c("pROC", "verification")  # pROC for AUC and CI calcs, verification for ROC curve functionality
installIfAbsentAndLoad(needed)

# Consider the following files:
# actuals.txt: 
#   a 1-dimensional file with no header containing
#   the y-values (Leave or Remain) used to construct a
#   classification model

# predicted.txt: 
#   a one-column table (with heading yhat) containing the
#   associated predictions (as probabilities of Leave) of that
#   classification model

# Create a confusion matrix of these results in the form we
# have discussed in class.
actuals <- scan('Actuals.txt',what='character')
predicted.df <- read.table('Predicted.txt', sep='\t', header=T)
head(actuals)
head(predicted.df)
# Convert prediction probabilities to Remain/Leave using 0.5
# as the threshold and remembering that the probabilities
# are of Leave
leave.indices.predicted <- which(predicted.df$yhat > .5)
predicted <- rep('Remain', nrow(predicted.df))
predicted[leave.indices.predicted] <- 'Leave'
# Convert both objects to factors
actuals <- factor(actuals)
predicted <- factor(predicted)
# Create a confusion matrix - actuals always in the rows
# (first parameter in table() function), predicted in the
# columns
mytable <- table(actuals, predicted)   
mytable   
# Wrong structure - always null hypothesis in first col/row
# - here it's Remain - the "do nothing", "not interesting",
# hay in the haystack, not the needle"
contrasts(actuals)
# Change coding of the factors to make Remain < Leave
actuals <- factor(actuals, 
                  ordered=T, 
                  levels=c('Remain', 'Leave'))
predicted <- factor(predicted, 
                    ordered=T, 
                    levels=c('Remain','Leave'))
mytable <- table(actuals, predicted)
mytable
str(mytable)
# Compute important statistics concerning the model
# performance
OverallErrorRate <- (mytable[2] + mytable[3]) / sum(mytable)
OverallErrorRate
type1FalsePositiveErrorRate <- mytable[1, 2] / sum(mytable[1, ])
type1FalsePositiveErrorRate
type2FalseNegativeErrorRate <- mytable[2, 1] / sum(mytable[2, ])
type2FalseNegativeErrorRate
sensitivityPowerRecall <- mytable[2, 2] / sum(mytable[2, ])
sensitivityPowerRecall
specificityTrueNegative <- mytable[1, 1] / sum(mytable[1, ])
specificityTrueNegative
precision <- mytable["Leave", "Leave"] / sum(mytable[, "Leave"])
precision
###Calculate the area under the ROC curve and confidence interval for this value
probs <- predicted.df$yhat
# Plot the ROC curve.
aucc <- roc.area(as.numeric(actuals)-1,probs)$A
roc.plot(as.integer(as.numeric(actuals))-1,probs, 
         main="ROC Curve")
# Let's reduce the Type II error rate by converting the
# prediction probabilities to Remain/Leave using 0.4 as the
# threshold
leave.indices.predicted <- which(predicted.df$yhat > .4)
predicted <- rep('Remain', nrow(predicted.df))
predicted[leave.indices.predicted] <- 'Leave'
#Convert both objects to factors
# Create a confusion matrix
predicted <- factor(predicted, 
                    ordered=T, 
                    levels=c('Remain','Leave'))
mytable <- table(actuals, predicted)
mytable
#Compute important statistics concerning the model performance
OverallErrorRate <- (mytable[2] + mytable[3]) / sum(mytable)
OverallErrorRate
type1FalsePositiveErrorRate <- mytable[1, 2] / sum(mytable[1, ])
type1FalsePositiveErrorRate
type2FalseNegativeErrorRate <- mytable[2, 1] / sum(mytable[2, ])
type2FalseNegativeErrorRate
sensitivityPowerRecall <- mytable[2, 2] / sum(mytable[2, ])
sensitivityPowerRecall
specificityTrueNegative <- mytable[1, 1] / sum(mytable[1, ])
specificityTrueNegative
precision <- mytable["Leave", "Leave"] / sum(mytable[, "Leave"])
precision



