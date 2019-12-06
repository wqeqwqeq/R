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
mydata<-read.table("LogisticsRegressionExample1DataInR.txt",
                   sep="\t",
                   header=T)
# Display breakdown of the purchase variable
table(mydata$purchase)
# Compute error rate if always predict No
nrow(mydata[mydata$purchase=="Yes",])/nrow(mydata)
# Convert the dependent variable to a nominal factor
mydata$purchase<-as.factor(mydata$purchase)
# Convert the zip variable to a nominal factor
mydata$zip<-as.factor(mydata$zip)
contrasts(mydata$purchase)   
# We see that the model will predict the class whose value
# is 1 - in this case, Yes
####################################
### Create two random subsets     ##
####################################
trainprop <- 0.8
n <- nrow(mydata)
# Create a vector of random integers of training size from
# the vector 1:n
train  <-  sample(n, trainprop * n)
test <- setdiff(1:n, train)
# Create the data frames using the indices created in the
# three vectors above
trainset <- mydata[train,]
trainset.x <- mydata[train,-4]
trainset.y <- mydata[train,4]
testset.x <- mydata[test,-4]
testset.y <- mydata[test,4]
# Build a Naive Bayes model
mymodel <- naiveBayes(purchase ~ ., data = trainset)
# Display Training Confusion Matrix
y.hat.train <- predict(mymodel, trainset.x)
table(trainset.y, y.hat.train)
# Compute Training Error Rate
mean(y.hat.train != trainset.y)
#...and now the moment of reckoning...
y.hat.test <- predict(mymodel, testset.x)
# Display Confusion Matrix
table(testset.y, y.hat.test)
# Compute Error Rate
mean(y.hat.test != testset.y)
# Investigate the structure of the model
#
# The apriori element is the counts of 0 and 1 - these
# proportions become the priors, thus the name
mymodel$apriori
# The tables element depends on the level of measurement of
# the variables For factors, the probabilities p(xi|y) are
# provided for each class in y These are simply the
# proportions of each factor value by class
mymodel$tables$zip
# For continuous variables, the mean and standard deviations
# by class This particular function assumes Gaussian
# (normal) distributions within each class.
mymodel$tables$income
mean(trainset.x$income[trainset.y == 'Yes'])
sd(trainset.x$income[trainset.y == 'Yes'])
# The remaining elements of mymodel are the levels for the
# classes in y and the call that created the model
mymodel$levels
mymodel$call

