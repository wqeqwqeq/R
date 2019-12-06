rm(list=ls())
##########################################################################################
### Functions
##########################################################################################
installIfAbsentAndLoad  <-  function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
##############################
### Load required packages ###
##############################
# MASS contains the Boston data
needed   <-   c("ISLR", "MASS", "corrplot", "class")      #MASS contains the qda() function
installIfAbsentAndLoad(needed)
############################################################
### Linear and Quadratic Discriminant Analysis (and KNN) ###
############################################################
# The Stock Market Data: Daily percentage returns for the S&P 500 stock index between 2001 and 2005. 
# Each row also contains returns for a day and returns lagged from 1 to 5 days, volume in billions, 
# and a factor named Direction with levels Down and Up indicating whether the market had a positive
# or negative return that day.
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)    #The pairs() function produces a matrix of scatter plots
head(Smarket)
#The cor() function produces a matrix that contains all of the pairwise correlations among the predictors in a data set.
cor(Smarket[, -9]) 
#Use the corrplot function to plot a Correlation Matrix (Need to install.packages("corrplot")
corrplot(cor(Smarket[, -9]),  mar=c(0, 0, 1, 0))
plot(Smarket$Volume)
#Create a training set of observations from 2001 to 2004 and a test set of observations from 2005 
train <- Smarket$Year<2005
Smarket.2005 <- Smarket[!train, ]             #Smarket.2005 is a data frame of rows that are not in train - this is the test set 
dim(Smarket.2005)
Direction.2005 <- Smarket$Direction[!train]  #Direction.2005 is a factor of "Up" and "Down" from the test data frame (needed for the table() function)
length(Direction.2005)

# Quadratic Discriminant Analysis

qda.fit <- qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit
qda.pred <- predict(qda.fit, Smarket.2005)
table(Direction.2005, qda.pred$class)
mean(qda.pred$class==Direction.2005)

# K-Nearest Neighbors (pp. 164)

#The knn() function requires 4 parameters:
#   1. A matrix containing the predictors associated with the training data, here train.X
#   2. A matrix containing the predictors associated with the data for which we wish to make predictions, here test.X
#   3. A vector containing the class labels for the training observations, here train.Direction
#   4. A value for K, the number of nearest neighbors to be used by the classifier.
train.X <- cbind(Smarket$Lag1, Smarket$Lag2)[train, ]
head(train.X)
nrow(train.X)
test.X <- cbind(Smarket$Lag1, Smarket$Lag2)[!train, ]
head(test.X)
nrow(test.X)
train.Direction <- Smarket$Direction[train]
head(train.Direction)
length(train.Direction)
set.seed(460)           #Ties in nearest neighbors are broken randomly
#Fit a knn model with k=1
knn.pred <- knn(train.X, test.X, train.Direction, k=1)
mytable <- table(Direction.2005, knn.pred)
mytable
(mytable["Up", "Up"]+mytable["Down", "Down"])/sum(mytable)
#Since the error rate with k=1 is so high, try with a less flexible model with k=3.
#Fit a knn model with k=3
knn.pred <- knn(train.X, test.X, train.Direction, k=3)
mytable <- table(Direction.2005, knn.pred)
mytable
(mytable["Up", "Up"]+mytable["Down", "Down"])/sum(mytable)
#Still not great. Looks like QDA with success rate of 0.5992063 beats LDA's 0.5595238 and KNN's 0.5277778
 

