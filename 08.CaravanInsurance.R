rm(list=ls())
# An Application to Caravan Insurance Data (pp. 165) 85
# predictors that measure demographic characteristics for
# 5,822 individuals Response variable is Purchase, which
# indicates whether or not a given individual will purchase
# a caravan insurance policy
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
# MASS contains the Boston data
needed  <-  c("ISLR", "class")      
installIfAbsentAndLoad(needed)

?Caravan
str(Caravan)
################################################
### Explore the data ###########################
################################################
dim(Caravan)
summary(Caravan$Purchase)
sum(Caravan$Purchase=="Yes")/length(Caravan$Purchase)
#Investigate the scale of the independent variables...
mean(Caravan[1])
#Error message...??? A data frame is really a list, where
#the list elements are vectors with names (the column names)
mean(Caravan[, 1])
head(Caravan, 2)[1:3]    #Display the first three columns of the first two rows
#To get all column means, use apply() on the second
#dimension, ommiting the character column Purchase
head(apply(Caravan[-86], 2, mean))
#Note that the scale of the data in the columns differs 
#considerably KNN computes distances, which are 
#scale-sensitive, so need to normalize the data by making 
#each column's mean = 0 and std. dev. = 1. 
#Recall that scale() normalizes the data when the center
#paramater is T and the scale parameter is true (both
#defaults)
standardized.X <- scale(Caravan[-86])   #scale accepts only numeric values - omit Purchase variable
#Variance of base data is not 1
var(Caravan[, 1])
var(Caravan[, 2])
#Variance of standardized data is 1
var(standardized.X[, 1])
var(standardized.X[, 2])
################################################ ## Create
#training and test sets ############## 
################################################ Create a
#test set composed of 20% of the observations and a training
#set composed of 80% of the observations Set the random seed
set.seed(123)
trainindices <- sample(1:nrow(Caravan), .80*nrow(Caravan))
#Recall setdiff() produces a vector of everything that's in
#the first parameter that's not also in the second
#parameter.
testindices <- setdiff(1:nrow(Caravan), trainindices)         
train.X <- standardized.X[trainindices, ]
test.X <- standardized.X[testindices, ]
#Create a vector of classes for each set
train.Y <- Caravan$Purchase[trainindices]
test.Y <- Caravan$Purchase[testindices]
################################################
### Model with KNN #############################
################################################
#Create a KNN model with k=1
knn.pred <- knn(train.X, test.X, train.Y, k=1)
#Compute the error rate
mean(test.Y!=knn.pred)
#Only about 11% - not bad, BUT...
mean(test.Y=="Yes")
#...could get the error rate down to around 7% by always predicting No.
#Create a confusion matrix
mytable <- table(test.Y, knn.pred)
mytable
#Compute the Power of the test (its ability to find the
#scarce purchasers)
powerknn1 <- (mytable["Yes", "Yes"]/sum(mytable["Yes", ]))
powerknn1
#This success rate is greater than the 7% we would obtain by
#always predicting No Compute the Type I and Type II error
#rates Type I error
(mytable["No", "Yes"]/sum(mytable["No", ]))
#Type II error
(mytable["Yes", "No"]/sum(mytable["Yes", ]))
#Note that the Type II error rate is just 1-Power
1-powerknn1
#Try with K=3 and K=5...
knn.pred <- knn(train.X, test.X, train.Y, k=3)
mytable <- table(knn.pred, test.Y)
mytable
#Compute the Power of the test (its ability to find the
#scarce purchasers)
powerknn3 <- (mytable["Yes", "Yes"]/sum(mytable["Yes", ]))
powerknn3
knn.pred <- knn(train.X, test.X, train.Y, k=5)
mytable <- table(knn.pred, test.Y)
mytable
#Compute the Power of the test (its ability to find the
#scarce purchasers)
powerknn5 <- (mytable["Yes", "Yes"]/sum(mytable["Yes", ]))
powerknn5
#Looks like KNN with K=5 is getting somewhere! 
################################################ ## Model
#with Logistic Regression ############# 
################################################ Fit a
#logistic model to the training subset of the original data
#frame
glm.fit <- glm(Purchase~., data=Caravan, family=binomial, subset=trainindices)
glm.probs <- predict(glm.fit, Caravan[testindices, ], type="response")
glm.pred <- rep("No", length(testindices))
glm.pred[glm.probs>.5] <- "Yes"
mytable <- table(test.Y, glm.pred)
mytable
#Compute the Power of the test (its ability to find the
#scarce purchasers)
powerlogistic50 <- (mytable["Yes", "Yes"]/sum(mytable["Yes", ]))
powerlogistic50
################################################
## Move along the ROC curve to increase Power ##
################################################
glm.pred <- rep("No", length(testindices))
glm.pred[glm.probs>.2]="Yes"
mytable <- table(glm.pred, test.Y)
mytable
#Compute the Power of the test (its ability to find the
#scarce purchasers)
powerlogistic20 <- (mytable["Yes", "Yes"]/sum(mytable["Yes", ]))
powerlogistic20
################################################
### Summary of Model Results ###################
################################################
powerknn1
powerknn3
powerknn5
powerlogistic50
powerlogistic20

