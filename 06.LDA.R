rm(list=ls())
############################################################
### Linear Discriminant Analysis (and KNN) ### 
############################################################
#The Stock Market Data: Daily percentage returns for the S&P
#500 stock index between 2001 and 2005. Each row also
#contains returns for a day and returns lagged from 1 to 5
#days, volume in billions, and a factor named Direction with
#levels Down and Up indicating whether the market had a
#positive or negative return that day.
require(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)    #The pairs() function produces a matrix of scatter plots
head(Smarket)
#The cor() function produces a matrix that contains all of the pairwise correlations among the predictors in a data set.
cor(Smarket[, -9]) 
#Use the corrplot function to plot a Correlation Matrix (Need to install.packages("corrplot")
require(corrplot)
corrplot(cor(Smarket[, -9]), mar=c(0, 0, 1, 0))
plot(Smarket$Volume)
#Create a training set of observations from 2001 to 2004 and a test set of observations from 2005 
train <- Smarket$Year<2005
Smarket.2005 <- Smarket[!train, ]             #Smarket.2005 is a data frame of rows that are not in train - this is the test set 
dim(Smarket.2005)
Direction.2005 <- Smarket$Direction[!train]  #Direction.2005 is a factor of "Up" and "Down" from the test data frame (needed for the table() function)
length(Direction.2005)
#So...
# train are the T/'Fs defining the rows of Smarket that compose the training set 
# Smarket.2005 is the test df (includes the Y's)
# Direction.2005 is the test Y's alone

#install.packages("MASS")
library(MASS)                              #Contains the lda() function
lda.fit <- lda(Direction~Lag1+Lag2, data=Smarket, subset=train)
lda.fit
 #Observe:
#    The Priors:       These are 0.492 and 0.508 for Down and Up respectively
#    The Group means:  These are the average of each predictor
#                      within each class, and are used by LDA 
#                      as estimates of the mu's for each class
#    The coefficients: These produce the linear discriminants in the LDA decision boundry with p>1 (slide 99). 
#                      If -0.642 * Lag1 - 0.514 * Lag2 is large, then the LDA classifier will predict a market   
#                      increase, and if it is small, then the LDA classifier will predict a market decline.

#The plot() function plots the distributions of the linear discriminants, obtained by computing
# -0.642 * Lag1 - 0.514 * Lag2 for each of the training observations .
plot(lda.fit)
#Use the model to make predictions on the test set
lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
#The predict() function returns a list with three elements.
##The "class" element contains LDA's predictions about the movement of the market (using 0.5 as the cutoff)
##The "posterior" element is the probability of the prediction in the class element
##The "x" element is a matrix of the linear discriminants described earlier
# To obtain probabilities, need to use posterior member
head(lda.pred$posterior[, 2])  # the probs of 1
contrasts(Smarket$Direction)
head(lda.pred$class, 20)
#Create a confusion matrix (always put the actuals in the rows abd the predictions in the columns)
mytable <- table(Direction.2005, lda.pred$class)
mytable
#Compute the error rate 2 ways
(mytable["Up", "Down"]+mytable["Down", "Up"])/sum(mytable)
1-mean(lda.pred$class==Direction.2005)
#Compute the success rate 2 ways
(mytable["Up", "Up"]+mytable["Down", "Down"])/sum(mytable)
mean(lda.pred$class==Direction.2005)
#Predict a direction for 2 new observations
predict(lda.fit, newdata=data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1, -.08)), type='response')
#Notice that the first element of the LDA model's posteriers
#are probabilities of the market going Down, and the second
#element is the probability of it going up
