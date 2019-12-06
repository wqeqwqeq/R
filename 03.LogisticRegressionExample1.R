rm(list=ls())
###########################################################
### Use the glm function to perform logistic regression ###
###########################################################
# Get the data into a data frame
data <- read.table("LogisticsRegressionExample1DataInR.txt",
                   sep="\t", 
                   header=T, 
                   stringsAsFactors=F)
# Display breakdown of the purchase variable
table(data$purchase)
# Compute error rate if always predict Yes
nrow(data[data$purchase == "No", ])/nrow(data)
# Convert the dependent variable to a nominal factor
data$purchase <- as.factor(data$purchase)
# Model will predict the class whose value is 1 - in this 
# case, Yes. Change the ordering of the factor if this is 
# not an apprioriate choice
contrasts(data$purchase)  
# Build the model
glm.fit <- glm(purchase ~ income + age + zip, 
               data=data, 
               family=binomial)
# Display the model results
summary(glm.fit)

# We derive the standard errors above as follows (slide 47):
v <- diag(glm.fit$fitted.values * (1-glm.fit$fitted.values))
x.mm <- model.matrix(~., data[,1:3])
cov.parameters <- solve(t(x.mm) %*% v %*% x.mm)
sqrt(diag(cov.parameters))

# Chi-square Hypothesis test on H0:Model Has No Predictive 
# Power.

# Deviance is analogous to RSS in linear regression. 
# Deviance = -2 * LogLiklihood

# For a particular coefficient, the distribution of the
# Liklihood Ratio Statistic calculated by taking the model
# deviance without the coefficient minus the deviance of the
# model with the coefficient follows a Chi-square
# distribution with (n-1) - (n-p-1) = p degrees of freedom.
pval <- 1 - pchisq(glm.fit$null.deviance - glm.fit$deviance, 
                   glm.fit$df.null - glm.fit$df.residual)
pval    #Reject null hypothesis - model has predictive power
# Compute the AIC (Akaike Information Criterion) - good way 
# to compare two models built using same data

yes.indices <- which(data$purchase=='Yes')
# Model's fittedvalues element is the vector of probabilities
# of a Yes for the training data 

# Create a vector of probs of obtaining the training y-values
# - just the fitted values if y was Yes, otherwise
# 1-fittedvalues (like the spreadsheet examaple)
probs <- rep(0, length(glm.fit$fitted.values))
probs[yes.indices] <- glm.fit$fitted.values[yes.indices]
probs[-yes.indices] <- 1 - glm.fit$fitted.values[-yes.indices]
# Sum of the ln's of the probs is the ln of the product of
# the probs, so the exp of this sum is the product of the
# probs, or the maximum likelihood
maxli <- exp(sum(log(probs)))
# AIC is Akaike Information Criterion (small is better). It
# is a common way to compare models that are constructed
# with the same training set. It is obtained in a number of
# ways, as follows:
(AIC <- glm.fit$aic)
# AIC = 2 * (p + 1) - 2 * LogLiklihood = 2 * (p + 1) + deviance 
(AIC <- 2 * length(glm.fit$coefficients) - 2 * log(maxli))
(AIC <- 2 * length(glm.fit$coefficients) + glm.fit$deviance)
# Evaluate various measures of predictive accuracy
glm.pred <- rep('No', nrow(data))
glm.pred[predict(glm.fit,type='response') > .5] <- 'Yes'
# Compute the proportion of correct and incorrect
# predictions
mean(glm.pred==data$purchase)
mean(glm.pred!=data$purchase)
# So we have cut the naive prediction error rate computed
# earlier (0.3888888) in half

# Create a Confusion Matrix (aka Contingency Table)
(train.table <- table(data$purchase,glm.pred))
# Overall error rate again, this time from the table
(train.table["No","Yes"]+train.table["Yes","No"])/sum(train.table)
# Type I Error Rate
(train.table["No","Yes"]/sum(train.table["No",]))
# Type II Error Rate
(train.table["Yes","No"]/sum(train.table["Yes",]))
# Power: 
1-(train.table["Yes","No"]/sum(train.table["Yes",]))
# We found 11 of the 14 Yes's...78.6%

