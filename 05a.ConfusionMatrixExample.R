###########################################################
### Earlier Example with ROC curve plots          ###
###########################################################
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
# These packages are needed to perform ROC curve analysis
needed <- c('verification', 'pROC')     
installIfAbsentAndLoad(needed)
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
aucc <- roc.area(as.integer(factor(data[, "purchase"]))-1,
                 glm.fit$fitted.values)
aucc$A
aucc$p.value                #null hypothesis: aucc=0.5 
roc.plot(as.integer(as.factor(data[,"purchase"]))-1,
         glm.fit$fitted.values, 
         main="ROC Curve")
legend("bottomright", bty="n",
       sprintf("Area Under the Curve (AUC) = %1.3f", aucc$A))
