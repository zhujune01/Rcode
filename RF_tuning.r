###http://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/

library(randomForest)
library(ROCR)

library(mlbench)
library(caret)

# Load Dataset
data(Sonar)
dataset <- Sonar
x <- dataset[,1:60]
y <- dataset[,61]

# plot.randomForest shows how OOB error and in-class OOB error evolved with increasing number of trees; 
# for classification, black solid line for overall OOB error 
# and a bunch of colour lines, one for each class' error (i.e. 1-this class recall).
# for regression, one black solid line for OOB MSE error.

# print(rf.model) and that gave me a confusion matrix with the class.error.

# varImpPlot shows attribute importance measures for top attributes and 
# MDSplot all objects plotted on the 2D projection of RF object proximity measure.

data(mtcars)
mtcars.rf <- randomForest(mpg ~ ., data=mtcars, ntree=1000, keep.forest=FALSE,
                          importance=TRUE)
plot(mtcars.rf, log="y")
varImpPlot(mtcars.rf)

rf3 <- randomForest(bad ~ ., data=train,ntree=200, proximity=TRUE)
DSplot(rf.training1, training$classe)
MDSplot(rf3, train$bad)


model=randomForest(x,y,xtest=x,ytest=y,keep.forest=TRUE). 
prob=predict(model,x,type="prob")

###Random Forest using R

## make formula
varNames <- names(cross.sell.dev)
# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("y")]

# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")

# Add response variable and convert to a formula object
rf.form <- as.formula(paste("y", varNames1, sep = " ~ "))

cross.sell.rf <- randomForest(rf.form,
                              cross.sell.dev,
                              ntree=500,
                              importance=T)

plot(cross.sell.rf)

varImpPlot(rf_50_11,
           sort = T,
           main="Variable Importance",
           n.var=15)

# Variable Importance Table
var.imp <- data.frame(importance(cross.sell.rf,
                                 type=2))
# make row names as columns
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]

# Predicting response variable
cross.sell.dev$predicted.response <- predict(cross.sell.rf ,cross.sell.dev)

# confusion matrix - Load Library or packages

#Install Packages
install.packages("caret"  )
install.packages("e1071")
# Load Library or packages
library(e1071)
library(caret)
## Loading required package: lattice
## Loading required package: ggplot2
# Create Confusion Matrix
confusionMatrix(data=cross.sell.dev$predicted.response,
                reference=cross.sell.dev$y,
                positive='yes')

# Predicting response variable
cross.sell.val$predicted.response <- predict(cross.sell.rf ,cross.sell.val)

# Create Confusion Matrix
confusionMatrix(data=cross.sell.val$predicted.response,
                reference=cross.sell.val$y,
                positive='yes')



#Getting Lift Charts in R
library(ROCR)

# Creating performance object
perf.obj <- prediction(predictions=termCrossSell$predicted,
                       labels=termCrossSell$target)
# Get data for ROC curve
lift.obj <- performance(perf.obj, measure="lift", x.measure="rpp")
plot(lift.obj,
     main="Cross-Sell - Lift Chart",
     xlab="% Populations",
     ylab="Lift",
     col="blue")
abline(1,0,col="grey")

###ROC curve
test$rf_score<-predict(rf, type='prob', test)[,2]
pred_rf<-prediction(test$rf_score, test$bad)
perf_rf<-performance(pred_rf, "tpr", "fpr")
plot(perf_rf, col='green', main='ROC curve')

ains Table using R

install.packages("gains")
library(gains)
# gains table
actual <- ifelse(termCrossSell$target==1,1,0)
gains.cross <- gains(actual=actual , 
                     predicted=termCrossSell$predicted,
                     groups=10)
print(gains.cross)


###Cumulative Lift Chart using R
install.packages("gains")
library(gains)
# gains table
actual <- ifelse(termCrossSell$target==1,1,0)
gains.cross <- gains(actual=actual , 
                     predicted=termCrossSell$predicted,
                     groups=10)
print(gains.cross)



### Lorenz Curve and Gini index
### Gini Index is measure of inequality and Lorenz curve shows inequality visually. 
### Gini Index was developed to measure income inequality in labour market. 
### In a graph, Gini Index is the area between line of equality and Lorenz curve.
### In the predictive model, Gini Index is used for measuring inequality/discrimination 
### among the population ( between Event and Non-event).
install.packages("ineq")
library(ineq)

# Gini Index
ineq(termCrossSell$predicted,type="Gini")

## Lorenz Curve
plot(Lc(termCrossSell$predicted),col="darkred",lwd=2)




rndF1 <- randomForest(train.X, train.Y, test.X, test.Y)
plot(rndF1)
rndF1.legend <- if (is.null(rndF1$test$err.rate)) {colnames(rndF1$err.rate)}
else {colnames(rndF1$test$err.rate)}

legend("top", cex =0.5, legend=rndF1.legend, lty=c(1,2,3), col=c(1,2,3), horiz=T)


set.seed(1)
data(iris)
iris.rf <- randomForest(Species ~ ., iris, proximity=TRUE,
                        keep.forest=FALSE)
MDSplot(iris.rf, iris$Species)

## getTree function to retrieve the tree and plot that separately.

getTree(randomForest(iris[,-5], iris[,5], ntree=10), 3, labelVar=TRUE)

#these two parameters are perhaps the most likely to have the biggest effect on your final accuracy.
#mtry: Number of variables randomly sampled as candidates at each split.
#ntree: Number of trees to grow.


# Algorithm Tune (tuneRF)
# randomForest package provides the tuneRF() function 
# that searches for optimal mtry values given your data.
set.seed(seed)
bestmtry <- tuneRF(x, y, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)


# Tune using Caret
## only mtry parameter is available in caret for tuning.
## Both 10-fold cross-validation and 3 repeats slows down the search process,
## but is intended to limit and reduce overfitting on the training set.

# Create model with default paramters (ntree=500, mtry=sqrt(ncol(x)))
## it is not finished after >18hours

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)


# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))
rf_random <- train(Class~., data=dataset, method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)


# Grid Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)



###craft your own parameter search
##Tune Manually

##We want to keep using caret because it provides a direct point of comparison to our previous models 
##(apples to apples, even the same data splits) and 
##because of the repeated cross validation test harness that we like as it reduces the severity of overfitting.

# Manual Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(x))))
modellist <- list()
for (ntree in c(1000, 1500, 2000, 2500)) {
  set.seed(seed)
  fit <- train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)


# Extend Caret
# We can define our own algorithm to use in caret by defining a list that contains a number of custom named elements 
# that the caret package looks for, such as how to fit and how to predict. 

customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes


# train model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(.mtry=c(1:15), .ntree=c(1000, 1500, 2000, 2500))
set.seed(seed)
custom <- train(Class~., data=dataset, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
summary(custom)
plot(custom)