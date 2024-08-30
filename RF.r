

forex <- read.csv("C:\\forex_data\\USDJYP.csv", header =TRUE)
c<-forex[,5]
o<-forex[,2]
h<-forex[,3]
l<-forex[,4]
v <- forex[,6]
Data_Frame <- data.frame(o,h,l,c,v)
str(Data_Frame)
table(Data_Frame$c)

# MAX-MIN NORMALIZATION
normalize <- function(X) {
  return ((X - min(X)) / (max(X) - min(X)))
}
maxmindf <- as.data.frame(lapply(Data_Frame, normalize))


#install.packages("dplyr")
# Create features and target
library(dplyr)
X <- maxmindf %>%
  select(o,h,l,v)
y <- maxmindf$c

####create train and test data set
ind <- sample(1:nrow(maxmindf),400)

##install.packages("caret")
library(caret)
# Split data into training and test sets
index <- createDataPartition(y, p=0.7, list=FALSE)
X_train <- X[ index, ]
X_test <- X[-index, ]
y_train <- y[index]
y_test<-y[-index]


#install.packages("randomForest")
library(randomForest)

# Train the model 
regr <- randomForest(x = X_train, y = y_train , maxnodes = 10, ntree = 10)

# Make prediction
predictions <- predict(regr, X_test)

result <- X_test
result['CloseActualValues'] <- y_test
result['prediction']<-  predictions

plot(result)
head(result)


##Accuracy
predicted=result$prediction * abs(diff(range(c))) + min(c)
actual=result$CloseActualValues * abs(diff(range(c))) + min(c)
comparison=data.frame(predicted,actual)
deviation=((actual-predicted)/actual)
comparison=data.frame(predicted,actual,deviation)
accuracy=1-abs(mean(deviation))
accuracy

#par(mfrow=c(1,2))
#plot(testDF$c,actualValues,col='red',main='Real vs predicted NN',pch=18, cex=0.7)
#abline(0,1,lwd=2)
#legend('bottomright',legend='NN',pch=18,col='red', bty='n')
#plot(testDF$c,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
#abline(0,1,lwd=2)
#legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

#plot(testDF$c,actualValues,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
#points(testDF$c,pr.lm,col='blue',pch=18,cex=0.7)
#abline(0,1,lwd=2)
#legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))

#install.packages("ggplot2")
# Import library for visualization
library(ggplot2)

# Build scatterplot
ggplot(  ) + 
  geom_point( aes(x = X_test$v, y = y_test, color = 'red', alpha = 0.5) ) + 
  geom_point( aes(x = X_test$v , y = predictions, color = 'blue',  alpha = 0.5)) + 
  labs(x = "Volume", y = "Close", color = "", alpha = 'Transperency') +
  scale_color_manual(labels = c( "Predicted", "Real"), values = c("blue", "red")) 

#install.packages("Metrics")
library(Metrics)
#library(caret)
print(paste0('MAE: ' , mae(y_test,predictions) ))
print(paste0('MSE: ' ,caret::postResample(predictions , y_test)['RMSE']^2 ))
print(paste0('R2: ' ,caret::postResample(predictions , y_test)['Rsquared'] ))


#Tuning the parameters
# If training the model takes too long try setting up lower value of N
N=500 #length(X_train)
X_train_ = X_train[1:N , ]
y_train_ = y_train[1:N]

seed <-7
metric<-'RMSE'

customRF <- list(type = "Regression", library = "randomForest", loop = NULL)

customRF$parameters <- data.frame(parameter = c("maxnodes", "ntree"), class = rep("numeric", 2), label = c("maxnodes", "ntree"))

customRF$grid <- function(x, y, len = NULL, search = "grid") {}

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, maxnodes = param$maxnodes, ntree=param$ntree, ...)
}

customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# Set grid search parameters and Cross Validation with Manual Fine Tuning
control <- trainControl(method="repeatedcv", number=10, repeats=3, search='grid')

# Outline the grid of parameters
tunegrid <- expand.grid(.maxnodes=c(70,80,90,100), .ntree=c(900, 1000, 1100))
set.seed(seed)

# Train the model
rf_gridsearch <- train(x=X_train_, y=y_train_, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
plot(rf_gridsearch)
#best parameter
rf_gridsearch$bestTune
#Defining and visualizing variables importance
varImpPlot(rf_gridsearch$finalModel, main ='Feature importance')



