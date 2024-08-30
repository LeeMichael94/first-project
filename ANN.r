forex <- read.csv("C:\\forex_data\\USDJYP.csv", header =TRUE)
c<-forex[,5]
o<-forex[,2]
h<-forex[,3]
l<-forex[,4]
v <- forex[,6]
DataFrame <- data.frame(o,h,l,c,v)

#install.packages("neuralnet")
library(neuralnet)

###Setting the seed so we get same results each time
###we run neural net
set.seed(123)

###Storing the data set named "forex" into DataFrame
#DataFrame <- forex

###help on forex data
help("DataFrame")

###Structure of forex  Data
str(DataFrame)

###Histogram of 
hist(DataFrame$c)

###Check dimention of data frame
dim(DataFrame)

##Check first 100 row
head(DataFrame,100)

##find out missing value
apply(forex, 2, function(x) sum(is.na(x)))

# MAX-MIN NORMALIZATION
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
maxmindf <- as.data.frame(lapply(DataFrame, normalize))


####create train and test data set
ind <- sample(1:nrow(maxmindf),round(0.70*nrow(maxmindf)))
trainDF <- maxmindf[ind,]
testDF <- maxmindf[-ind,]
lm.fit <- glm(c~., data=trainDF)
summary(lm.fit)
pr.lm <- predict(lm.fit,testDF)
MSE.lm <- sum((pr.lm - testDF$c)^2)/nrow(testDF)

#### 4-2-1

allVars <- colnames(maxmindf)
predictorVars <- allVars[!allVars%in%"c"]
predictorVars <- paste(predictorVars, collapse = "+")
form <- as.formula(paste("c~", predictorVars, collapse = "+"))

neuralModel <- neuralnet(formula = form, hidden = c(5,3), linear.output = TRUE, data = trainDF, threshold=0.01)
#neuralModel <- neuralnet(0.1, hidden = c(4,2), linear.output = TRUE, data = trainDF, threshold=0.01)
neuralModel$result.matrix

### Plot neural net
plot(neuralModel)

###Predict for test data
predictions <- compute (neuralModel, testDF[,1:5])
str(predictions)

#predictions_ <- predictions$net.result*(max(DataFrame$c)-min(DataFrame$c))+min(DataFrame$c)
#actualValues <- (testDF$c) * (max(DataFrame$c)-min(DataFrame$c))+min(DataFrame$c)
#MSE.nn <- sum((actualValues - predictions_)^2)/nrow(testDF)
#print(paste(MSE.lm,MSE.nn))
##plot(testDF$c,predictions,col="blue", main="Real vs Predicted")

##model Validation
results <- data.frame(actual = testDF$c, prediction=predictions$net.result)
head(results)

##Accuracy
predicted=results$prediction * abs(diff(range(c))) + min(c)
actual=results$actual * abs(diff(range(c))) + min(c)
comparison=data.frame(predicted,actual)
deviation=((actual-predicted)/actual)
comparison=data.frame(predicted,actual,deviation)
accuracy=1-abs(mean(deviation))
accuracy

##MSE
MSE.neuralModel <- sum((predicted - actual)^2)/nrow(testDF)
print(MSE.neuralModel)


par(mfrow=c(1,2))
plot(testDF$c,predictions,col='red',main='Real vs predicted NN',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')
plot(testDF$c,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

plot(testDF$c,predictions,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(testDF$c,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))

##cross validation (k-fold)
library(boot)
set.seed(200)
lm.fit <- glm(c~.,data=DataFrame)
cv.glm(DataFrame,lm.fit,K=10)$delta[1]

#install.packages("plyr")
set.seed(450)
cv.error <- NULL
k <- 10
library(plyr)
for(i in 1:k){
  index <- sample(1:nrow(maxmindf),round(0.7*nrow(maxmindf)))
  train.cv <- maxmindf[index,]
  test.cv <- maxmindf[-index,]
  neuralModel <- neuralnet(form,data=train.cv,hidden=c(5,3),linear.output=T)   
  predictions <- compute (neuralModel, test.cv[,1:5])
  predictions <- predictions$net.result*(max(DataFrame$c)-min(DataFrame$c))+min(DataFrame$c)
  test.cv.r <- (test.cv$c)*(max(DataFrame$c)-min(DataFrame$c))+min(DataFrame$c)
  # predicted=results$prediction * abs(diff(range(c))) + min(c)
  #actual.cv=results$actual * abs(diff(range(c))) + min(c)
  cv.error[i] <- sum((predictions - test.cv.r)^2)/nrow(test.cv)    
}
mean(cv.error)
cv.error
boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)


