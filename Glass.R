library(caTools)
library(dplyr)
library(ggplot2)
library(caret)
library(class)
library(corrplot)
library(readr)
library(plyr)


glass <- read.csv("D:/DataScience/KNN/glass.csv")
str(glass)
summary(glass)
attach(glass)
View(glass)
glass[, 'Type'] <- as.factor(glass[, 'Type'])
View(glass)
str(glass)


boxplot(RI) 
summary(RI)
hist(RI)
h<-hist(RI, breaks=10, col="lightblue", xlab="RI", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(RI),max(RI),length=40) 
yfit<-dnorm(xfit,mean=mean(RI),sd=sd(RI)) 
yfit <- yfit*diff(h$mids[1:2])*length(RI) 
lines(xfit, yfit, col="blue", lwd=2)
d <- density(RI) # returns the density data 
plot(d, col = "Blue") # plots the results


boxplot(Na) 
summary(Na)
hist(Na)
h<-hist(Na, breaks=10, col="lightblue", xlab="Na", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(Na),max(Na),length=40) 
yfit<-dnorm(xfit,mean=mean(Na),sd=sd(Na)) 
yfit <- yfit*diff(h$mids[1:2])*length(Na) 
lines(xfit, yfit, col="blue", lwd=2)
d <- density(Na) # returns the density data 
plot(d, col = "Blue") # plots the results


boxplot(Mg) 
summary(Mg)
hist(Mg)
h<-hist(Mg, breaks=10, col="lightblue", xlab="Miles Per Gallon", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(Mg),max(Mg),length=40) 
yfit<-dnorm(xfit,mean=mean(Mg),sd=sd(Mg)) 
yfit <- yfit*diff(h$mids[1:2])*length(Mg) 
lines(xfit, yfit, col="blue", lwd=2)
d <- density(Mg) # returns the density data 
plot(d, col = "Blue") # plots the results



boxplot(Al) 
summary(Al)
hist(Al)
h<-hist(Al, breaks=10, col="lightblue", xlab="Al", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(Al),max(Al),length=40) 
yfit<-dnorm(xfit,mean=mean(Al),sd=sd(Al)) 
yfit <- yfit*diff(h$mids[1:2])*length(Al) 
lines(xfit, yfit, col="blue", lwd=2)
d <- density(Al) # returns the density data 
plot(d, col = "Blue") # plots the results


boxplot(Si) 
summary(Si)
hist(Si)
h<-hist(Si, breaks=10, col="lightblue", xlab="Si", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(Si),max(Si),length=40) 
yfit<-dnorm(xfit,mean=mean(Si),sd=sd(Si)) 
yfit <- yfit*diff(h$mids[1:2])*length(Si) 
lines(xfit, yfit, col="blue", lwd=2)
d <- density(Si) # returns the density data 
plot(d, col = "Blue") # plots the results



boxplot(K) 
summary(K)
hist(K)
h<-hist(K, breaks=10, col="lightblue", xlab="K", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(K),max(K),length=40) 
yfit<-dnorm(xfit,mean=mean(K),sd=sd(K)) 
yfit <- yfit*diff(h$mids[1:2])*length(K) 
lines(xfit, yfit, col="blue", lwd=2)
d <- density(K) # returns the density data 
plot(d, col = "Blue") # plots the results



boxplot(Ca) 
summary(Ca)
hist(Ca)
h<-hist(Ca, breaks=10, col="lightblue", xlab="Ca", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(Ca),max(Ca),length=40) 
yfit<-dnorm(xfit,mean=mean(Ca),sd=sd(Ca)) 
yfit <- yfit*diff(h$mids[1:2])*length(Ca) 
lines(xfit, yfit, col="blue", lwd=2)
d <- density(Ca) # returns the density data 
plot(d, col = "Blue") # plots the results


boxplot(Ba) 
summary(Ba)
hist(Ba)
h<-hist(Ba, breaks=10, col="lightblue", xlab="Ba", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(Ba),max(Ba),length=40) 
yfit<-dnorm(xfit,mean=mean(Ba),sd=sd(Ba))
yfit <- yfit*diff(h$mids[1:2])*length(Ba) 
lines(xfit, yfit, col="blue", lwd=2)
d <- density(Ba) # returns the density data 
plot(d, col = "Blue") # plots the results



boxplot(Fe) 
summary(Fe)
hist(Fe)
h<-hist(K, breaks=10, col="lightblue", xlab="Fe", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(Fe),max(Fe),length=40) 
yfit<-dnorm(xfit,mean=mean(Fe),sd=sd(Fe)) 
yfit <- yfit*diff(h$mids[1:2])*length(Fe) 
lines(xfit, yfit, col="blue", lwd=2)
d <- density(Fe) # returns the density data 
plot(d, col = "Blue") # plots the results



###Standardize the Data

standard.features <- scale(glass[,1:9])

#Join the standardized data with the target column
data <- cbind(standard.features,glass[10])

#Check if there are any missing values to impute. 
anyNA(data)

# Looks like the data is free from NA's
head(data)


###Data Visualization 
# Below plot explains the relation between different features in `glass` dataset. 


corrplot(cor(data[,-10]))


###Test and Train Data Split

# Using `caTools()` to split the `data`into `train` and `test` datasets with a `SplitRatio` = 0.70. 

set.seed(100)

sample <- sample.split(data$Type,SplitRatio = 0.70)

train <- subset(data,sample==TRUE)

test <- subset(data,sample==FALSE)


###KNN Model

# Using `knn()` to predict our target variable `Type` of the test dataset with `k=1`.

predicted.type <- knn(train[1:9],test[1:9],train$Type,k=1)

#Error in prediction
error <- mean(predicted.type!=test$Type)



#Confusion Matrix

confusionMatrix(predicted.type,as.factor(test$Type))

predicted.type <- NULL
error.rate <- NULL

for (i in 1:10) {
  predicted.type <- knn(train[1:9],test[1:9],train$Type,k=i)
  error.rate[i] <- mean(predicted.type!=test$Type)
  
}

knn.error <- as.data.frame(cbind(k=1:10,error.type =error.rate))


###Choosing K Value by Visualization

# Lets plot `error.type` vs `k` using `ggplot`. 

ggplot(knn.error,aes(k,error.type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:10)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')


# The above plot reveals that error is lowest when `k=3` and then jumps back high revealing that `k=3` is the optimum value. Now lets build our
# model using `k=3` and assess it.

###Result

predicted.type <- knn(train[1:9],test[1:9],train$Type,k=3)
#Error in prediction
error <- mean(predicted.type!=test$Type)
#Confusion Matrix
confusionMatrix(predicted.type,as.factor(test$Type))
