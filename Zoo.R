library(ggplot2) #data visualization
library(readr) #to read csv files
library(Hmisc) # descriptive statistics
library(gridExtra) #to display plots in grids
library(plyr) # data manipulation
library(caret)
library(pROC)
library(mlbench)
library(lattice)


zoo  =  read.csv("D:/DataScience/KNN/Zoo.csv")

View(zoo)

str(zoo)
attach(zoo)

summary(zoo)

library(FactoMineR)
library(factoextra)
res.ca <- CA(zoo[,-1], graph = FALSE)
fviz_ca_biplot(res.ca, repel = TRUE)


pie <- ggplot(zoo, aes(x = "", fill = factor(type))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of class", 
       caption="Source: mpg")

pie + coord_polar(theta = "y", start=0)

g <- ggplot(zoo, aes(type))
g + geom_bar(aes(fill=type), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Categorywise Bar Chart", 
       subtitle="Type Of Animals", 
       caption="Source: Types from 'Zoo' dataset")




zoo1 <- zoo[,2:18]
str(zoo1)

zoo1$hair <- as.factor(zoo1$hair)
zoo1$feathers <- as.factor(zoo1$feathers)
zoo1$eggs <- as.factor(zoo1$eggs)
zoo1$milk <- as.factor(zoo1$milk)
zoo1$airborne <- as.factor(zoo1$airborne)
zoo1$aquatic <- as.factor(zoo1$aquatic)
zoo1$predator <- as.factor(zoo1$predator)
zoo1$toothed <- as.factor(zoo1$toothed)
zoo1$backbone <- as.factor(zoo1$backbone)
zoo1$breathes <- as.factor(zoo1$breathes)
zoo1$venomous <- as.factor(zoo1$venomous)
zoo1$fins <- as.factor(zoo1$fins)
zoo1$legs <- as.factor(zoo1$legs)
zoo1$tail <- as.factor(zoo1$tail)
zoo1$domestic <- as.factor(zoo1$domestic)
zoo1$catsize <- as.factor(zoo1$catsize)
zoo1$type <- as.factor(zoo1$type)



# Data partition
set.seed(123)
ind <- sample(2,nrow(zoo1), replace = T, prob = c(0.7,0.3))
train <- zoo1[ind==1,]
test <- zoo1[ind==2,]

# KNN Model 

trcontrol <- trainControl(method = "repeatedcv", number = 10,repeats = 3
                          # classprobs are needed when u want to select ROC for optimal K Value
)
set.seed(222)
fit <- train(type ~., data = train, method = 'knn', tuneLength = 20,
             trControl = trcontrol, preProc = c("center","scale"))

fit 

plot(fit)

varImp(fit)

pred <- predict(fit, newdata = test )
confusionMatrix(pred, test$type)
