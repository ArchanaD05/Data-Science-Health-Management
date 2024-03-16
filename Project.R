HMO_data <- read.csv("G:/Courses/IDS notes/Project/HMO_data.csv")
datafile <- HMO_data

#data cleansing
colSums(is.na(datafile))
library(imputeTS)

#removing NAs using na_interpolation
datafile$bmi<- na_interpolation(datafile$bmi)
datafile$hypertension <- na_interpolation(datafile$hypertension)
colSums(is.na(datafile))

#removing bad data from hypertension column
datafile <- subset(datafile, hypertension != 0.5)
datafile$smoker <- ifelse(datafile$smoker == "yes", 1,0)
datafile$exercise <- ifelse(datafile$exercise == "Active", 1,0)
View(datafile)


#decision tree using rpart
library(caret)
library(kernlab)

#creating data partition with 70% training data
trainlist = createDataPartition(datafile$cost, p = 0.7, list = F)

#creating training data
training <- datafile[trainlist,]
#checking the number of rows and columns in the training data
print(dim(training))

#creating testing data
testing <- datafile[-trainlist, ]
#checking the number of rows and columns in the testing data
print(dim(testing))


#building model with rpart
library(e1071)
library(rpart)
library(rpart.plot)
tree<-rpart(cost ~ ., data = training, method = "anova")
plot(tree)
text(tree)

#predicting the test data and building the confusion matrix
pred <-as.factor(predict(tree,testing, type = "vector"))
confusionMatrix(pred,testing$cost)










