
# 1.	Naive Bayes using e1071 package

require('e1071')

#2.	Use the EmployeeCleanedDataset.csv 

mydata<-read.csv("C:/Users/Harsh Darji/Desktop/EmployeeCleanedData.csv")
#Reading the csv file 

head(mydata)

library(caret)

library(e1071)

#3.	Split the set into train and test

intrain<-createDataPartition(y=mydata$gender,p=0.7,list=FALSE) #spiting 70-30 
training<-mydata[intrain,]
testing<-mydata[-intrain,]

#split <- sample(nrow(mydata), nrow(mydata) * 2/3)
#train <- mydata[split, ]
#test <- mydata[-split, ]


head(training)

# 4.	Convert the variables to factors

str(training)

head(testing)

str(training)

#training[] <- lapply(training, factor) 
#testinng[] <- lapply(testing, factor)


# 5.	Using Naïve Bayes on training data set. With the flag for Laplace as 1, which means it will use Laplace smoothing for zero probabilities.

nb_default <- naiveBayes(Attrition ~., laplace = 1, data = training, na.action = na.pass)

# 6.	Fit the model on test data

nb_predict <- predict(nb_default, testing[,-(10)], type = c("class"))

# 7.	Get the confusion matrix and accuracy

matrix <- table(nb_predict, testing$Attrition)

matrix

# 8.	Print accuracy 

accuracy <- sum(diag(matrix)) / sum(matrix)

accuracy

###EXTRA

table(training$Attrition)

table(testing$Attrition)


