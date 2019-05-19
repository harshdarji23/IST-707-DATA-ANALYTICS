library("RWeka")
trainset <- read.csv("/Users/byu/Desktop/Data/Kaggle-digit-train-sample-small-1400.csv")
trainset <- read.csv("/Users/byu/Desktop/Data/Kaggle-digit-train.csv")
trainset$label=factor(trainset$label)

# print J48 parameters
WOW("J48")
m=J48(label~., data = trainset)
m=J48(label~., data = trainset, control=Weka_control(U=FALSE, M=2, C=0.5))
e1=evaluate_Weka_classifier(m, seed=1, numFolds=3)
e1

WOW("NaiveBayes")
NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
# build default NB model
nb_model=NB(label~., data=trainset)
# turn on discretization
nb_model=NB(label~., data=trainset, control=Weka_control(D=TRUE))
# turn on kernel estimation
nb_model=NB(label~., data=trainset, control=Weka_control(K=TRUE))
e2 <- evaluate_Weka_classifier(nb_model, numFolds = 3, seed = 1, class = TRUE)
e2

WOW("IBk")
knn <- make_Weka_classifier("weka/classifiers/lazy/IBk")
knn_model=knn(label~., data=trainset)
knn_model=NB(label~., data=trainset)
knn_model=NB(label~., data=trainset, control=Weka_control(K=3))
e3 <- evaluate_Weka_classifier(knn_model, numFolds = 3, seed = 1, class = TRUE)
e3

WOW("SMO")
svm <- make_Weka_classifier("weka/classifiers/functions/SMO")
svm_model=svm(label~., data=trainset)
e4 <- evaluate_Weka_classifier(svm_model, numFolds = 3, seed = 1, class = TRUE)
e4

WOW("weka/classifiers/trees/RandomForest")
rf <- make_Weka_classifier("weka/classifiers/trees/RandomForest")
# build default model with 100 trees
rf_model=rf(label~., data=trainset)
# build a model with 10 trees instead
rf_model=rf(label~., data=trainset, control=Weka_control(I=10))
e5 <- evaluate_Weka_classifier(rf_model, numFolds = 3, seed = 1, class = TRUE)
e5

# submit to Kaggle
# first use textwrangler to insert "?," to each row, change "?," in the first row to "label,"
# create test ids
testid = seq(1, 28000, by=1)
# apply model to all test data
pred=predict(rf_model, newdata = testset, type = c("class"))
pred=predict(rf_model, newdata = testset)
newpred=cbind(testid, pred)
colnames(newpred)=c("ImageId", "Label")
write.csv(newpred, file="/Users/byu/Desktop/Data/digit-RF-pred.csv", row.names=FALSE)
