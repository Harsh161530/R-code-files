library(caret)
set.seed(1)


trainX <- train_data[,colnames(train_data) != "Class"]
preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
preProcValues

#ctrl <- trainControl(method = "repeatedcv",repeats = 3)
#knn <- train(Class~.,data = train_data,method = "knn" , trControl = ctrl, preProcess = c("center","scale"), tuneLength = 50)
#knn

#plot(knn)

#knnpredict <- predict(knn,newdata = test_data)

#confusionMatrix(knnpredict,test_data$Class)

ctrl_2 <- trainControl(method = "repeatedcv",repeats = 3, classProbs = TRUE,summaryFunction = twoClassSummary)
knn1 <- train(Class~.,data = train_data,method = "knn",
              trControl = ctrl_2,
              preProcess = c("center","scale"),
              tuneLength = 50)
knn1

plot(knn1,print.thres = 0.5,type = "S") # plot for the test data

knnpredict1 <- predict(knn1,newdata = test_data)
confusionMatrix(knnpredict1,test_data$Class)

library(pROC)
knnpredict2 <- predict(knn1,newdata = test_data, type = "prob")
knnROC <- roc(test_data$Class,knnpredict2[,"Bad"],levels = levels(test_data$Class))
knnROC

plot(knnROC,col = "red")


