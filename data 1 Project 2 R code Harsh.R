GermanCredit<-read.csv("'E:/AML - BUAN 6341/GermanCredit.csv")
GermanCredit$X<-NULL
#splitting the data into train and test data
set.seed(42)
sample_size <- floor(0.70* nrow(GermanCredit))
train_index <- sample(seq_len(nrow(GermanCredit)),size = sample_size, replace = FALSE)

train_credit <- GermanCredit[train_index,]
test_credit <- GermanCredit[-train_index,]
summary(GermanCredit$Class)
#using the train data to train SVM
install.packages("caret")
library(caret)
library(pROC)
library(kernlab)
set.seed(42)
ctrl_linear<- trainControl(method = "cv",savePred = T,classProb = T,summaryFunction = twoClassSummary)
SVMlinearmod <- train(Class~.,data=train_credit,method = "svmLinear", trControl = ctrl_linear,metric= "ROC", tuneLength = 5)
SVMlinearmod
#tuning the model by running it for some values near the calculated value
set.seed(42)
grid_linear<-expand.grid(C=c(0.01,0.1,0.25,0.50,0.75,1,1.25,1.50,1.75,2))
SVMlinearmod_tune <- train(Class~.,data=train_credit,method = "svmLinear", trControl = ctrl_linear,metric= "ROC", tuneGrid = grid_linear)
SVMlinearmod_tune
plot(SVMlinearmod_tune)
SVMlinear <- SVMlinearmod_tune$results
prediction_data_linear <- SVMlinearmod_tune$pred
plot(roc(predictor = SVMlinearmod_tune$pred$Good, response = SVMlinearmod_tune$pred$obs))
test_result_svmlinear <- predict(SVMlinearmod_tune,newdata= test_credit)
confusionMatrix(test_result_svmlinear,test_credit$Class)

set.seed(42)
ctrl_poly <- trainControl(method = "cv",savePred = T,classProb = T,summaryFunction = twoClassSummary)
SVMpolymod <- train(Class~.,data=train_credit,method = "svmPoly",trControl = ctrl_poly,tuneLength = 5, metric = "ROC")
SVMpolymod
grid_poly<- expand.grid(degree = c(1,2,3,4),C = c(0.25,0.50,0.75,1,1.25,1.50),scale = c(0,0.005,0.01,0.02,0.03))
set.seed(42)
SVMpolymod_tune <- train(Class~.,data=train_credit,method = "svmPoly",trControl = ctrl_poly,tuneGrid = grid_poly, metric = "ROC")
SVMpolymod_tune
plot(SVMpolymod_tune)
prediction_data_poly <- SVMpolymod_tune$pred
plot(roc(predictor = SVMpolymod_tune$pred$Good, response = SVMpolymod_tune$pred$obs))
SVMpolyROCdata<-SVMpolymod_tune$results
test_result_svmpoly <- predict(SVMpolymod_tune,newdata = test_credit)
confusionMatrix(test_result_svmpoly,test_credit$Class)

set.seed(42)
ctrl_Radial <- trainControl(method = "cv",savePred  =T , classProb = T,summaryFunction = twoClassSummary)
SVMRadialmod <- train(Class~.,data= train_credit,method = "svmRadial",trControl = ctrl_Radial,
                      tuneLength = 5, metric = "ROC")
SVMRadialmod
grid_radial <-expand.grid(sigma = c(0.005,0.010,0.015,0.020,0.025), C= c(0.15,0.20,0.25,0.50,0.75,1,1.25,1.50))
set.seed(42)
SVMRadialmod_tune <- train(Class~.,data= train_credit,method = "svmRadial",trControl = ctrl_Radial,
                      tuneGrid = grid_radial, metric = "ROC")
SVMRadialmod_tune
plot(SVMRadialmod_tune)
prediction_data_Radial <- SVMRadialmod_tune$pred
plot(roc(predictor = SVMRadialmod_tune$pred$Good,response = SVMRadialmod_tune$pred$obs))
SVMRadialmod_results <- SVMRadialmod_tune$results
test_result_svmradial <- predict(SVMRadialmod_tune,newdata = test_credit)
confusionMatrix(test_result_svmradial,test_credit$Class)

#Since the seed was set to the same value before training each model we can compare the ROC values for the three kernels
rvalues <- resamples(list(svm=SVMlinearmod_tune,SVMpolymod_tune,SVMRadialmod_tune))
summary(rvalues)
bwplot(rvalues,metric = "ROC",ylab=c("radial","polynomial","linear"))

#plot of validation ROC shows clearly that the SVM linear is  the best model in our case for the given data
plot(roc(predictor = SVMlinearmod_tune$pred$Good, response = SVMlinearmod_tune$pred$obs))
lines(roc(predictor = SVMpolymod_tune$pred$Good, response = SVMpolymod_tune$pred$obs),col="green")
lines(roc(predictor = SVMRadialmod_tune$pred$Good,response = SVMRadialmod_tune$pred$obs),col="red")


#implementing decision tree on the dataset with pruning done using the complexity parameter
install.packages("rpart.plot")
library(rpart.plot)
ctrl_decisiontree <- trainControl(method = "cv",savePred = T, classProb = T,summaryFunction = twoClassSummary)
set.seed(42)
grid_unpruned <- expand.grid(cp=0)
dtree_unpruned <- train(Class~.,data = train_credit,method = "rpart",trControl = ctrl_decisiontree,tuneGrid = grid_unpruned,parms = list(split= "information"),metric = "ROC")
dtree_unpruned
prp(dtree_unpruned$finalModel,box.palette = "Reds",tweak = 1.2)
set.seed(42)
dtree <- train(Class~.,data = train_credit,method = "rpart",trControl = ctrl_decisiontree,tuneLength = 10,parms = list(split= "information"),metric = "ROC")
dtree
plot(dtree)
prp(dtree$finalModel, box.palette = "Reds", tweak = 1.2)
#tuning the model for the parameter cp (Complexity Parameter)
set.seed(42)
grid_dtree <- expand.grid(cp = c(0,0.005,0.01,0.015,0.020))
dtree_tune <- train(Class~.,data = train_credit,method = "rpart",trControl = ctrl_decisiontree,tuneGrid = grid_dtree,parms = list(split= "information"),metric = "ROC")
dtree_tune
plot(dtree_tune)
prp(dtree_tune$finalModel,box.palette = "Reds",tweak = 1.2)
prediction_data_dtree<- dtree_tune$pred
dtree_results<-dtree_tune$results
plot(roc(predictor = dtree_tune$pred$Good,response = dtree_tune$pred$obs))
test_result_dtree <- predict(dtree_tune,newdata = test_credit)
confusionMatrix(test_result_dtree,test_credit$Class)
#tree is alreadypruned for the complexity parameter cp using this way of implementation
install.packages("xgboost")
library(xgboost)
ctrl_xgbtree <- trainControl(method = "cv",savePred = T, classProb = T,summaryFunction = twoClassSummary,verboseIter = T)
#creating the unpruned collection of trees for the xgboost algorihtm
set.seed(42)
xgbtree <- train(Class~.,data = train_credit,method = "xgbTree",trControl = ctrl_xgbtree,tuneLength = 3,metric = "ROC")
xgbtree
plot(xgbtree)
#tuning the model for the parameter cp (Complexity Parameter)
set.seed(42)
ctrl_xgbtree1 <- trainControl(method = "cv",savePred = T, classProb = T,summaryFunction = twoClassSummary,verboseIter = T,number =3)
grid_xgbtree <- expand.grid(nrounds = c(100,150,200),max_depth=c(2,3,4),eta = c(0.3,0.4,0.5),gamma=c(0,1,2),colsample_bytree=c(0.7,0.8,0.9),min_child_weight=c(0,1,2),subsample=c(0.70,0.75,0.80))
xgbtree_tune <- train(Class~.,data = train_credit,method = "xgbTree",trControl = ctrl_xgbtree1,tuneGrid = grid_xgbtree,metric = "ROC")
xgbtree_tune
plot(xgbtree_tune)
prediction_data_xgbtree<- xgbtree_tune$pred
xgbtree_results<-xgbtree_tune$results
XgbROCdata_sampleindex<- sample(seq_len(nrow(prediction_data_xgbtree)),size =50000,replace = FALSE)
xgbplotdata<- as.data.frame(prediction_data_xgbtree[XgbROCdata_sampleindex,])
plot(roc(predictor = xgbplotdata$Good,response = xgbplotdata$obs))
test_result_xgbtree <- predict(xgbtree_tune,newdata = test_credit)
confusionMatrix(test_result_xgbtree,test_credit$Class)

#plot of validation ROC shows clearly that the SVM linear is  the best model in our case for the given data
plot(roc(predictor = SVMlinearmod_tune$pred$Good, response = SVMlinearmod_tune$pred$obs))
lines(roc(predictor = SVMpolymod_tune$pred$Good, response = SVMpolymod_tune$pred$obs),col="green")
lines(roc(predictor = SVMRadialmod_tune$pred$Good,response = SVMRadialmod_tune$pred$obs),col="red")
lines(roc(predictor = dtree_tune$pred$Good,response = dtree_tune$pred$obs),col="blue")
lines(roc(predictor = xgbplotdata$Good,response = xgbplotdata$obs),col="orange")

