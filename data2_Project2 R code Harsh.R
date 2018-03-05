#data <- read.csv("D:/Semester 4/Applied Machine Learning/Project 2/online news popularity/OnlineNewsPopularity/OnlineNewsPopularity.csv")
#data$Class[data$shares >=1400]<-"Popular"
#data$Class[data$shares <1400]<-"NotPopular"
#Reducing the data to make the processing faster
#sample<- floor(0.25*nrow(data))
#mytrain<- sample(seq_len(nrow(data)),size=sample,replace= FALSE)
#data1<- data[mytrain,]
data1<-read.csv("'E:/AML - BUAN 6341/onlinepopular.csv")
data1$X<-NULL
data1$url <- NULL
data1$timedelta<-NULL
data1$shares<- NULL
data1$num_self_hrefs<-NULL
data1$weekday_is_monday<-NULL
data1$weekday_is_tuesday<-NULL
data1$weekday_is_wednesday<-NULL
data1$weekday_is_thursday<-NULL
data1$weekday_is_friday<-NULL
data1$weekday_is_saturday<-NULL
data1$weekday_is_sunday<-NULL
data1$Class<- as.factor(data1$Class)
#splitting the dataset into train and test (70:30)
set.seed(42)
sample_size <- floor(0.70* nrow(data1))
train_index <- sample(seq_len(nrow(data1)),size = 7000, replace = FALSE)

train_online <- data1[train_index,]
test_online <- data1[-train_index,]

#using the train data to train SVM
install.packages("caret")
library(caret)
library(pROC)
library(kernlab)
set.seed(42)
ctrl_online_linear <- trainControl(method = "cv",savePred = T,classProb = T,summaryFunction = twoClassSummary,verbose= TRUE,number =3)
SVMlinearmod_online <- train(Class~.,data=train_online,method="svmLinear",trControl = ctrl_online_linear,metric ="ROC",tunelength = 3)
SVMlinearmod_online
#tuning the model for better fit based on the tuning parameter
grid_online_linear<- expand.grid(C=c(0.5,1,1.5))
set.seed(42)
SVMlinearmod_online_tune<- train(Class~.,data=train_online,method="svmLinear",trControl=ctrl_online_linear,metric="ROC",tuneGrid=grid_online_linear)
SVMlinearmod_online_tune
815SVMlinear_online<- SVMlinearmod_online_tune$results
prediction_data_online_linear<-SVMlinearmod_online_tune$pred
roc_online_linear<-roc(predictor=SVMlinearmod_online_tune$pred$Popular,response = SVMlinearmod_online_tune$pred$obs)
plot(roc_online_linear)
test_result_svmlinear_online <- predict(SVMlinearmod_online_tune,newdata=test_online)
confusionMatrix(test_result_svmlinear_online,test_online$Class)

#training the Poly SVM kernel 
set.seed(42)
ctrl_online_poly<- trainControl(method="cv",savePred=T,classProb = T,summaryFunction=twoClassSummary,verboseIter=T,number =3)
SVMpolymod_online<- train(Class~.,data=train_online,method="svmPoly",trControl=ctrl_online_poly,tuneLength=3,metric="ROC")
SVMpolymod_online
grid_online_poly<- expand.grid(degree=c(1,2,3),C=c(1),scale = c(0,0.005,0.01,0.02,0.03))
set.seed(42)
SVMpolymod_online_tune<- train(Class~.,data=train_online,method="svmPoly",trControl=ctrl_online_poly,tuneGrid=grid_online_poly,metric="ROC")
SVMpolymod_online_tune
prediction_data_online_poly<-SVMpolymod_online_tune$pred
roc_online_poly<- roc(predictor=SVMpolymod_online_tune$pred$Popular,response=SVMpolymod_online_tune$pred$obs)
plot(roc_online_poly)
SVMpoly_online<- SVMpolymod_online_tune$results
test_result_svmpoly_online<-predict(SVMpolymod_online_tune,newdata=test_online)
confusionMatrix(test_result_svmpoly_online,test_online$Class)

#training the SVM Radial kernel
set.seed(42)
ctrl_online_Radial <- trainControl(method = "cv",savePred  =T , classProb = T,summaryFunction = twoClassSummary,verbose=TRUE,number=3)
SVMRadialmod_online <- train(Class~.,data= train_online,method = "svmRadial",trControl = ctrl_online_Radial,
                      tuneLength = 3, metric = "ROC")
SVMRadialmod_online
grid_online_radial <-expand.grid(sigma = c(0.013,0.014,0.015), C= c(1,2,3))
set.seed(42)
SVMRadialmod_online_tune <- train(Class~.,data= train_online,method = "svmRadial",trControl = ctrl_online_Radial,
                           tuneGrid = grid_online_radial, metric = "ROC")
SVMRadialmod_online_tune
prediction_data_online_Radial <- SVMRadialmod_online_tune$pred
roc_online_radial<-roc(predictor = SVMRadialmod_online_tune$pred$Popular,response = SVMRadialmod_online_tune$pred$obs)
plot(roc_online_radial)
SVMRadial_online <- SVMRadialmod_online_tune$results
test_result_svmradial_online <- predict(SVMRadialmod_online_tune,newdata = test_online)
confusionMatrix(test_result_svmradial_online,test_online$Class)

#Since the seed was set to the same value before training each model we can compare the ROC values for the three kernels
rvalues <- resamples(list(svm=SVMlinearmod_online_tune,SVMpolymod_online_tune,SVMRadialmod_online_tune))
summary(rvalues)
bwplot(rvalues,metric = "ROC",ylab=c("radial","polynomial","linear"))

#plot of validation ROC shows clearly that the SVM linear is  the best model in our case for the given data
plot(roc_online_linear)
lines(roc_online_poly,col="green")
lines(roc_online_radial,col="red")

#implementing the decision tree on the dataset with pruning done using the complexity parameter
install.packages("rpart.plot")
library(rpart.plot)
ctrl_decisiontree_online <- trainControl(method = "cv",savePred = T, classProb = T,summaryFunction = twoClassSummary)
set.seed(42)
grid_unpruned <- expand.grid(cp=0)
dtree_unpruned_online <- train(Class~.,data = train_online,method = "rpart",trControl = ctrl_decisiontree_online,tuneGrid = grid_unpruned,parms = list(split= "information"),metric = "ROC")
dtree_unpruned_online
prp(dtree_unpruned_online$finalModel,box.palette = "Reds",tweak = 1.2)
set.seed(42)
dtree_online <- train(Class~.,data = train_online,method = "rpart",trControl = ctrl_decisiontree_online,tuneLength = 7,parms = list(split= "information"),metric = "ROC")
dtree_online
plot(dtree_online)
prp(dtree_online$finalModel, box.palette = "Reds", tweak = 1.2)
#tuning the model for the parameter cp (Complexity Parameter)
set.seed(42)
grid_dtree_online <- expand.grid(cp = c(0,0.005,0.010))
dtree_tune_online <- train(Class~.,data = train_online,method = "rpart",trControl = ctrl_decisiontree_online,tuneGrid = grid_dtree_online,parms = list(split= "information"),metric = "ROC")
dtree_tune_online
plot(dtree_tune_online)
prp(dtree_tune_online$finalModel,box.palette = "Reds",tweak = 1.2)
prediction_data_dtree<- dtree_tune_online$pred
dtree_results<-dtree_tune_online$results
roc_dtree_online<-roc(predictor = dtree_tune_online$pred$Popular,response = dtree_tune_online$pred$obs)
plot(roc_dtree_online)
test_result_dtree_online <- predict(dtree_tune_online,newdata = test_online)
confusionMatrix(test_result_dtree_online,test_online$Class)

#implementing xgbtree algorithm on the train data
install.packages("xgboost")
library(xgboost)
ctrl_xgbtree_online <- trainControl(method = "cv",savePred = T, classProb = T,summaryFunction = twoClassSummary,verboseIter = T,number =3)
#creating the unpruned collection of trees for the xgboost algorihtm
set.seed(42)
xgbtree_online <- train(Class~.,data = train_online,method = "xgbTree",trControl = ctrl_xgbtree_online,tuneLength = 3,metric = "ROC")
xgbtree_online
plot(xgbtree_online)
#tuning the model for the parameter cp (Complexity Parameter)
set.seed(42)
ctrl_xgbtree1_online <- trainControl(method = "cv",savePred = T, classProb = T,summaryFunction = twoClassSummary,verboseIter = T,number =3)
grid_xgbtree_online <- expand.grid(nrounds = c(100,150,200),max_depth=c(1,2,3),eta = c(0.2,0.3,0.4),gamma=c(0,1,2),colsample_bytree=c(0.5,0.6,0.7),min_child_weight=c(0,1,2),subsample=c(0,0.5,1))
xgbtree_tune_online <- train(Class~.,data = train_online,method = "xgbTree",trControl = ctrl_xgbtree1_online,tuneGrid = grid_xgbtree_online,metric = "ROC")
xgbtree_tune_online
plot(xgbtree_tune_online)
prediction_data_xgbtree_online<- xgbtree_tune_online$pred
xgbtree_results_online<-xgbtree_tune_online$results
XgbROCdata_sampleindex<- sample(seq_len(nrow(prediction_data_xgbtree_online)),size =50000,replace = FALSE)
xgbplotdata<- as.data.frame(prediction_data_xgbtree_online[XgbROCdata_sampleindex,])
roc_xgbtree_online<-roc(predictor = xgbplotdata$Popular,response = xgbplotdata$obs)
plot(roc_xgbtree_online)
test_result_xgbtree_online <- predict(xgbtree_tune_online,newdata = test_online)
confusionMatrix(test_result_xgbtree_online,test_online$Class)

#plot of validation ROC shows clearly that the SVM linear is  the best model in our case for the given data
plot(roc_online_linear)
lines(roc_online_poly,col="green")
lines(roc_online_radial,col="red")
lines(roc_dtree_online,col="blue")
lines(roc_xgbtree_online,col="orange")
