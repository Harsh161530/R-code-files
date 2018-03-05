### Author: Abhishek M
rm(list=ls())
set.seed(1)
library(caret)
library(data.table)
library(h2o)
library(ROSE)

data("GermanCredit")
View(GermanCredit)

#missing values
sum(is.na(GermanCredit))

#near zero variance features
zerovarCol<- nearZeroVar(GermanCredit)
zerovarCol

#removing near zero var features
GermanCredit<-GermanCredit[,-nearZeroVar(GermanCredit)]

#Since the level variables right now have linear dependendicies we need to get rid of 1 of them for effective dummy coding
GermanCredit$CheckingAccountStatus.lt.0 <- NULL
GermanCredit$SavingsAccountBonds.lt.100 <- NULL
GermanCredit$EmploymentDuration.lt.1 <- NULL
GermanCredit$EmploymentDuration.Unemployed <- NULL
GermanCredit$Personal.Male.Married.Widowed <- NULL
GermanCredit$Property.Unknown <- NULL
GermanCredit$Housing.ForFree <- NULL

round(prop.table(table(GermanCredit$Class))*100)

preProcValues <- preProcess(GermanCredit[,c(1:7)], method = c("center","scale"))
train_processed <- predict(preProcValues, GermanCredit)
View(train_processed)
#creating data partition
index<-createDataPartition(train_processed$Class,p=0.9)[[1]]


train_data<-train_processed[index,]
test_data<-train_processed[-index,]
View(test_data)
#balancing classes
#library(DMwR)
#smote_train <- SMOTE(Class ~ ., data  = train_data)
#dim(smote_train)
#round(prop.table(table(smote_train$Class))*100)
#str(smote_train)
#dim(test_data)
#round(prop.table(table(test_data$Class))*100)
###
response<-"Class"
predictors<-setdiff(names(train_data),response)
predictors

##initializaing H20
h2o.init(nthreads=-1, max_mem_size="6G")

train_h20<-as.h2o(train_data)
train_split <- h2o.splitFrame(data=train_h20, ratios=0.9)
train_h <- train_split[[1]]
valid_h<-train_split[[2]]
test_h<-as.h2o(test_data)

nnet1 <- h2o.deeplearning(
  model_id="nnet_model_1", 
  training_frame=train_h, 
 validation_frame=valid_h,
  x=predictors,
  y=response,
  hidden=c(20,10,5),                  ## small network, runs faster
  epochs=10000,                      ## hopefully converges earlier...
 # score_validation_samples=500,      ## sample the validation dataset (faster)
  stopping_rounds=5,
  stopping_metric="misclassification", ## could be "MSE","logloss","r2"
  stopping_tolerance=0.0001,
  variable_importances = T,
 standardize = FALSE,
 adaptive_rate = T,
  #balance_classes = T
  nfolds=5,
  fold_assignment = "Stratified",
 max_w2 = 10
)
summary(nnet1)
plot(nnet1)
plot(h2o.performance(nnet1,valid=T))
train_perf<-h2o.performance(nnet1,valid=T)
train_perf@metrics$AUC
test_perf<-predict(nnet1, test_h)
test_perf_check<-h2o.performance(nnet1,test_h)
confusion_matrix_1<-h2o.confusionMatrix(test_perf_check)
print(confusion_matrix_1)
plot(test_perf_check,type="roc")
h2o.auc(test_perf_check)


######nnet model2#####
hyper_params <- list(
  l1=seq(0,1e-4,1e-2),
  #l2=seq(0,1e-4,1e-6),
  rate=c(0.1,0.01,0.03,0.005),
  rate_annealing=c(1e-8,1e-7,1e-6),
  epochs=c(500,100),
  hidden=list(c(20),c(40,20,10,5,2),c(20,10),c(20,10,5))
)



nnet2 <- h2o.grid(
  algorithm="deeplearning",
 search_criteria=list(strategy="RandomDiscrete",max_models=200,stopping_rounds=3,stopping_metric="misclassification",stopping_tolerance=0.0001),
  grid_id="nnet2_grid2", 
  model_id="nnet_model_2", 
  training_frame=train_h, 
  validation_frame=valid_h,
  x=predictors,
  y=response,
 stopping_rounds=3,
 stopping_metric="misclassification", ## could be "MSE","logloss","r2"
  stopping_tolerance=0.0001,
  nfolds=3,
  fold_assignment = "Stratified",
 activation=c("Rectifier"),
  hyper_params=hyper_params,
  standardize=FALSE,
 adaptive_rate=F,                ## manually tuned learning rate
 momentum_start=0.5,             ## manually tuned momentum
 momentum_stable=0.9, 
 momentum_ramp=1e7,
 classification_stop=-1
)
model_list <- h2o.getGrid("nnet2_grid2",sort_by="auc",decreasing=TRUE)
model_list

## To see what other "sort_by" criteria are allowed
#grid <- h2o.getGrid("dl_grid",sort_by="wrong_thing",decreasing=FALSE)

## Sort by logloss
h2o.getGrid("nnet2_grid2",sort_by="logloss",decreasing=FALSE)

## Find the best model and its full set of parameters
model_list@summary_table[1,]
best_model <- h2o.getModel(model_list@model_ids[[1]])
summary(best_model)
plot(best_model)
plot(h2o.performance(best_model))
test_perf_bm<-predict(best_model, test_h)
test_perf_check_bm<-h2o.performance(best_model,test_h)
confusion_matrix_bm<-h2o.confusionMatrix(test_perf_check_bm)
print(confusion_matrix_bm)
plot(test_perf_check_bm,type="roc")
h2o.auc(test_perf_check_bm)

###maxout implementation###
maxout_nnet1 <- h2o.deeplearning(
  model_id="nnet_maxout_1",
  training_frame=train_h,
  validation_frame=valid_h,
  x=predictors,
  y=response,
  hidden=c(40,20,10,5,2),                  ## small network, runs faster
  epochs=10000,                      ## hopefully converges earlier...
  # score_validation_samples=500,      ## sample the validation dataset (faster)
  stopping_rounds=5,
  stopping_metric="misclassification", ## could be "MSE","logloss","r2"
  stopping_tolerance=0.0001,
  variable_importances = T,
  standardize = FALSE,
  activation=c("Maxout"),
  adaptive_rate=F,                ## manually tuned learning rate
  momentum_start=0.5,             ## manually tuned momentum
  momentum_stable=0.99,
  momentum_ramp=1e2,
  rate=0.0005,
  rate_annealing = 1e-7
  #balance_classes = T
  #nfolds=5,
  #fold_assignment = "Stratified"
)
 
 
summary(maxout_nnet1)
plot(maxout_nnet1)
plot(h2o.performance(maxout_nnet1))
test_perf_maxout<-predict(maxout_nnet1, test_h)
test_perf_check_maxout<-h2o.performance(maxout_nnet1,test_h)
confusion_matrix_maxout1<-h2o.confusionMatrix(test_perf_check_maxout)
print(confusion_matrix_maxout1)
plot(test_perf_check_maxout,type="roc")
h2o.auc(test_perf_check_maxout)

