rm(list=ls())
library(dummies)
library(h2o)
library(data.table)
library(caret)
#setting working directory
setwd("~/Documents/JSOM/Fall'18/ML/Project3/Part2")

#set seed
set.seed(999)

# readin in the data
data<-fread('bank-additional-full.csv',stringsAsFactors = TRUE)


#one hot encoding
data_transformed<-dummy.data.frame(data,names=c("job","marital","education","default","housing","loan","contact","day_of_week","poutcome"))


#dropping the one of the dummy encoded variable to remove the linear dependency
data_transformed$jobunknown<-NULL
data_transformed$maritalunkown<-NULL
data_transformed$educationunkown<-NULL
data_transformed$defaultunkown<-NULL
data_transformed$housingunkown<-NULL
data_transformed$loanunkown<-NULL
data_transformed$day_of_weekmon<-NULL
data_transformed$poutcomenonexistent<-NULL

#some feature engineering

#duration to minutes
data_transformed$duration=data_transformed$duration/60

# discretizing duration into bins 0-2,2-5,5-10 and greater than 10
data_transformed$duration_2<-ifelse(data_transformed$duration<=2,1,0)
data_transformed$duration_5<-ifelse(data_transformed$duration>2 & data_transformed$duration<=5,1,0)
data_transformed$duration_10<-ifelse(data_transformed$duration>5 & data_transformed$duration<=10,1,0)
data_transformed$duration_10p<-ifelse(data_transformed$duration>10,1,0)

# discretizing pdays
data_transformed$pdays_0_3<-ifelse(data_transformed$pdays<=2,1,0)
data_transformed$pdays_3_5<-ifelse(data_transformed$pdays>2 & data_transformed$pdays<=5,1,0)
data_transformed$pdays_6_10<-ifelse(data_transformed$pdays>5 & data_transformed$pdays<=10,1,0)
data_transformed$pdays_10_30<-ifelse(data_transformed$pdays>10 & data_transformed$pdays<=30,1,0)
data_transformed$pdays_30_999<-ifelse(data_transformed$pdays>30,1,0)

#discretizing age
data_transformed$adult<-ifelse(data_transformed$age>=17 & data_transformed$age<=35,1,0)
data_transformed$middle_age<-ifelse(data_transformed$age>=36 & data_transformed$age<=60,1,0)
data_transformed$old<-ifelse(data_transformed$age>60,1,0)

#removing the month column
data_transformed$month<-NULL


#checking the structure of the prepared data before modeling
str(data_transformed)
colnames(data_transformed)

data_transformed$age<-NULL
data_transformed$pdays<-NULL
data_transformed$duration<-NULL


#checking near zero var columns
zerovarCol<- nearZeroVar(data_transformed)
zerovarCol
colnames(data_transformed)[zerovarCol]

#removing near zero var columns
data_transformed<-data_transformed[,-c(3,4,6,7,9,11,15,20,23,26,28,31,52,53,54,55,56,59)]
str(data_transformed)
dim(data_transformed)

# standardizing data
preProcValues <- preProcess(data_transformed[,c(32:35)], method = c("center","scale"))
train_processed <- predict(preProcValues, data_transformed)
View(train_processed)

# checking class balance
round(prop.table(table(train_processed$y))*100)


#creating data partition
index<-createDataPartition(train_processed$y,p=0.8)[[1]]
train_data<-train_processed[index,]
test_data<-train_processed[-index,]

# checking class balance in train and test as well as dimension of the datasets
dim(train_data)
round(prop.table(table(train_data$y))*100)

dim(test_data)
round(prop.table(table(test_data$y))*100)


#response and target for h2o training
response<-"y"
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
  hidden=c(41,20,10,5,2),                  ## small network, runs faster
  epochs=10000,                      ## hopefully converges earlier...
  # score_validation_samples=500,      ## sample the validation dataset (faster)
  stopping_rounds=5,
  stopping_metric="misclassification", ## could be "MSE","logloss","r2"
  stopping_tolerance=0.0001,
  variable_importances = T,
  standardize = FALSE,
  adaptive_rate = T,
  #balance_classes = T
  nfolds=3,
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


##### random parameter search #####
hyper_params <- list(
  l1=seq(0,1e-4,1e-2),
  #l2=seq(0,1e-4,1e-6),
  rate=c(0.1,0.01,0.03,0.005),
  rate_annealing=c(1e-8,1e-7,1e-6),
  epochs=c(500,100),
  hidden=list(c(20),c(41),c(41,20,10,5,2),c(20,10),c(20,10,5))
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

##maxout activation
maxout_nnet1 <- h2o.deeplearning(
  model_id="nnet_model_1", 
  training_frame=train_h, 
  validation_frame=valid_h,
  x=predictors,
  y=response,
  hidden=c(41,20,10,5,2),                  ## small network, runs faster
  epochs=10000,                      ## hopefully converges earlier...
  # score_validation_samples=500,      ## sample the validation dataset (faster)
  stopping_rounds=5,
  stopping_metric="misclassification", ## could be "MSE","logloss","r2"
  stopping_tolerance=0.0001,
  variable_importances = T,
  standardize = FALSE,
  activation = c("Maxout"),
  adaptive_rate = T,
  #balance_classes = T
  nfolds=3,
  fold_assignment = "Stratified",
  max_w2 = 10
)
summary(maxout_nnet1)
plot(maxout_nnet1)
plot(h2o.performance(maxout_nnet1,valid=T))
train_perf<-h2o.performance(maxout_nnet1,valid=T)
train_perf@metrics$AUC
test_perf<-predict(maxout_nnet1, test_h)
test_perf_check<-h2o.performance(maxout_nnet1,test_h)
confusion_matrix_1<-h2o.confusionMatrix(test_perf_check)
print(confusion_matrix_1)
plot(test_perf_check,type="roc")
h2o.auc(test_perf_check)
