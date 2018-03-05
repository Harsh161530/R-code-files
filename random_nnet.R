# combining feature selection data with response variable
response<-train_processed$y
typeof(random_comp)
random_comp<-as.data.frame(random_comp)
random_nnet<-cbind(random_comp,response)
View(random_nnet)
str(random_nnet)


#creating data partition
index<-createDataPartition(random_nnet$response,p=0.8)[[1]]
train_data<-random_nnet[index,]
test_data<-random_nnet[-index,]


response<-"response"
predictors<-setdiff(names(train_data),response)
predictors


# training, validation and test split
train_h20<-as.h2o(train_data)
train_split <- h2o.splitFrame(data=train_h20, ratios=0.9)
train_h <- train_split[[1]]
valid_h<-train_split[[2]]
test_h<-as.h2o(test_data)


##### random parameter search #####
hyper_params <- list(
  l1=seq(0,1e-4),
  #l2=seq(0,1e-4,1e-6),
  rate=c(0.005),
  rate_annealing=c(1e-8,1e-7),
  epochs=c(500,100),
  hidden=list(c(20),c(20,10),c(20,10,5))
)


nnet2_random <- h2o.grid(
  algorithm="deeplearning",
  search_criteria=list(strategy="RandomDiscrete",max_models=200,stopping_rounds=3,stopping_metric="misclassification",stopping_tolerance=0.0001),
  grid_id="nnet2_rand_grid2", 
  model_id="nnet_rand_model_2", 
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


model_list <- h2o.getGrid("nnet2_rand_grid2",sort_by="auc",decreasing=TRUE)
model_list

## To see what other "sort_by" criteria are allowed
#grid <- h2o.getGrid("dl_grid",sort_by="wrong_thing",decreasing=FALSE)

## Sort by logloss
h2o.getGrid("nnet2_rand_grid2",sort_by="logloss",decreasing=FALSE)

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
