#created the dataset names data1617 having the participant ids of the people who participated in both years
#Task1: reducing the columns that we want to use in the feature selection model
#removing irrelevant columns from the dataset

data_1617$`16_City`<-NULL
data_1617$`17_City`<-NULL
data_1617$`16_Company_Goal`<-NULL
data_1617$`17_Company_Goal`<-NULL
data_1617$`16_Company_Name`<-NULL
data_1617$`17_Company_Name`<-NULL
data_1617$`16_Event_Date`<-NULL
data_1617$`17_Event_Date`<-NULL
data_1617$`16_Event_Year`<-NULL
data_1617$`17_Event_Year`<-NULL
data_1617$`16_Name`<-NULL
data_1617$`17_Name`<-NULL
data_1617$`16_Participant_Id`<-NULL
data_1617$`17_Participant_Id`<-NULL
data_1617$`16_Fundraising_Goal`<-NULL
data_1617$`17_Fundraising_Goal`<-NULL
data_1617$`16_Street`<-NULL
data_1617$`17_Street`<-NULL
data_1617$`16_Team_Average`<-NULL
data_1617$`17_Team_Average`<-NULL
data_1617$`16_Team_Captain`<-NULL
data_1617$`17_Team_Captain`<-NULL
data_1617$`16_Team_Id`<-NULL
data_1617$`17_Team_Id`<-NULL
data_1617$`16_Team_Name`<-NULL
data_1617$`17_Team_Name`<-NULL
data_1617$`16_Team_Total_Gifts`<-NULL
data_1617$`17_Team_Total_Gifts`<-NULL
data_1617$`16_Total_Gifts`<-NULL
data_1617$`17_Total_Gifts`<-NULL
data_1617$`16_Total_Gifts_num`<-NULL
data_1617$`17_Total_Gifts_num`<-NULL
data_1617$`16_Zip`<-NULL
data_1617$`17_Zip`<-NULL
data_1617$`16_Participant_Gifts`<-NULL
data_1617$`17_Participant_Gifts`<-NULL
data_1617$`16_Participant_Gifts_num`<-NULL
data_1617$`17_Participant_Gifts_num`<-NULL
data_1617$`16_Personal_Gift`<-NULL
data_1617$`17_Personal_Gift`<-NULL
data_1617$`16_Personal_Gift_num`<-NULL
data_1617$`17_Personal_Gift_num`<-NULL
data_1617$`16_Team_Total_Gifts_num`<-NULL
data_1617$`17_Team_Total_Gifts_num`<-NULL
data_1617$`17_Team_Member_Goal`<-NULL
data_1617$`16_Team_Member_Goal`<-NULL
data_1617$`16_Team_Member_Goal_num`<-NULL
data_1617$`17_Team_Member_Goal_num`<-NULL
data_1617$`16_Team_Average_num`<-NULL
data_1617$`17_Team_Average_num`<-NULL

data_1617$state_match[data_1617$`16_State` == data_1617$`17_State`] <-TRUE
data_1617$state_match[data_1617$`16_State` != data_1617$`17_State`] <-FALSE
summary(data_1617$state_match)
data_1617$`16_State`<-NULL
data_1617$state_match<-NULL
data_1617$`17_State`<-NULL

data_1617$match_code[data_1617$`16_MATCH_CODE` == data_1617$`17_MATCH_CODE`]<-TRUE
data_1617$match_code[data_1617$`16_MATCH_CODE` != data_1617$`17_MATCH_CODE`]<-FALSE
summary(data_1617$match_code)
data_1617$`16_MATCH_CODE`<-NULL
data_1617$match_code<-NULL

data_1617$tap_lvl[data_1617$`16_TAP_LEVEL` == data_1617$`17_TAP_LEVEL`]<-TRUE
data_1617$tap_lvl[data_1617$`16_TAP_LEVEL` != data_1617$`17_TAP_LEVEL`]<-FALSE
summary(data_1617$tap_lvl)
data_1617$tap_lvl<-NULL
data_1617$`16_TAP_LEVEL`<-NULL

data_1617$tap_desc[data_1617$`16_TAP_DESC` == data_1617$`17_TAP_DESC`]<-TRUE
data_1617$tap_desc[data_1617$`16_TAP_DESC`!= data_1617$`17_TAP_DESC`]<-FALSE
summary(data_1617$tap_desc)
data_1617$`16_TAP_DESC`<-NULL
data_1617$tap_desc<-NULL

data_1617$tap_lifed[data_1617$`16_TAP_LIFED` == data_1617$`17_TAP_LIFED`]<-TRUE
data_1617$tap_lifed[data_1617$`16_TAP_LIFED` != data_1617$`17_TAP_LIFED`]<-FALSE
summary(data_1617$tap_lifed)
data_1617$tap_lifed<-NULL
data_1617$`16_TAP_LIFED`<-NULL

data_1617$medage[data_1617$`16_MEDAGE_CY` == data_1617$`17_MEDAGE_CY`]<-TRUE
data_1617$medage[data_1617$`16_MEDAGE_CY` != data_1617$`17_MEDAGE_CY`]<-FALSE
summary(data_1617$medage)
data_1617$medage<-NULL
data_1617$`16_MEDAGE_CY`<-NULL

data_1617$divin[data_1617$`16_DIVINDX_CY` == data_1617$`17_DIVINDX_CY`]<-TRUE
data_1617$divin[data_1617$`16_DIVINDX_CY` != data_1617$`17_DIVINDX_CY`]<-FALSE
summary(data_1617$divin)
data_1617$`16_DIVINDX_CY`<-NULL
data_1617$divin<-NULL

data_1617$medhinc[data_1617$`16_MEDHINC_CY` == data_1617$`17_MEDHINC_CY`]<-TRUE
data_1617$medhinc[data_1617$`16_MEDHINC_CY` != data_1617$`17_MEDHINC_CY`]<-FALSE
summary(data_1617$medhinc)
data_1617$medhinc<-NULL
data_1617$`16_MEDHINC_CY`<-NULL

data_1617$meddi[data_1617$`16_MEDDI_CY` == data_1617$`17_MEDDI_CY`]<-TRUE
data_1617$meddi[data_1617$`16_MEDDI_CY` != data_1617$`17_MEDDI_CY`]<-FALSE
summary(data_1617$meddi)
data_1617$meddi<-NULL
data_1617$`16_MEDDI_CY`<-NULL

data_1617$mednw[data_1617$`16_MEDNW_CY` == data_1617$`17_MEDNW_CY`]<-TRUE
data_1617$mednw[data_1617$`16_MEDNW_CY` != data_1617$`17_MEDNW_CY`]<-FALSE
summary(data_1617$mednw)
data_1617$mednw<-NULL
data_1617$`16_MEDNW_CY`<-NULL

data_1617$reg[data_1617$`16_Registration_Gift` == data_1617$`17_Registration_Gift`]<-TRUE
data_1617$reg[data_1617$`16_Registration_Gift` != data_1617$`17_Registration_Gift`]<-FALSE
summary(data_1617$reg)
data_1617$reg<-NULL

data_1617$top[data_1617$`16_Top_walker` == data_1617$`17_Top_walker`]<-TRUE
data_1617$top[data_1617$`16_Top_walker` != data_1617$`17_Top_walker`]<-FALSE
summary(data_1617$top)
summary(data_1617$`16_Top_walker`)#summary of number of top walkers in the year 2016
summary(data_1617$`17_Top_walker`)#summary of number of top walkers in the year 2017
#the count of top walkers decrease by 215 from year 2016 to 2017 for the repeated participants
data_1617$top<-NULL

library(Boruta)
set.seed(1)
Feature_selection1617<-Boruta(`17_Top_walker`~.,data=data_1617,maxRuns = 20,doTrace =2,getImp = getImpRfZ)
Feature_selection1617$finalDecision
summary(Feature_selection1617$finalDecision)

#plot of feature selection importance based on random forest application
plot(Feature_selection1617, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(Feature_selection1617$ImpHistory),function(i)
  Feature_selection1617$ImpHistory[is.finite(Feature_selection1617$ImpHistory[,i]),i])
names(lz) <- colnames(Feature_selection1617$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),at = 1:ncol(Feature_selection1617$ImpHistory), cex.axis = 0.7)


#training a neural network on the 2016 2017 data
#scaling the numerical variables.
data_1617$`16_Team_Count`<-scale(data_1617$`16_Team_Count`,center = TRUE,scale = TRUE)
data_1617$`16_Gifts_Count`<-scale(data_1617$`16_Gifts_Count`,center = TRUE, scale = TRUE)
data_1617$`16_Fundraising_Goal_num`<-scale(data_1617$`16_Fundraising_Goal_num`,center = TRUE,scale = TRUE)
data_1617$`17_Team_Count`<-scale(data_1617$`17_Team_Count`,center = TRUE,scale = TRUE)
data_1617$`17_Gifts_Count`<-scale(data_1617$`17_Gifts_Count`,center = TRUE, scale = TRUE)
data_1617$`17_MEDAGE_CY`<-scale(data_1617$`17_MEDAGE_CY`,center = TRUE,scale = TRUE)
data_1617$`17_DIVINDX_CY`<-scale(data_1617$`17_DIVINDX_CY`,center = TRUE, scale = TRUE)
data_1617$`17_MEDHINC_CY`<-scale(data_1617$`17_MEDHINC_CY`,center = TRUE, scale = TRUE)
data_1617$`17_MEDDI_CY`<-scale(data_1617$`17_MEDDI_CY`, center = TRUE, scale = TRUE)
data_1617$`17_MEDNW_CY`<-scale(data_1617$`17_MEDNW_CY`,center = TRUE, scale =  TRUE)
data_1617$`17_Fundraising_Goal_num`<- scale(data_1617$`17_Fundraising_Goal_num`,center = TRUE,scale = TRUE)


set.seed(42)
sample_size <- floor(0.75* nrow(data_1617))
train_index <- sample(seq_len(nrow(data_1617)),size = sample_size, replace = FALSE)
train_1617 <- data_1617[train_index,]
remdata <- data_1617[-train_index,]

set.seed(42)
sample_size <- floor(0.50*nrow(remdata))
train_index <- sample(seq_len(nrow(remdata)),size = sample_size,replace = FALSE)
valid_1617 <- remdata[train_index,]
test_1617 <- remdata[-train_index,]

response<-"17_Top_walker"
predictors<-setdiff(names(data_1617),response)
install.packages("h2o")
library(h2o)
h2o.init(nthreads=-1, max_mem_size="8G")

write.csv(train_1617, file = "dat1617.csv", row.names = F, col.names = T)
train_h1617<- h2o.importFile(path = "D:/Semester 5/project 1 R files/Practicum project 1 R files/dat1617.csv")
train_h1617$`17_Top_walker`<-as.factor(train_h1617$`17_Top_walker`)
train_h1617$`16_Top_walker`<-as.factor(train_h1617$`16_Top_walker`)
#tw_valid$DIVINDX_CY<-scale(tw_valid$DIVINDX_CY,center = TRUE,scale=TRUE)
write.csv(valid_1617, file = "dat11617.csv", row.names = F, col.names = T)
valid_h1617<-h2o.importFile(path = "D:/Semester 5/project 1 R files/Practicum project 1 R files/dat11617.csv") 
valid_h1617$`17_Top_walker`<-as.factor(valid_h1617$`17_Top_walker`)
valid_h1617$`16_Top_walker`<-as.factor(valid_h1617$`16_Top_walker`)
#tw_test$DIVINDX_CY<-scale(tw_test$DIVINDX_CY,center = TRUE,scale = TRUE)
write.csv(test_1617, file = "dat21617.csv", row.names = F, col.names = T)
test_h1617<-h2o.importFile(path = "D:/Semester 5/project 1 R files/Practicum project 1 R files/dat21617.csv") 
test_h1617$`17_Top_walker`<-as.factor(test_h1617$`17_Top_walker`)
test_h1617$`16_Top_walker`<-as.factor(test_h1617$`16_Top_walker`)

#neural net model for featrure selection dataset
hyper_params <- list(
  #l1=seq(0,1e-3,1e-1),
  l2=seq(0,1e-4,1e-6),
  rate=c(0.1,0.01,0.03,0.005),
  rate_annealing=c(1e-8,1e-7,1e-6),
  epochs=c(100,200,500,1000),
  hidden=list(c(18),c(18,15,10,5,2),c(15,10),c(15,10,5),c(10,5))
)



nnet21617 <- h2o.grid(
  algorithm="deeplearning",
  search_criteria=list(strategy="RandomDiscrete",max_models=200,stopping_rounds=3,stopping_metric="misclassification",stopping_tolerance=0.0001),
  grid_id="nnet2_grid21617", 
  model_id="nnet_model_21617", 
  training_frame=train_h1617, 
  validation_frame=valid_h1617,
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
model_list1617 <- h2o.getGrid("nnet2_grid21617",sort_by="auc",decreasing=TRUE)
model_list1617

## To see what other "sort_by" criteria are allowed
#grid <- h2o.getGrid("dl_grid",sort_by="wrong_thing",decreasing=FALSE)

## Sort by logloss
h2o.getGrid("nnet2_grid21617",sort_by="logloss",decreasing=FALSE)

## Find the best model and its full set of parameters
model_list1617@summary_table[1,]
best_model1617 <- h2o.getModel(model_list1617@model_ids[[1]])
summary(best_model1617)
plot(best_model1617)
plot(h2o.performance(best_model1617))
test_perf_bm1617<-predict(best_model1617, test_h1617)
test_perf_check_bm1617<-h2o.performance(best_model1617,test_h1617)
confusion_matrix_bm1617<-h2o.confusionMatrix(test_perf_check_bm1617)
print(confusion_matrix_bm1617)
plot(test_perf_check_bm1617,type="roc")
h2o.auc(test_perf_check_bm1617)


