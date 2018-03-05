projectdata1 <- read.csv(file = "D:/Semester 5/datasets/project 1 Top Walker Analysis/LuminateDataExport_UTDP2_011818.csv")
data<- projectdata1

#converting the factor variable into nummeric for ease in analysis
data$Total_Gifts_num <-as.character(data$Total_Gifts)
data$Total_Gifts_num<-as.numeric(gsub("[\\$,]","",data$Total_Gifts_num))
#str(data$Total_Gifts_num)

data$Team_Average_num <- as.character(data$Team_Average)
data$Team_Average_num <- as.numeric(gsub("[\\$,]","",data$Team_Average_num))

data$Team_Total_Gifts_num <- as.character(data$Team_Total_Gifts)
data$Team_Total_Gifts_num <- as.numeric(gsub("[\\$,]","",data$Team_Total_Gifts_num))

data$Fundraising_Goal_num <- as.character(data$Fundraising_Goal)
data$Fundraising_Goal_num <- as.numeric(gsub("[\\$,]","",data$Fundraising_Goal_num))

data$Team_Member_Goal_num <- as.character(data$Team_Member_Goal)
data$Team_Member_Goal_num <- as.numeric(gsub("[\\$,]","",data$Team_Member_Goal_num))

data$Participant_Gifts_num <- as.character(data$Participant_Gifts)
data$Participant_Gifts_num <- as.numeric(gsub("[\\$,]","",data$Participant_Gifts_num))

data$Personal_Gift_num <- as.character(data$Personal_Gift)
data$Personal_Gift_num <- as.numeric(gsub("[\\$,]","",data$Personal_Gift_num))

data$Top_walker[data$Total_Gifts_num >= 1000] <- 1
data$Top_walker[data$Total_Gifts_num < 1000]<- 0
data$Top_walker <- as.factor(data$Top_walker)
summary(data$Top_walker)
plot(data$Top_walker,xlab = "Class labels",ylab ="number of observations")

install.packages("caret")
library(caret)
nzv <- nearZeroVar(data,saveMetrics = TRUE) #checcking to see what features have a near zero variance and it will not make sense to
                                            #include them in the model

#scaling the dataset
data1 <- data
data1$Team_Count <- scale(data1$Team_Count, center = TRUE , scale = TRUE)
data1$Gifts_Count <- scale(data1$Gifts_Count,center = TRUE,scale = TRUE)
data1$MEDAGE_CY <- scale(data1$MEDAGE_CY,center = TRUE,scale = TRUE)
data1$MEDHINC_CY <- scale(data1$MEDHINC_CY,center = TRUE,scale = TRUE)
data1$MEDDI_CY <- scale(data1$MEDDI_CY,center = TRUE,scale =  TRUE)
data1$MEDNW_CY <- scale(data1$MEDNW_CY,center = TRUE,scale = TRUE)
data1$Fundraising_Goal_num <- scale(data1$Fundraising_Goal_num,center = TRUE,scale = TRUE)


#calculating the correaltion of the int columns:
nums<-sapply(data1,is.numeric)
cor_data<-  data1[,nums]
cor_data$Team_Member_Goal_num<-NULL
a<-cor(cor_data,cor_data)
install.packages("corrplot")
library(corrplot)
corrplot(a,method = "circle")


#EDA
install.packages("sqldf")
library(sqldf)
P1<- sqldf("SELECT * FROM data WHERE data.Event_Year == 'FY 2015' ")
col2015<-colnames(P1)
colnames(P1)<-paste("15",col2015,sep = "_")
P2<- sqldf("SELECT * FROM data WHERE data.Event_Year == 'FY 2016' ")
col2016<-colnames(P2)
colnames(P2)<-paste("16",col2016,sep = "_")
P3<- sqldf("SELECT * FROM data WHERE data.Event_Year == 'FY 2017' ")
col2017<-colnames(P3)
colnames(P3)<-paste("17",col2017,sep = "_")
#trying to find the number of participants that participated three years in a row
sqldf("SELECT COUNT(*) FROM P1 INNER JOIN P2 ON P1.'15_Participant_Id' = P2.'16_Participant_Id' INNER JOIN P3 ON P3.'17_Participant_Id' = P1.'15_Participant_Id'")
#trying to find the number of participants that participated in 2016 and 2017
sqldf("sELECT COUNT(*) FROM P2 INNER JOIN P3 ON P2.'16_Participant_Id' = P3.'17_Participant_Id'")
#trying to find the number of participants that participated in 2015 and 2016
sqldf("sELECT COUNT(*) FROM P1 INNER JOIN P2 ON P1.'15_Participant_Id' = P2.'16_Participant_Id'")

data_1617 <- sqldf("SELECT * FROM P2 INNER JOIN P3 ON P2.'16_Participant_Id' = P3.'17_Participant_Id'")
#calculating percent data identified as top walker for the year 2015
mattop15<-summary(P1$`15_Top_walker`)
topper15 <- (mattop15[2]/(mattop15[1]+mattop15[2]))*100
topper15

mattop16<-summary(P2$`16_Top_walker`)
topper16 <- (mattop16[2]/(mattop16[1]+mattop16[2]))*100
topper16

mattop17<-summary(P3$`17_Top_walker`)
topper17 <- (mattop17[2]/(mattop17[1]+mattop17[2]))*100
topper17

#running a simple glm model on the data to identify the top walkers
set.seed(42)
sample_size <- floor(0.75* nrow(data1))
train_index <- sample(seq_len(nrow(data1)),size = sample_size, replace = FALSE)
tw_trains <- data1[train_index,]
remdata <- data1[-train_index,]

set.seed(42)
sample_size <- floor(0.50*nrow(remdata))
train_index <- sample(seq_len(nrow(remdata)),size = sample_size,replace = FALSE)
tw_valid <- remdata[train_index,]
tw_test <- remdata[-train_index,]


model1_logistic <- glm(Top_walker ~ Fundraising_Goal_num+State+Team_Count+Gifts_Count+Registration_Gift+MATCH_CODE+TAP_LEVEL+
                         TAP_DESC+TAP_LIFED+MEDAGE_CY+MEDHINC_CY+MEDDI_CY+MEDNW_CY,data = tw_trains,family = binomial)

summary(model1_logistic)

predict_train_logistic <- predict(model1_logistic,type = "response")
table(tw_trains$Top_walker,predict_train_logistic > 0.5)

predict_test_logistic <- predict(model1_logistic,tw_valid,type="response") #testing using the validation set
table(tw_valid$Top_walker,predict_test_logistic >0.5)

install.packages("pROC")
library(pROC)
rocCurve   <- roc(response = tw_trains$Top_walker,
                  predictor = predict_train_logistic,
                  levels = rev(levels(tw_trains$Top_walker)))
plot(rocCurve, print.thres = "best") # ROC curve for the train dataset

rocCurve   <- roc(response = tw_valid$Top_walker,
                  predictor = predict_test_logistic,
                  levels = rev(levels(tw_valid$Top_walker)))
plot(rocCurve, print.thres = "best") # ROC curve for the test dataset

predict_test_logistic1 <- predict(model1_logistic,tw_test,type="response") #testing using the validation set
table(tw_test$Top_walker,predict_test_logistic1 >0.5)

rocCurve   <- roc(response = tw_test$Top_walker,
                  predictor = predict_test_logistic1,
                  levels = rev(levels(tw_test$Top_walker)))
plot(rocCurve, print.thres = "best") # ROC curve for the test dataset


#Need to play around with the cutoff probability values to see if that can help imporve the model.
#Need to calculate the accuracy and other metrics for the train and the validation dataset

#decision treee modeling for classsification:

#Boruta package for feature selection
tw_trains_fs<-tw_trains
tw_trains_fs$Company_Goal<-NULL
tw_trains_fs$Company_Name<-NULL
tw_trains_fs$Team_Average<- NULL
tw_trains_fs$Team_Member_Goal<-NULL
tw_trains_fs$Team_Total_Gifts<- NULL
tw_trains_fs$Zip<-NULL
tw_trains_fs$Participant_Gifts<-NULL
tw_trains_fs$Personal_Gift<-NULL
tw_trains_fs$Total_Gifts<-NULL
tw_trains_fs$Total_Gifts_num<-NULL
tw_trains_fs$Team_Average_num<-NULL
tw_trains_fs$Team_Total_Gifts_num<-NULL
tw_trains_fs$Fundraising_Goal<-NULL
tw_trains_fs$Team_Member_Goal_num<-NULL
tw_trains_fs$Participant_Gifts_num<-NULL
tw_trains_fs$Personal_Gift_num<-NULL
tw_trains_fs$Event_Date<-NULL
tw_trains_fs$Event_Year<-NULL
tw_trains_fs$Name<-NULL
tw_trains_fs$Participant_Id<-NULL
tw_trains_fs$Street<-NULL
tw_trains_fs$Team_Id<-NULL
tw_trains_fs$Team_Name<-NULL
tw_trains_fs$DIVINDX_CY<-scale(tw_trains_fs$DIVINDX_CY,center = TRUE,scale = TRUE)
tw_trains_fs$City<-NULL



install.packages("Boruta")
library(Boruta)
set.seed(1)
Feature_selection<-Boruta(Top_walker~.,data=tw_trains_fs,maxRuns = 20,doTrace =2,getImp = getImpRfZ)
Feature_selection$finalDecision
summary(Feature_selection$finalDecision)

#plot of feature selection importance based on random forest application
plot(Feature_selection, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(Feature_selection$ImpHistory),function(i)
Feature_selection$ImpHistory[is.finite(Feature_selection$ImpHistory[,i]),i])
names(lz) <- colnames(Feature_selection$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),at = 1:ncol(Feature_selection$ImpHistory), cex.axis = 0.7)

tw_valid1<-tw_valid
tw_valid1$Company_Goal<-NULL
tw_valid1$Company_Name<-NULL
tw_valid1$Team_Average<- NULL
tw_valid1$Team_Member_Goal<-NULL
tw_valid1$Team_Total_Gifts<- NULL
tw_valid1$Zip<-NULL
tw_valid1$Participant_Gifts<-NULL
tw_valid1$Personal_Gift<-NULL
tw_valid1$Total_Gifts<-NULL
tw_valid1$Total_Gifts_num<-NULL
tw_valid1$Team_Average_num<-NULL
tw_valid1$Team_Total_Gifts_num<-NULL
tw_valid1$Fundraising_Goal<-NULL
tw_valid1$Team_Member_Goal_num<-NULL
tw_valid1$Participant_Gifts_num<-NULL
tw_valid1$Personal_Gift_num<-NULL
tw_valid1$Event_Date<-NULL
tw_valid1$Event_Year<-NULL
tw_valid1$Name<-NULL
tw_valid1$Participant_Id<-NULL
tw_valid1$Street<-NULL
tw_valid1$Team_Id<-NULL
tw_valid1$Team_Name<-NULL
tw_valid1$DIVINDX_CY<-scale(tw_valid1$DIVINDX_CY,center = TRUE,scale = TRUE)
tw_valid1$City<-NULL

tw_test1<-tw_test
tw_test1$Company_Goal<-NULL
tw_test1$Company_Name<-NULL
tw_test1$Team_Average<- NULL
tw_test1$Team_Member_Goal<-NULL
tw_test1$Team_Total_Gifts<- NULL
tw_test1$Zip<-NULL
tw_test1$Participant_Gifts<-NULL
tw_test1$Personal_Gift<-NULL
tw_test1$Total_Gifts<-NULL
tw_test1$Total_Gifts_num<-NULL
tw_test1$Team_Average_num<-NULL
tw_test1$Team_Total_Gifts_num<-NULL
tw_test1$Fundraising_Goal<-NULL
tw_test1$Team_Member_Goal_num<-NULL
tw_test1$Participant_Gifts_num<-NULL
tw_test1$Personal_Gift_num<-NULL
tw_test1$Event_Date<-NULL
tw_test1$Event_Year<-NULL
tw_test1$Name<-NULL
tw_test1$Participant_Id<-NULL
tw_test1$Street<-NULL
tw_test1$Team_Id<-NULL
tw_test1$Team_Name<-NULL
tw_test1$DIVINDX_CY<-scale(tw_test1$DIVINDX_CY,center = TRUE,scale = TRUE)
tw_test1$City<-NULL

#Applying neural net on the identified daataset to predict top walker
response<-"Top_walker"
predictors<-setdiff(names(tw_trains_fs),response)
install.packages("h2o")
library(h2o)
h2o.init(nthreads=-1, max_mem_size="8G")

write.csv(tw_trains_fs, file = "dat.csv", row.names = F, col.names = T)
train_h<- h2o.importFile(path = "D:/Semester 5/project 1 R files/Practicum project 1 R files/dat.csv")
train_h$Top_walker<-as.factor(train_h$Top_walker)
#tw_valid$DIVINDX_CY<-scale(tw_valid$DIVINDX_CY,center = TRUE,scale=TRUE)
write.csv(tw_valid1, file = "dat1.csv", row.names = F, col.names = T)
valid_h<-h2o.importFile(path = "D:/Semester 5/project 1 R files/Practicum project 1 R files/dat1.csv") 
valid_h$Top_walker<-as.factor(valid_h$Top_walker)
#tw_test$DIVINDX_CY<-scale(tw_test$DIVINDX_CY,center = TRUE,scale = TRUE)
write.csv(tw_test1, file = "dat2.csv", row.names = F, col.names = T)
test_h<-h2o.importFile(path = "D:/Semester 5/project 1 R files/Practicum project 1 R files/dat2.csv") 
test_h$Top_walker<-as.factor(test_h$Top_walker)


#neural net model for featrure selection dataset
hyper_params <- list(
  #l1=seq(0,1e-3,1e-1),
  l2=seq(0,1e-4,1e-6),
  rate=c(0.1,0.01,0.03,0.005),
  rate_annealing=c(1e-8,1e-7,1e-6),
  epochs=c(100,200,500,1000),
  hidden=list(c(18),c(18,15,10,5,2),c(15,10),c(15,10,5),c(10,5))
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




