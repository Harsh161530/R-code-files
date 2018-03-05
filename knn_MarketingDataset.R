rm(list=ls())
library(dummies)
#library(h2o)
library(data.table)
library(caret)
library(caTools)
library(pROC)
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

#####knn model with tunelength of 20####
ctrl <- trainControl(method="repeatedcv",number=3,repeats = 3,classProbs=TRUE,summaryFunction = twoClassSummary,verboseIter = TRUE)
knnFit <- train(y ~ ., data = train_data, method = "knn",metric="ROC", trControl = ctrl,  tuneLength = 20)
plot(knnFit)
knnpredict<-predict(knnFit,test_data)
confusionMatrix(knnpredict,test_data$y)
knnpredict_roc<-predict(knnFit,test_data,type="prob")
colAUC(knnpredict_roc,test_data$y,plotROC = TRUE)
auc(test_data$y,knnpredict_roc$no)
