#k-means clustering of the german credit data
rm(list = ls())
install.packages("rattle")
library(rattle)
install.packages("NbClust")
library(NbClust)
install.packages("flexclust")
library(flexclust)
install.packages("ClusterR")
library(ClusterR)
library(caret)

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

#Checking to see the Class ratio of the german credit data
summary(GermanCredit$Class)
plot(GermanCredit$Class,xlab = "Class labels",ylab ="number of observations")
#Checking to see the structure of the dataset
str(GermanCredit)

data<- GermanCredit
data$Class <- NULL#removing the class labels from the dataset
str(data)#checking to see the structure of the new dataset created for k-means
#we need to standardize the data for clustering so that no one feature dominates the distance measures
#standardizing the data
data <- scale(data,center = TRUE,scale = TRUE)


wssplot <- function(data, nc=10, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
wssplot(data)

set.seed(1234)
nc <- NbClust(data, min.nc=2, max.nc=10, method="kmeans",index = "all")
barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

set.seed(1234)
oc_variance=Optimal_Clusters_KMeans(data, 10, criterion = "variance_explained",
                        fK_threshold = 0.85, num_init = 25, max_iters = 2000,
                        initializer = "kmeans++", tol = 1e-04,
                        plot_clusters = TRUE, verbose = FALSE, tol_optimal_init = 0.3,
                        seed = 1)
#number of clusters identified from variance explained metric is 2.
set.seed(1234)
oc_silhouette=Optimal_Clusters_KMeans(data, 10, criterion = "silhouette",
                            fK_threshold = 0.85, num_init = 25, max_iters = 2000,
                            initializer = "kmeans++", tol = 1e-04,
                            plot_clusters = TRUE, verbose = FALSE, tol_optimal_init = 0.3,
                            seed = 1)
#based on this metric the number of clusters to be choosen comes out to be as 3

set.seed(1234)
oc_distortion_fK = Optimal_Clusters_KMeans(data, 10, criterion = "distortion_fK",
                                           fK_threshold = 0.85, num_init = 25, max_iters = 2000,
                                           initializer = "kmeans++", tol = 1e-04,
                                           plot_clusters = TRUE, verbose = FALSE, tol_optimal_init = 0.3,
                                           seed = 1)
#the distortion metric is of no use as the values are greater than the threshold value although the value is smallest for 2 clusters
#doing the cluster formation for 2 clusters.
set.seed(1234)
fit.km <- kmeans(data,2,nstart=25)
fit.km$size
fit.km$centers

cluster_means<-aggregate(GermanCredit, by=list(cluster=fit.km$cluster), mean)
ct.km <- table(GermanCredit$Class,fit.km$cluster)
ct.km

randIndex(ct.km)

GermanCredit$kmeans_cluster <- fit.km$cluster

#####################################################################################
#Expectation Maximization
EM1<-GMM(data, gaussian_comps = 2,
    seed_mode = "random_subset", em_iter = 500,
    verbose = FALSE, var_floor = 1e-10, seed = 1234)
GermanCredit$EM_cluster[EM1$Log_likelihood[,1]>EM1$Log_likelihood[,2]]<-1
GermanCredit$EM_cluster[EM1$Log_likelihood[,2]>EM1$Log_likelihood[,1]]<-2
et<-table(GermanCredit$Class,GermanCredit$EM_cluster)
et
######################################################################################
install.packages("Boruta")
library(Boruta)

#doing feature selection using random forest for the credit data
set.seed(1234)
Feature_selection<-Boruta(Class~.,data=GermanCredit,maxRuns = 100,doTrace =2,getImp = getImpRfZ)
Feature_selection$finalDecision
summary(Feature_selection$finalDecision)
data_fs <-GermanCredit
data_fs$Class <- NULL
data_fs$kmeans_cluster<-NULL
data_fs$EM_cluster<- NULL
data_fs<-data_fs[Feature_selection$finalDecision == "Confirmed"]#the dataset created has only the featrues that are identified as important by the the feature selection process

#Applying kmeans to the new dataset
#standardizing the dataset data_fs
data_fs <- scale(data_fs,center = TRUE,scale=TRUE)
wssplot(data_fs)#looking at the graph we predict 4 clusters
set.seed(1234)
nc_fs <- NbClust(data_fs, min.nc=2, max.nc=10, method="kmeans",index = "all")
barplot(table(nc_fs$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

set.seed(1234)
oc_fs_variance=Optimal_Clusters_KMeans(data_fs, 10, criterion = "variance_explained",
                                    fK_threshold = 0.85, num_init = 25, max_iters = 2000,
                                    initializer = "kmeans++", tol = 1e-04,
                                    plot_clusters = TRUE, verbose = FALSE, tol_optimal_init = 0.3,
                                    seed = 1234)

set.seed(1234)
oc_fs_silhouette=Optimal_Clusters_KMeans(data_fs, 10, criterion = "silhouette",
                                      fK_threshold = 0.85, num_init = 25, max_iters = 2000,
                                      initializer = "kmeans++", tol = 1e-04,
                                      plot_clusters = TRUE, verbose = FALSE, tol_optimal_init = 0.3,
                                      seed = 1234)
#based on this metric the number of clusters to be choosen comes out to be as 6

set.seed(1234)
oc_fs_distortion_fK = Optimal_Clusters_KMeans(data_fs, 10, criterion = "distortion_fK",
                                           fK_threshold = 0.85, num_init = 25, max_iters = 2000,
                                           initializer = "kmeans++", tol = 1e-04,
                                           plot_clusters = TRUE, verbose = FALSE, tol_optimal_init = 0.3,
                                           seed = 1234)
#the distortion metric is of no use as the values are greater than the threshold value although the value is smallest for 2 clusters
#doing the cluster formation for 4 clusters.
set.seed(1234)
fit.km_fs <- kmeans(data_fs,4,nstart=25)
fit.km_fs$size
fit.km_fs$centers

cluster_means_fs<-aggregate(GermanCredit, by=list(cluster=fit.km_fs$cluster), mean)
ct.km_fs <- table(GermanCredit$Class,fit.km_fs$cluster)
ct.km_fs

randIndex(ct.km_fs)

GermanCredit$kmeans_cluster_fs <- fit.km_fs$cluster

#Expectation Maximization for feature selection
EM1_fs<-GMM(data_fs, gaussian_comps = 4,
         seed_mode = "random_subset", em_iter = 500,
         verbose = FALSE, var_floor = 1e-10, seed = 1234)
GermanCredit$EM_fs_cluster[EM1_fs$Log_likelihood[,1]>EM1_fs$Log_likelihood[,2]&EM1_fs$Log_likelihood[,1]>EM1_fs$Log_likelihood[,3]&EM1_fs$Log_likelihood[,1]>EM1_fs$Log_likelihood[,4]]<-1
GermanCredit$EM_fs_cluster[EM1_fs$Log_likelihood[,2]>EM1_fs$Log_likelihood[,1]&EM1_fs$Log_likelihood[,2]>EM1_fs$Log_likelihood[,3]&EM1_fs$Log_likelihood[,2]>EM1_fs$Log_likelihood[,4]]<-2
GermanCredit$EM_fs_cluster[EM1_fs$Log_likelihood[,3]>EM1_fs$Log_likelihood[,1]&EM1_fs$Log_likelihood[,3]>EM1_fs$Log_likelihood[,2]&EM1_fs$Log_likelihood[,3]>EM1_fs$Log_likelihood[,4]]<-3
GermanCredit$EM_fs_cluster[EM1_fs$Log_likelihood[,4]>EM1_fs$Log_likelihood[,1]&EM1_fs$Log_likelihood[,4]>EM1_fs$Log_likelihood[,2]&EM1_fs$Log_likelihood[,4]>EM1_fs$Log_likelihood[,3]]<-4
et_fs<-table(GermanCredit$Class,GermanCredit$EM_fs_cluster)
et_fs

#pca code 
pca1 <- prcomp(data)
plot(pca1,type="l")
summary(pca1)
str(pca1)
var1 = pca1$sdev^2
var1
plot(var1,type="b")
var2 = var1/sum(var1)
plot(var2,type="b")
plot(cumsum(var2),type="b")
cumsum(var2)
data_pca <- pca1[["x"]]
data_pca<-data_pca[,1:19]

#performing K-means for pca data
wssplot(data_pca)#looking at the graph we predict 4 clusters
set.seed(1234)
nc_pca <- NbClust(data_pca, min.nc=2, max.nc=10, method="kmeans",index = "all")
barplot(table(nc_pca$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

set.seed(1234)
oc_pca_variance=Optimal_Clusters_KMeans(data_pca, 10, criterion = "variance_explained",
                                       fK_threshold = 0.85, num_init = 25, max_iters = 2000,
                                       initializer = "kmeans++", tol = 1e-04,
                                       plot_clusters = TRUE, verbose = FALSE, tol_optimal_init = 0.3,
                                       seed = 1234)
#based on the graph the ideal number of clusters seem to be 2
set.seed(1234)
oc_pca_silhouette=Optimal_Clusters_KMeans(data_pca, 10, criterion = "silhouette",
                                         fK_threshold = 0.85, num_init = 25, max_iters = 2000,
                                         initializer = "kmeans++", tol = 1e-04,
                                         plot_clusters = TRUE, verbose = FALSE, tol_optimal_init = 0.3,
                                         seed = 1234)
#based on this metric the number of clusters to be choosen comes out to be as 4

set.seed(1234)
oc_pca_distortion_fK = Optimal_Clusters_KMeans(data_pca, 10, criterion = "distortion_fK",
                                              fK_threshold = 0.85, num_init = 25, max_iters = 2000,
                                              initializer = "kmeans++", tol = 1e-04,
                                              plot_clusters = TRUE, verbose = FALSE, tol_optimal_init = 0.3,
                                              seed = 1234)
#the distortion metric is of no use as the values are greater than the threshold value although the value is smallest for 2 clusters
#doing the cluster formation for 2 clusters.
set.seed(1234)
fit.km_pca <- kmeans(data_pca,2,nstart=25)
fit.km_pca$size
fit.km_pca$centers

cluster_means_pca<-aggregate(GermanCredit, by=list(cluster=fit.km_pca$cluster), mean)
ct.km_pca <- table(GermanCredit$Class,fit.km_pca$cluster)
ct.km_pca

randIndex(ct.km_pca)

GermanCredit$kmeans_cluster_pca <- fit.km_pca$cluster

#Expectation Maximization for pca data
EM1_pca<-GMM(data_pca, gaussian_comps = 2,
         seed_mode = "random_subset", em_iter = 500,
         verbose = FALSE, var_floor = 1e-10, seed = 1234)
GermanCredit$EM_cluster_pca[EM1_pca$Log_likelihood[,1]>EM1_pca$Log_likelihood[,2]]<-1
GermanCredit$EM_cluster_pca[EM1_pca$Log_likelihood[,2]>EM1_pca$Log_likelihood[,1]]<-2
et_pca<-table(GermanCredit$Class,GermanCredit$EM_cluster_pca)
et_pca
#ica code 
install.packages("ica")
library(ica)
install.packages("moments")
library(moments)
ica1<-icafast(data,41)
data_ica <- ica1[["S"]]
kur<-kurtosis(data_ica,na.rm = TRUE)
kur
a<- sort(kur,decreasing = TRUE)
a
#retaining all the ICA components uptill the 1st quartile
data_ica<- data_ica[,kur>1.886]

#Doing K-means clustering for ICA components
wssplot(data_ica)#looking at the graph we predict 4 clusters
set.seed(1234)
nc_ica <- NbClust(data_ica, min.nc=2, max.nc=15, method="kmeans",index = "all")
barplot(table(nc_ica$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

set.seed(1234)
oc_ica_variance=Optimal_Clusters_KMeans(data_ica, 10, criterion = "variance_explained",
                                        fK_threshold = 0.85, num_init = 25, max_iters = 2000,
                                        initializer = "kmeans++", tol = 1e-04,
                                        plot_clusters = TRUE, verbose = FALSE, tol_optimal_init = 0.3,
                                        seed = 1234)
#based on the graph the ideal number of clusters seem to be 2
set.seed(1234)
oc_ica_silhouette=Optimal_Clusters_KMeans(data_ica, 10, criterion = "silhouette",
                                          fK_threshold = 0.85, num_init = 25, max_iters = 2000,
                                          initializer = "kmeans++", tol = 1e-04,
                                          plot_clusters = TRUE, verbose = FALSE, tol_optimal_init = 0.3,
                                          seed = 1234)
#based on this metric the number of clusters to be choosen comes out to be as 2

set.seed(1234)
oc_ica_distortion_fK = Optimal_Clusters_KMeans(data_ica, 10, criterion = "distortion_fK",
                                               fK_threshold = 0.85, num_init = 25, max_iters = 2000,
                                               initializer = "kmeans++", tol = 1e-04,
                                               plot_clusters = TRUE, verbose = FALSE, tol_optimal_init = 0.3,
                                               seed = 1234)
#the distortion metric is of no use as the values are greater than the threshold value although the value is smallest for 2 clusters
#doing the cluster formation for 2 clusters.
set.seed(1234)
fit.km_ica <- kmeans(data_ica,2,nstart=25)
fit.km_ica$size
fit.km_ica$centers

cluster_means_ica<-aggregate(GermanCredit, by=list(cluster=fit.km_ica$cluster), mean)
ct.km_ica <- table(GermanCredit$Class,fit.km_ica$cluster)
ct.km_ica

randIndex(ct.km_ica)

GermanCredit$kmeans_cluster_ica <- fit.km_ica$cluster

#Expectation Maximization for ica
EM1_ica<-GMM(data_ica, gaussian_comps = 2,
             seed_mode = "random_subset", em_iter = 500,
             verbose = FALSE, var_floor = 1e-10, seed = 1234)
GermanCredit$EM_cluster_ica[EM1_ica$Log_likelihood[,1]>EM1_ica$Log_likelihood[,2]]<-1
GermanCredit$EM_cluster_ica[EM1_ica$Log_likelihood[,2]>EM1_ica$Log_likelihood[,1]]<-2
et_ica <- table(GermanCredit$Class,GermanCredit$EM_cluster_ica)
et_ica
#random code
mat1 <- matrix(runif(41*19,min = -1,max = 1),41,19)
ran1 <- as.matrix(data)%*%mat1
data_ran <- as.data.frame(ran1)

#doing K-means for randomized component
wssplot(data_ran)#looking at the graph we predict 2 clusters
set.seed(1234)
nc_rca <- NbClust(data_ran, min.nc=2, max.nc=15, method="kmeans",index = "all")
barplot(table(nc_rca$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

set.seed(1234)
oc_rca_variance=Optimal_Clusters_KMeans(data_ran, 10, criterion = "variance_explained",
                                        fK_threshold = 0.85, num_init = 25, max_iters = 2000,
                                        initializer = "kmeans++", tol = 1e-04,
                                        plot_clusters = TRUE, verbose = FALSE, tol_optimal_init = 0.3,
                                        seed = 1234)
#based on the graph the ideal number of clusters seem to be 2
set.seed(1234)
oc_rca_silhouette=Optimal_Clusters_KMeans(data_ran, 10, criterion = "silhouette",
                                          fK_threshold = 0.85, num_init = 25, max_iters = 2000,
                                          initializer = "kmeans++", tol = 1e-04,
                                          plot_clusters = TRUE, verbose = FALSE, tol_optimal_init = 0.3,
                                          seed = 1234)
#based on this metric the number of clusters to be choosen comes out to be as 5

set.seed(1234)
oc_rca_distortion_fK = Optimal_Clusters_KMeans(data_ran, 10, criterion = "distortion_fK",
                                               fK_threshold = 0.85, num_init = 25, max_iters = 2000,
                                               initializer = "kmeans++", tol = 1e-04,
                                               plot_clusters = TRUE, verbose = FALSE, tol_optimal_init = 0.3,
                                               seed = 1234)
#the distortion metric is of no use as the values are greater than the threshold value although the value is smallest for 2 clusters
#doing the cluster formation for 2 clusters.
set.seed(1234)
fit.km_rca <- kmeans(data_ran,2,nstart=25)
fit.km_rca$size
fit.km_rca$centers

cluster_means_rca<-aggregate(GermanCredit, by=list(cluster=fit.km_rca$cluster), mean)
ct.km_rca <- table(GermanCredit$Class,fit.km_rca$cluster)
ct.km_rca

randIndex(ct.km_rca)

GermanCredit$kmeans_cluster_rca <- fit.km_rca$cluster

#Expectation maximization for RCA
EM1_rca<-GMM(data_ran, gaussian_comps = 2,
             seed_mode = "random_subset", em_iter = 500,
             verbose = FALSE, var_floor = 1e-10, seed = 1234)
GermanCredit$EM_cluster_rca[EM1_rca$Log_likelihood[,1]>EM1_rca$Log_likelihood[,2]]<-1
GermanCredit$EM_cluster_rca[EM1_rca$Log_likelihood[,2]>EM1_rca$Log_likelihood[,1]]<-2
et_rca<-table(GermanCredit$Class,GermanCredit$EM_cluster_rca)
et_rca

################################################################################################3
#Neural Network implementation on the dimensionally reduced data 
#Splitting the data into train validate and test set
#Neural Net for featur selection data
data_fs<-as.data.frame(data_fs)
data_fs$Class<-GermanCredit$Class

set.seed(1234)
index<-createDataPartition(data_fs$Class,p=0.9)[[1]]
train_data_fs<-data_fs[index,]
test_data_fs<-data_fs[-index,]

response<-"Class"
predictors<-setdiff(names(train_data_fs),response)
predictors

library(h2o)
##initializaing H20
h2o.init(nthreads=-1, max_mem_size="6G")

train_h20<-as.h2o(train_data_fs)
train_split <- h2o.splitFrame(data=train_h20, ratios=0.9)
train_h <- train_split[[1]]
valid_h<-train_split[[2]]
test_h<-as.h2o(test_data_fs)

#neural net model for featrure selection dataset
hyper_params <- list(
  l1=seq(0,1e-3,1e-1),
  #l2=seq(0,1e-4,1e-6),
  rate=c(0.1,0.01,0.03,0.005),
  rate_annealing=c(1e-8,1e-7,1e-6),
  epochs=c(500,100),
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

#Neural net for PCA data
data_pca <- as.data.frame(data_pca)
data_pca$Class<- GermanCredit$Class
colnames(data_pca)<-c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11","PC12","PC13","PC14","PC15","PC16","PC17","PC18","PC19","Class")

set.seed(1234)
index<-createDataPartition(data_pca$Class,p=0.9)[[1]]
train_data_pca<-data_pca[index,]
test_data_pca<-data_pca[-index,]

response<-"Class"
predictors<-setdiff(names(train_data_pca),response)
predictors


train_h20_pca<-as.h2o(train_data_pca)
train_split_pca <- h2o.splitFrame(data=train_h20_pca, ratios=0.9)
train_h_pca<- train_split_pca[[1]]
valid_h_pca<-train_split_pca[[2]]
test_h_pca<-as.h2o(test_data_pca)

#neural net model for PCA dataset
hyper_params_pca <- list(
  l1=seq(0,1e-3,1e-1),
  #l2=seq(0,1e-4,1e-6),
  rate=c(0.1,0.01,0.03,0.005),
  rate_annealing=c(1e-8,1e-7,1e-6),
  epochs=c(1000,500,100),
  hidden=list(c(19),c(19,15,10,5,2),c(15,10),c(15,10,5),c(10,5))
)

nnet2_pca <- h2o.grid(
  algorithm="deeplearning",
  search_criteria=list(strategy="RandomDiscrete",max_models=200,stopping_rounds=3,stopping_metric="misclassification",stopping_tolerance=0.0001),
  grid_id="nnet2_grid2_pca", 
  model_id="nnet_model_2_pca", 
  training_frame=train_h_pca, 
  validation_frame=valid_h_pca,
  x=predictors,
  y=response,
  stopping_rounds=3,
  stopping_metric="misclassification", ## could be "MSE","logloss","r2"
  stopping_tolerance=0.0001,
  nfolds=3,
  fold_assignment = "Stratified",
  activation=c("Rectifier"),
  hyper_params=hyper_params_pca,
  standardize=FALSE,
  adaptive_rate=F,                ## manually tuned learning rate
  momentum_start=0.5,             ## manually tuned momentum
  momentum_stable=0.9, 
  momentum_ramp=1e7,
  classification_stop=-1
)
model_list_pca <- h2o.getGrid("nnet2_grid2_pca",sort_by="auc",decreasing=TRUE)
model_list_pca

## To see what other "sort_by" criteria are allowed
#grid <- h2o.getGrid("dl_grid",sort_by="wrong_thing",decreasing=FALSE)

## Sort by logloss
h2o.getGrid("nnet2_grid2_pca",sort_by="logloss",decreasing=FALSE)

## Find the best model and its full set of parameters
model_list_pca@summary_table[1,]
best_model_pca <- h2o.getModel(model_list_pca@model_ids[[1]])
summary(best_model_pca)
plot(best_model_pca)
plot(h2o.performance(best_model_pca))
test_perf_bm_pca<-predict(best_model_pca, test_h_pca)
test_perf_check_bm_pca<-h2o.performance(best_model_pca,test_h_pca)
confusion_matrix_bm_pca<-h2o.confusionMatrix(test_perf_check_bm_pca)
print(confusion_matrix_bm_pca)
plot(test_perf_check_bm_pca,type="roc")
h2o.auc(test_perf_check_bm_pca)

#Neural Net for ICA data
data_ica = as.data.frame(data_ica)
data_ica$Class<-GermanCredit$Class
colnames(data_ica)<-c("IC1","IC2","IC3","IC4","IC5","IC6","IC7","IC8","IC9","IC10",
                      "IC11","IC12","IC13","IC14","IC15","IC16","IC17","IC18","IC19","IC20",
                      "IC21","IC22","IC23","IC24","IC25","IC26","IC27","IC28","IC29","IC30","IC31","Class")

set.seed(1234)
index<-createDataPartition(data_ica$Class,p=0.9)[[1]]
train_data_ica<-data_ica[index,]
test_data_ica<-data_ica[-index,]

response<-"Class"
predictors<-setdiff(names(train_data_ica),response)
predictors


train_h20_ica<-as.h2o(train_data_ica)
train_split_ica <- h2o.splitFrame(data=train_h20_ica, ratios=0.9)
train_h_ica<- train_split_ica[[1]]
valid_h_ica<-train_split_ica[[2]]
test_h_ica<-as.h2o(test_data_ica)

#neural net model for ICA dataset
hyper_params_ica <- list(
  l1=seq(0,1e-3,1e-1),
  #l2=seq(0,1e-4,1e-6),
  rate=c(0.1,0.01,0.03,0.005),
  rate_annealing=c(1e-8,1e-7,1e-6),
  epochs=c(1000,500,100),
  hidden=list(c(15),c(30,20,15,10,2),c(20,10),c(20,10,5),c(20,5))
)

nnet2_ica <- h2o.grid(
  algorithm="deeplearning",
  search_criteria=list(strategy="RandomDiscrete",max_models=200,stopping_rounds=3,stopping_metric="misclassification",stopping_tolerance=0.0001),
  grid_id="nnet2_grid2_ica", 
  model_id="nnet_model_2_ica", 
  training_frame=train_h_ica, 
  validation_frame=valid_h_ica,
  x=predictors,
  y=response,
  stopping_rounds=3,
  stopping_metric="misclassification", ## could be "MSE","logloss","r2"
  stopping_tolerance=0.0001,
  nfolds=3,
  fold_assignment = "Stratified",
  activation=c("Rectifier"),
  hyper_params=hyper_params_ica,
  standardize=FALSE,
  adaptive_rate=F,                ## manually tuned learning rate
  momentum_start=0.5,             ## manually tuned momentum
  momentum_stable=0.9, 
  momentum_ramp=1e7,
  classification_stop=-1
)
model_list_ica <- h2o.getGrid("nnet2_grid2_ica",sort_by="auc",decreasing=TRUE)
model_list_ica

## To see what other "sort_by" criteria are allowed
#grid <- h2o.getGrid("dl_grid",sort_by="wrong_thing",decreasing=FALSE)

## Sort by logloss
h2o.getGrid("nnet2_grid2_ica",sort_by="logloss",decreasing=FALSE)

## Find the best model and its full set of parameters
model_list_ica@summary_table[1,]
best_model_ica <- h2o.getModel(model_list_ica@model_ids[[1]])
summary(best_model_ica)
plot(best_model_ica)
plot(h2o.performance(best_model_ica))
test_perf_bm_ica<-predict(best_model_ica, test_h_ica)
test_perf_check_bm_ica<-h2o.performance(best_model_ica,test_h_ica)
confusion_matrix_bm_ica<-h2o.confusionMatrix(test_perf_check_bm_ica)
print(confusion_matrix_bm_ica)
plot(test_perf_check_bm_ica,type="roc")
h2o.auc(test_perf_check_bm_ica)


#Neueral net for RCA
data_ran$Class <- GermanCredit$Class

set.seed(1234)
index<-createDataPartition(data_ran$Class,p=0.9)[[1]]
train_data_rca<-data_ran[index,]
test_data_rca<-data_ran[-index,]

response<-"Class"
predictors<-setdiff(names(train_data_rca),response)
predictors


train_h20_rca<-as.h2o(train_data_rca)
train_split_rca <- h2o.splitFrame(data=train_h20_rca, ratios=0.9)
train_h_rca<- train_split_rca[[1]]
valid_h_rca<-train_split_rca[[2]]
test_h_rca<-as.h2o(test_data_rca)

#neural net model for RCA dataset
hyper_params_rca <- list(
  l1=seq(0,1,3),
  #l2=seq(0,1e-4,1e-6),
  rate=c(0.001,0.003,0.005),
  rate_annealing=c(1e-8,1e-7,1e-6),
  epochs=c(1000,500,100),
  hidden=list(c(10),c(18,15,10,2),c(19,10),c(19,10,5),c(19,5))
)

nnet2_ica <- h2o.grid(
  algorithm="deeplearning",
  search_criteria=list(strategy="RandomDiscrete",max_models=200,stopping_rounds=3,stopping_metric="misclassification",stopping_tolerance=0.0001),
  grid_id="nnet2_grid2_rca", 
  model_id="nnet_model_2_rca", 
  training_frame=train_h_rca, 
  validation_frame=valid_h_rca,
  x=predictors,
  y=response,
  stopping_rounds=3,
  stopping_metric="misclassification", ## could be "MSE","logloss","r2"
  stopping_tolerance=0.0001,
  nfolds=3,
  fold_assignment = "Stratified",
  activation=c("Rectifier"),
  hyper_params=hyper_params_rca,
  standardize=FALSE,
  adaptive_rate=F,                ## manually tuned learning rate
  momentum_start=0.5,             ## manually tuned momentum
  momentum_stable=0.9, 
  momentum_ramp=1e7,
  classification_stop=-1
)
model_list_rca <- h2o.getGrid("nnet2_grid2_rca",sort_by="auc",decreasing=TRUE)
model_list_rca

## To see what other "sort_by" criteria are allowed
#grid <- h2o.getGrid("dl_grid",sort_by="wrong_thing",decreasing=FALSE)

## Sort by logloss
h2o.getGrid("nnet2_grid2_rca",sort_by="logloss",decreasing=FALSE)

## Find the best model and its full set of parameters
model_list_rca@summary_table[1,]
best_model_rca <- h2o.getModel(model_list_rca@model_ids[[1]])
summary(best_model_rca)
plot(best_model_rca)
plot(h2o.performance(best_model_rca))
test_perf_bm_rca<-predict(best_model_rca, test_h_rca)
test_perf_check_bm_rca<-h2o.performance(best_model_rca,test_h_rca)
confusion_matrix_bm_rca<-h2o.confusionMatrix(test_perf_check_bm_rca)
print(confusion_matrix_bm_rca)
plot(test_perf_check_bm_rca,type="roc")
h2o.auc(test_perf_check_bm_rca)

#########################################################################################
#Neural net on the cluster assignments of k-means as features.
data_cluster_feature<- as.data.frame(GermanCredit$kmeans_cluster)
data_cluster_feature$Class<- GermanCredit$Class
data_cluster_feature$Cluster1[data_cluster_feature$`GermanCredit$kmeans_cluster`==1]<-1
data_cluster_feature$Cluster1[data_cluster_feature$`GermanCredit$kmeans_cluster`!=1]<-0
data_cluster_feature$Cluster2[data_cluster_feature$`GermanCredit$kmeans_cluster`==2]<-1
data_cluster_feature$Cluster2[data_cluster_feature$`GermanCredit$kmeans_cluster`!=2]<-0
data_cluster_feature$`GermanCredit$kmeans_cluster`<-NULL


set.seed(1234)
index<-createDataPartition(data_cluster_feature$Class,p=0.9)[[1]]
train_data_cf<-data_cluster_feature[index,]
test_data_cf<-data_cluster_feature[-index,]

response<-"Class"
predictors<-setdiff(names(train_data_cf),response)
predictors


train_h20_cf<-as.h2o(train_data_cf)
train_split_cf <- h2o.splitFrame(data=train_h20_cf, ratios=0.9)
train_h_cf<- train_split_cf[[1]]
valid_h_cf<-train_split_cf[[2]]
test_h_cf<-as.h2o(test_data_cf)


hyper_params_cf <- list(
  l1=seq(0,1e-2,1e-1),
  #l2=seq(0,1e-4,1e-6),
  rate=c(0.001,0.002,0.003),
  rate_annealing=c(1e-8,1e-7,1e-6),
  epochs=c(1000,500,100),
  hidden=list(c(2),c(5,5),c(5,2),c(5,5,5),c(2,2),c(40,40))
)

nnet2_cf <- h2o.grid(
  algorithm="deeplearning",
  search_criteria=list(strategy="RandomDiscrete",max_models=200,stopping_rounds=3,stopping_metric="misclassification",stopping_tolerance=0.0001),
  grid_id="nnet2_grid2_cf", 
  model_id="nnet_model_2_cf", 
  training_frame=train_h_cf, 
  validation_frame=valid_h_cf,
  x=predictors,
  y=response,
  stopping_rounds=3,
  stopping_metric="misclassification", ## could be "MSE","logloss","r2"
  stopping_tolerance=0.0001,
  nfolds=3,
  fold_assignment = "Stratified",
  activation=c("Rectifier"),
  hyper_params=hyper_params_cf,
  standardize=FALSE,
  adaptive_rate=F,                ## manually tuned learning rate
  momentum_start=0.5,             ## manually tuned momentum
  momentum_stable=0.9, 
  momentum_ramp=1e7,
  classification_stop=-1
)
model_list_cf <- h2o.getGrid("nnet2_grid2_cf",sort_by="auc",decreasing=TRUE)
model_list_cf

## To see what other "sort_by" criteria are allowed
#grid <- h2o.getGrid("dl_grid",sort_by="wrong_thing",decreasing=FALSE)

## Sort by logloss
h2o.getGrid("nnet2_grid2_cf",sort_by="logloss",decreasing=FALSE)

## Find the best model and its full set of parameters
model_list_cf@summary_table[1,]
best_model_cf <- h2o.getModel(model_list_cf@model_ids[[1]])
summary(best_model_cf)
plot(best_model_cf)
plot(h2o.performance(best_model_cf))
test_perf_bm_cf<-predict(best_model_cf, test_h_cf)
test_perf_check_bm_cf<-h2o.performance(best_model_cf,test_h_cf)
confusion_matrix_bm_cf<-h2o.confusionMatrix(test_perf_check_bm_cf)
print(confusion_matrix_bm_cf)
plot(test_perf_check_bm_cf,type="roc")
h2o.auc(test_perf_check_bm_cf)
########################################################################################
