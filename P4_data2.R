rm(list=ls())

library(ClusterR)
library(data.table)
library(caret)
library(dummies)
library(Boruta)
library(ica)
library(moments)
library(h2o)
#setting working directory
#setwd("~/Documents/JSOM/Fall'18/ML/Project3/Part2") Created a project instead

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


#response and target for clustering
response<-"y"
predictors<-setdiff(names(train_processed),response)
predictors

train_cluster<-train_processed[,predictors]
colnames(train_cluster)

#finding the optimal number of clusters for k-means clustering
Optimal_Clusters_KMeans(train_cluster, max_clusters=15, criterion = "variance_explained",
                        fK_threshold = 0.85, num_init = 3, max_iters = 1000,
                        initializer = "optimal_init", tol = 1e-04, plot_clusters = TRUE,
                        verbose = TRUE, tol_optimal_init = 0.3, seed = 999)

Optimal_Clusters_KMeans(train_cluster, max_clusters=15, criterion = "distortion_fK",
                        fK_threshold = 0.85, num_init = 3, max_iters = 200,
                        initializer = "optimal_init", tol = 1e-04, plot_clusters = TRUE,
                        verbose = TRUE, tol_optimal_init = 0.3, seed = 999)

Optimal_Clusters_KMeans(train_cluster, max_clusters=4, criterion = "silhouette",
                        fK_threshold = 0.85, num_init = 3, max_iters = 200,
                        initializer = "optimal_init", tol = 1e-04, plot_clusters = TRUE,
                        verbose = TRUE, tol_optimal_init = 0.3, seed = 999)

Optimal_Clusters_KMeans(train_cluster, max_clusters=15, criterion = "dissimilarity
                        ",
                        fK_threshold = 0.85, num_init = 3, max_iters = 200,
                        initializer = "optimal_init", tol = 1e-04, plot_clusters = TRUE,
                        verbose = TRUE, tol_optimal_init = 0.3, seed = 999)

## Running k-means for 2 clusters
kmean_2<-KMeans_rcpp(train_cluster,cluster=2,num_init=5,max_iters=200,initializer='optimal_init')
View(kmean_2$clusters)
kmean_nnet_data<-train_processed
kmean_nnet_data$cluster<-kmean_2$clusters
kmean_nnet_data$c1<-ifelse(kmean_nnet_data$cluster==1,1,0)
kmean_nnet_data$c2<-ifelse(kmean_nnet_data$cluster==2,1,0)
kmean_nnet_data<-kmean_nnet_data[,c("y","c1","c2")]
View(kmean_nnet_data)
kmean_table<-xtabs(~y+c1+c2,data=kmean_nnet_data)
ftable(kmean_table)
## finding out optimal number of cluster for GMM

Optimal_Clusters_GMM(train_cluster, max_clusters=20, criterion = "AIC",dist_mode = "maha_dist", 
                     seed_mode = "random_subset", km_iter = 10,em_iter = 10,
                     verbose = FALSE, var_floor = 1e-10, plot_data = TRUE,seed = 999)

soft_cluster<-GMM(train_cluster, gaussian_comps = 3, dist_mode = "maha_dist",seed_mode = "random_subset", 
                  km_iter = 10, em_iter = 10,verbose = TRUE, var_floor = 1e-10, seed = 999)
str(soft_cluster)
View(soft_cluster$Log_likelihood)
soft_cluster_k<-soft_cluster$Log_likelihood
soft_cluster_k<-as.data.frame(soft_cluster_k)
soft_cluster_k$c1<-ifelse(soft_cluster_k$V1>soft_cluster_k$V2 & soft_cluster_k$V1>soft_cluster_k$V3,1,0)
soft_cluster_k$c2<-ifelse(soft_cluster_k$V2>soft_cluster_k$V1 & soft_cluster_k$V2>soft_cluster_k$V3,1,0)
soft_cluster_k$c3<-ifelse(soft_cluster_k$V3>soft_cluster_k$V1 & soft_cluster_k$V3>soft_cluster_k$V2,1,0)
soft_cluster_k$y<-train_processed$y
soft_table<-xtabs(~y+c1+c2+c3,data=soft_cluster_k)
ftable(soft_table)

## Feature selection using Boruta
boruta.train <- Boruta(y~., data = train_processed, maxRuns=100, doTrace = 2)
print(boruta.train)
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
filter_variables<-getSelectedAttributes(boruta.train, withTentative = F)
typeof(filter_variables)
filter_variables[1]<-"jobblue-collar"
filtered_data<-train_processed[,filter_variables]


Optimal_Clusters_KMeans(filtered_data, max_clusters=15, criterion = "variance_explained",
                        fK_threshold = 0.85, num_init = 3, max_iters = 1000,
                        initializer = "optimal_init", tol = 1e-04, plot_clusters = TRUE,
                        verbose = TRUE, tol_optimal_init = 0.3, seed = 999)

kmean_filter<-KMeans_rcpp(filtered_data,cluster=2,num_init=5,max_iters=200,initializer='optimal_init')

Optimal_Clusters_GMM(filtered_data, max_clusters=5, criterion = "AIC",dist_mode = "maha_dist", 
                     seed_mode = "random_subset", km_iter = 10,em_iter = 10,
                     verbose = FALSE, var_floor = 1e-10, plot_data = TRUE,seed = 999)
soft_cluster_filter<-GMM(filtered_data, gaussian_comps = 4, dist_mode = "maha_dist",seed_mode = "random_subset", 
                  km_iter = 10, em_iter = 10,verbose = TRUE, var_floor = 1e-10, seed = 999)


##principal component analysis
pca_trans<-prcomp(train_cluster)
plot(pca_trans,type="l")
summary(pca_trans)
var1<-pca_trans$sdev^2
var2<-var1/sum(var1)
plot(var2,type="b",main = "Percentage of variance in Principal Components",ylab = "Varaince",xlab="Prin Components")
plot(cumsum(var2),type="b",main = "Cumulative Variance in Principal Components",ylab = "Varaince",xlab="Prin Components")
#retaining first 14 principal components as they explain 90% varince in the data
data_pca<-pca_trans[["x"]]
data_pca<-data_pca[,1:14]

Optimal_Clusters_KMeans(data_pca, max_clusters=15, criterion = "variance_explained",
                        fK_threshold = 0.85, num_init = 3, max_iters = 1000,
                        initializer = "optimal_init", tol = 1e-04, plot_clusters = TRUE,
                        verbose = TRUE, tol_optimal_init = 0.3, seed = 999)
kmean_pca<-KMeans_rcpp(filtered_data,cluster=2,num_init=5,max_iters=200,initializer='optimal_init')

Optimal_Clusters_GMM(data_pca, max_clusters=5, criterion = "AIC",dist_mode = "maha_dist", 
                     seed_mode = "random_subset", km_iter = 10,em_iter = 10,
                     verbose = FALSE, var_floor = 1e-10, plot_data = TRUE,seed = 999)
soft_cluster_pca<-GMM(filtered_data, gaussian_comps = 3, dist_mode = "maha_dist",seed_mode = "random_subset", 
                         km_iter = 10, em_iter = 10,verbose = TRUE, var_floor = 1e-10, seed = 999)


##ICA code
ica1<-icafast(train_cluster,41)
data_ica<-ica1[["S"]]
data_ica<-as.data.frame(data_ica)
ica_kurtosis<-kurtosis(data_ica)
View(ica_kurtosis)
typeof(ica_kurtosis)
str(ica_kurtosis)
ica_kurtosis>5
data_ica<-data_ica[,ica_kurtosis>5]
dim(data_ica)

Optimal_Clusters_KMeans(data_ica, max_clusters=15, criterion = "variance_explained",
                        fK_threshold = 0.85, num_init = 3, max_iters = 1000,
                        initializer = "optimal_init", tol = 1e-04, plot_clusters = TRUE,
                        verbose = TRUE, tol_optimal_init = 0.3, seed = 999)
kmean_ica<-KMeans_rcpp(filtered_data,cluster=5,num_init=5,max_iters=200,initializer='optimal_init')

Optimal_Clusters_GMM(data_ica, max_clusters=5, criterion = "AIC",dist_mode = "maha_dist", 
                     seed_mode = "random_subset", km_iter = 10,em_iter = 10,
                     verbose = FALSE, var_floor = 1e-10, plot_data = TRUE,seed = 999)
soft_cluster_ica<-GMM(filtered_data, gaussian_comps = 3, dist_mode = "maha_dist",seed_mode = "random_subset", 
                      km_iter = 10, em_iter = 10,verbose = TRUE, var_floor = 1e-10, seed = 999)


##random components
mat1 <- matrix(runif(41*14),41,14)
random_comp <- as.matrix(train_cluster)%*%mat1
dim(random_comp)
all.moments(random_comp,order.max=2)[3,]


Optimal_Clusters_KMeans(random_comp, max_clusters=15, criterion = "variance_explained",
                        fK_threshold = 0.85, num_init = 3, max_iters = 1000,
                        initializer = "optimal_init", tol = 1e-04, plot_clusters = TRUE,
                        verbose = TRUE, tol_optimal_init = 0.3, seed = 999)
kmean_random<-KMeans_rcpp(random_comp,cluster=2,num_init=5,max_iters=200,initializer='optimal_init')

Optimal_Clusters_GMM(random_comp, max_clusters=5, criterion = "AIC",dist_mode = "maha_dist", 
                     seed_mode = "random_subset", km_iter = 10,em_iter = 10,
                     verbose = FALSE, var_floor = 1e-10, plot_data = TRUE,seed = 999)
soft_cluster_random<-GMM(random_comp, gaussian_comps = 5, dist_mode = "maha_dist",seed_mode = "random_subset", 
                      km_iter = 10, em_iter = 10,verbose = TRUE, var_floor = 1e-10, seed = 999)
