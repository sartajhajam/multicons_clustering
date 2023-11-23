##################################################################################
# GENERATES THE BASE CLUSTERINGS THAT WILL GENERATE THE BINARY MEMBERSHIP MATRIX #
##################################################################################

library(chron)
library(infotheo)
library(clusterSim) #speccl
library(cluster) # diana
library(fpc) #DBSCAN
library(mclust) # mclust
library(clv)
library(flashClust) # flashClust
library(e1071) # bclust, cmeans

###########################################################################################
# REMARK: If the dataset consists of mixed data types, choose only clustering algorithms 
# that use distance matrix, as "gower" metric can be used to calculate the distance in 
# such case. Note that for distance matix, dataset size can have limitation in the number 
# of records (30K records limit on test PC).
###########################################################################################

Dataset <- read.csv(file = file.choose(), header = T)

#---------------------------#
# PARAMETER: CLASS VARIABLE #
#---------------------------#

# Use this instruction if the last column is a class variable (ignored for clustering)
Test_Data <- Dataset[,-ncol(Dataset)] 
# Otherwise, use one of the lines below instead:
#Test_Data <- Dataset[,-c(1,2)]    # Removing first columns
#Test_Data <- Dataset[,c(2:9)]     # Selecting columns
#Test_Data <- Dataset              # Use all columns

Data_Class <- Dataset[,ncol(Dataset)]
#Data_Class <- as.numeric(Dataset[,1])

#--------------------------#
# PARAMETER: NORMALIZATION #
#--------------------------#

# Compute Z-Score version of the test dataset if necessary
Norm_Dataset <- data.frame(scale(Test_Data))  
# If not necessary, use the line below instead
#Norm_Dataset <- Test_Data

Data_Size <- nrow(Norm_Dataset)

#-----------------------------#
# PARAMETER: BASE CLUSTERINGS #
#-----------------------------#

# Note: KL (K values list) and Algo_lst (Algorithms list) are necessary to change or select at each run of the script

## Choose one of the following lines to generate a range of K values for the ensemble ##
#KL <- sample(3:11,size = 8,replace = F)
#KL <- c(sample(4:9,size = 5,replace = F),sample(4:9,size = 6,replace = F))
#KL <- sample(2:trunc(sqrt(Data_Size)),size = 9,replace = T)
#KL <- trunc(runif(8,min = 2,max = 8))
#KL <- c(3:7)                                                                   # Range of K values
#KL <- seq(from = 2,to = 40,by = 3)
#KL <- rep(4,5)                                                                # Repeats Val2 times for K = Val1
KL <- c(3,4,5,6,7,3,4,5,6,7,3,4,5,6,7)
#KL <- c(3,4,5,3,4,5,3,4,5)

## Define the list of algorithms to use for ensemble base clusterings ##
# Note: AGNES is the same clustering as flashClust
# "flashClust","kmeans","mclust","flashClust","cmeans","flashClust","pam","diana","dbscan","bclust","speccl" 
# "agnes","fanny","clara"
# For mixed types datasets: # "flashClust","pam","diana","dbscan" #,"fanny"
Algo_lst <- c("flashClust","flashClust","flashClust","kmeans","mclust","cmeans","pam","diana","bclust","speccl","dbscan")
Algo_lst_len <- length(Algo_lst)

Results <- list() # list of all generated clustering vectors
Methods <- data.frame(name = character(), distance = character(), other = character(),stringsAsFactors = F) # algorithms used

Clstrings <- 0 # counter for the number of clustering results 
Algo_indx <- 1 # index for selecting a clutering algorithm from Algo_lst

Metrc <- c("euclidean", "manhattan", "gower") # distance mertic
Metrc <- Metrc[1]

if (Data_Size > 30000) # remove diana with even lower size
{
  Lrge <- T
  Algo_lst <- Algo_lst[Algo_lst!="flashClust" | Algo_lst!="diana"] # keep only the algorithms applicable on large datasets
  Algo_lst_len <- length(Algo_lst)
}else{
  Lrge <- F
  Dist <- daisy(x = Norm_Dataset,metric = Metrc)
} 

if ("pam" %in% Algo_lst)
{
  if (Lrge) {Pam_Swp <- F} else {Pam_Swp <- T} # PAM parameter, set to F for big datasets
}

if ("dbscan" %in% Algo_lst)
{
  Eps <- 0.5 # DBSCAN parameter
  MinP <- ncol(Test_Data) + 1 # DBSCAN parameter
}

if (("flashClust" %in% Algo_lst) || ("bclust" %in% Algo_lst)) #|| ("agnes" %in% Algo_lst)
{
  Hrch_linkg <- c("average" , "single" , "complete" , "centroid" , "ward" , "mcquitty",  "median" )
#  Agns_linkg <- c("average" , "single" , "complete" , "ward" , "weighted" , "average" ,"gaverage")
  N_linkg <- 7 # the maximum linkage methods to use
  Lnkg_indx <- 1 # index for selecting the linkage method for hierarchical clustering
}

Distance <- Other <- "default"
Rpt <- 1

## Build the base clusterings ##
for (K in KL)
{
  Clstrings <- Clstrings + 1
  if (Algo_indx > Algo_lst_len)
  {
    Algo_indx <- 1
    Rpt <- Rpt + 1
  }
  Algo <- Algo_lst[Algo_indx]
  Algo_indx <- Algo_indx + 1
  
  if(Algo=="kmeans")
  {
    Clustering <- kmeans(x = Norm_Dataset , centers = K ,iter.max = 100,nstart = 50)
    Cluster <- Clustering$cluster
    Name <- paste(Rpt,Algo,K,sep="")
    
  }else if(Algo=="pam"){
    if (Lrge) {X <- Norm_Dataset} else { X <- Dist}
    Cluster <- pam(x = X , k = K ,diss = Pam_Swp,cluster.only = T,do.swap = Pam_Swp)
    Name <- paste(Rpt,Algo, K, sep="")
    Distance <- Metrc
    
  }else if(Algo=="clara"){
    N_Samp <- 5 # CLARA parameter
    Samp_Size <- trunc(Data_Size/N_Samp) # CLARA parameter
    Clustering <- clara(x = Norm_Dataset,k = K,samples = N_Samp,sampsize = Samp_Size)
    Cluster <- Clustering$cluster
    Name <- paste(Rpt,Algo,K,sep="")
    Other <- paste("N_sample=",N_Samp," Size=",Samp_Size)
    rm(N_Samp,Samp_Size)
    
  }else if(Algo=="dbscan"){
    if (Lrge)
    {
      X <- Norm_Dataset
      DBSCN_mthd <- "hybrid"
    } else { 
      X <- Dist
      DBSCN_mthd <- "dist"
    }
#    Clustering <- dbscan(data = X , MinPts = MinP, eps = Eps, method = DBSCN_mthd , seeds = F)
    source("repeat_dbscan.R")
    Cluster <- Clustering$cluster
    if (min(Cluster)==0) { Cluster <- Cluster + 1}
    Name <- paste(Rpt,Algo, length(unique(Cluster)) ,sep="")
    Distance <- Metrc
    Other <- paste("Eps=",round(Eps,digits = 2)," , MinPts=",MinP,sep="")
    rm(Eps,MinP,DBSCN_mthd)
    
  }else if(Algo=="flashClust"){
    
    if (Lnkg_indx > N_linkg) {Lnkg_indx <- 1}
    Clustering <- flashClust(d = Dist , method = Hrch_linkg[Lnkg_indx])
    Cluster <- cutree(tree = Clustering,k = K)
    Other <- Hrch_linkg[Lnkg_indx]
    Name <- paste(Rpt,Algo,K,Other,sep="")
    Distance <- Metrc
    Lnkg_indx <- Lnkg_indx + 1
    
  }else if(Algo=="agnes"){
    
    if (Lnkg_indx > N_linkg) {Lnkg_indx <- 1}
    if (Lrge) {X <- Norm_Dataset} else { X <- Dist}
    Clustering <- agnes(x = X ,diss = !Lrge, metric = Metrc , method = Agns_linkg[Lnkg_indx], keep.diss = F, keep.data = F)
    Cluster <- cutree(tree = Clustering,k = K)
    Name <- paste(Rpt,Algo,K,sep="")
    Distance <- Metrc
    Other <- Agns_linkg[Lnkg_indx]
    Lnkg_indx <- Lnkg_indx + 1
    
  }else if(Algo=="diana"){
    if (Lrge) {X <- Norm_Dataset} else { X <- Dist}
    Clustering <- diana(x = X , diss = !Lrge, metric = Metrc , keep.diss = F, keep.data = F)
    Cluster <- cutree(tree = as.hclust(Clustering),k = K)
    Name <- paste(Rpt,Algo,K,sep="")
    Distance <- Metrc
    
  }else if(Algo=="fanny"){
    if (Lrge) {X <- Norm_Dataset} else { X <- Dist}
    Clustering <- fanny(x = X , k = K, diss = !Lrge, metric = Metrc, stand = F, keep.diss = F, keep.data = F)
    Cluster <- Clustering$clustering
    Name <- paste(Rpt,Algo,length(unique(Cluster)),sep="")
    Distance <- Metrc
    
  }else if(Algo=="mclust"){
    
    Clustering <- Mclust(data = Norm_Dataset , G = K)
    Cluster <- Clustering$classification
    Name <- paste(Rpt,Algo,K,sep="")
    Other <- Clustering$modelName
    
  }else if(Algo=="bclust"){
    
    if (Lnkg_indx > N_linkg) {Lnkg_indx <- 1}
    Clustering <- bclust(x = Norm_Dataset , centers = K , iter.base = 20 , dist.method = Metrc , hclust.method = Hrch_linkg[Lnkg_indx] , base.method = "kmeans" , base.centers = max(20,K) , verbose = F)
    Cluster <- Clustering$cluster
    Name <- paste(Rpt,Algo,length(unique(Cluster)),sep="")
    Distance <- Metrc
    Other <- Hrch_linkg[Lnkg_indx]
    Lnkg_indx <- Lnkg_indx + 1
    
  }else if(Algo=="cmeans"){
    
    Clustering <- cmeans(x = Norm_Dataset , centers = K)
    Cluster <- Clustering$cluster
    Name <- paste(Rpt,Algo,length(unique(Cluster)),sep="")
    
  }else if(Algo=="speccl"){
    
    Clustering <- speccl(data = Norm_Dataset, nc = K, distance = Metrc) #other distance measures available
    Cluster <- Clustering$clusters
    Name <- paste(Rpt,Algo,K,sep="")
    Distance <- Metrc
    
#  }else if(Algo=="som"){
#    # needs adjustment, as a grid 1 X K is not correct. hclust can be used over a big grid to retrieve the final
#    # clustering of grid cells (see http://www.r-bloggers.com/self-organising-maps-for-customer-segmentation-using-r/)
#    Clustering <- som(as.matrix(Norm_Dataset),somgrid(xdim = K,ydim = 1,topo =  "hexagonal"),rlen = 100 , keep.data = T)
#    Cluster <- Clustering$unit.classif
#    Name <- paste(Rpt,Algo,K,sep="")
#    Other <- "hexagonal"
    
  }else{
    print(paste("Incorrect algorithm name:",Algo))
  }
  Methods <- rbind(Methods,data.frame(name=Name,distance=Distance,other=Other,stringsAsFactors = F))
  Distance <- Other <- "default"
  Results[[Clstrings]] <- Cluster
  cat("Algorithm:",Methods$name[Clstrings]," done.",'\n')      
}

source("refine_clusterings.R") # removes the base clustering that consist of 1 big cluster
Clstrings <- length(Results)
source("check_similar_clusterings.R") # removes identical clusterings
print("Remaining clusterings:")
for (i in 1:Clstrings)
{
  cat("Algorithm:",Methods$name[i],'\t',"- Distance:",Methods$distance[i],'\t', "- Other:",Methods$other[i],'\n')
}

source("ensemble_quality.R") # calcualtes in-ensemble similarity
cat("Ensemble quality = ",round(Avg_ensemble,digits = 2))
rm(Dist,Algo,Algo_indx,Algo_lst,Algo_lst_len)
rm(Clustering,Cluster,K,KL,Hrch_linkg,Lnkg_indx,N_linkg,Pam_Swp)

# Modified 2017-04-27
if(exists("X")){
  rm(X)
}

# Modified 2017-04-27}
rm(Name,Distance,Other,Lrge,Rpt)
if(exists("MinP")){
  rm(MinP)
}
