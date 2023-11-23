##########################
# MCC APPROACH VERSION 2 #
##########################

# Consensus Clustering: Method 2-ae
# created 19/08/2015, updated 25/01/2016

library(clue)

T1 <- Sys.time()

# build first consensus
Cons_Vctrs <- list()
source("build_first_cons.R")

# build multiple consensuses:
DT_range <- c((Clstrings-1):1)
for (DT in DT_range)
{
  Bi_Clust <- c(Bi_Clust,FCI[FCI$CS_size == DT,"Object.List"])
  N_Row <- length(Bi_Clust)
  repeat
  {
    Rpt_Chk <- F
    for(i in 1:N_Row)
    {
      Xi <- Bi_Clust[[i]]
      XiL <- length(Xi)
      if (XiL==0){next}
      for (j in 1:N_Row)
      {
        if (j==i){next}
        Xj <- Bi_Clust[[j]]
        XjL <- length(Xj)
        Intrs_Size <-length(intersect(Xi,Xj))
        if ( Intrs_Size==0 || XjL==0 ){next}
        else if (Intrs_Size==XiL)
        {
          Bi_Clust[[i]]<-integer(0)   # set Xi to empty to remove it later
          break
        }
        else if (Intrs_Size==XjL)
        {
          Bi_Clust[[j]]<-integer(0)   # set Xj to empty to remove it later
          next
        }
        else if ((Intrs_Size >= XiL*MT)||(Intrs_Size >= XjL*MT))
        { # merge bi_clusters Xi and Xj
          Rpt_Chk <- T
          Bi_Clust[[j]] <- sort(union(Xi,Xj))
          Bi_Clust[[i]] <- integer(0)
          break
        }  
        else  
        { # split bi_clusters Xi and Xj
          Rpt_Chk <- T
          if (XiL <= XjL)
          { 
            Bi_Clust[[j]] <- setdiff(Xj,Xi)
          }
          else
          {
            Xi <- Bi_Clust[[i]] <- setdiff(Xi,Xj)
            XiL <- length(Xi)
          }
        }
      } # end for j
    } # end for i
    if (Rpt_Chk==F) {break}
  } # end repeat
  # Build final clusters
  Bi_Clust <- unique(Bi_Clust)
  Bi_Clust <- Bi_Clust[sapply(Bi_Clust,length)>0]
  N_Row <- length(Bi_Clust)
  ClustV <- NA
  for(i in 1:N_Row)
  {
    Indx <- Bi_Clust[[i]]
    if (all(is.na(ClustV[Indx]))) {ClustV[Indx] <- i} else {cat("error! cluster vector overwritten at DT =",DT,", final Bi-clusters are not unique.",'\n')}
  }
  Cons_Vctrs[[DT]] <- ClustV
}

DT_range <- c(1:Clstrings)



rm(Bi_Clust,ClustV,DT,Intrs_Size,Xi,Xj,XiL,XjL,N_Row,Rpt_Chk)

Stability <- 1
# calculate the stability of consensus results
for(i in Clstrings:2)
{
  if (is.na(Cons_Vctrs[i])){next}
  SimC <- 1
  for (j in (i-1):1)
  {
    if (is.na(Cons_Vctrs[j])){next}
    Simlr <- cl_agreement(x= as.cl_hard_partition(as.cl_class_ids(Cons_Vctrs[[i]])), y= as.cl_hard_partition(as.cl_class_ids(Cons_Vctrs[[j]])), method = "Jaccard")
    if (Simlr==1)
    {
      SimC <- SimC + 1
      Cons_Vctrs[[j]] <- NA
      Stability[[j]] <- NA
      DT_range[j] <- NA
    }    
  }
  Stability[i] <- SimC
}

Stabl_Cons_Vctrs <- Cons_Vctrs[!is.na(Cons_Vctrs)]
Stability <- Stability[!is.na(Stability)]
DT_range <- na.omit(DT_range) 

rm(Cons_Vctrs,SimC,Simlr)

Cons_cls_size <- list() # clusters sizes for all consensuses
for (i in 1:length(Stabl_Cons_Vctrs))
{
  Cons_cls_size[[i]] <- data.frame(table(Stabl_Cons_Vctrs[i]))
}

Legnd_txt <- NULL
for(i in length(DT_range):1)
{
  cat("DT=",DT_range[i],"     ST=",Stability[i],"     N of clusters =",max(Stabl_Cons_Vctrs[[i]]),"\n")
  cat("Clusters sizes:")
  print(table(Stabl_Cons_Vctrs[[i]]))
  cat("___________________________________________\n\n")
  Legnd_txt[i]<-paste("DT=",DT_range[i],", ST=",Stability[i])  
}

source("median_cons.R")

T2 <- Sys.time()

Tme <- difftime(T2,T1,units = "sec")
cat("method 2 execution time",Tme,"sec.",'\n')
rm(T1,T2)
