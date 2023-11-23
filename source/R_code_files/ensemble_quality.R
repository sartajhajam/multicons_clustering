library(clue)

Ens_Similr <- matrix(data = 0, nrow = Clstrings, ncol = Clstrings)
Avg_Similr <- rep(0,times=Clstrings)
for (i in 1:(Clstrings-1))
{
  for (j in (i+1):Clstrings)
  {
    Sim <- cl_agreement(x= as.cl_hard_partition(as.cl_class_ids(Results[[i]])), y= as.cl_hard_partition(as.cl_class_ids(Results[[j]])), method = "Jaccard")
    Ens_Similr[i,j] <- Ens_Similr[j,i] <- Sim
  }
  Avg_Similr[i] <- sum(Ens_Similr[i,])/(Clstrings-1)
}
Avg_Similr[Clstrings] <- sum(Ens_Similr[Clstrings,])/(Clstrings-1)
Avg_ensemble <- mean(Avg_Similr)
rm(Sim,i,j,Avg_Similr,Ens_Similr)