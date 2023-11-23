N <- length(Stabl_Cons_Vctrs)
Similr <- rep(0,times=N)
for (i in 1:N)
{
  for (j in 1:Clstrings)
  {
    Sim <- cl_agreement(x= as.cl_hard_partition(as.cl_class_ids(Stabl_Cons_Vctrs[[i]])), y= as.cl_hard_partition(as.cl_class_ids(Results[[j]])), method = "Jaccard")
    Similr[[i]] <- Similr[[i]] + Sim
  }
  Similr[[i]] <- Similr[[i]]/Clstrings
}
Bst_Cons <- which.max(Similr)
Legnd_txt_2 <- NULL
Legnd_txt_2[1] <- paste("Advised: DT=",DT_range[Bst_Cons],'\n',"Similarity=",round(Similr[Bst_Cons],digits = 2))

# Tree quality
if (length(Cons_cls_size[[1]]$Var1) == 1)
{
  Tree_Qualty <- 1 - ((Stability[1]-1)/Clstrings)
}else{
  Tree_Qualty <- 1 
}

Legnd_txt_2[2] <- paste("Tree Quality=",round(Tree_Qualty,digits = 2))
Legnd_txt_2[3] <- paste("Ens. Quality=",round(Avg_ensemble,digits = 2))
rm(N,Tree_Qualty,Sim,i,j)
Legnd_txt <- c(Legnd_txt,Legnd_txt_2)

