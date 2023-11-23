######################################################################
# BUILDS THE FIRST CONSENSUS: AGREEMENTS BETWEEN ALL BASE CLUSTERING #
######################################################################

# created: 19/08/2015

#cat("Building first consensus")                           # Debugging
#Bi_Clust <- FCI[FCI$CS_size == Clstrings,"Object.List"]   # Modified 2017-04-28 
Bi_Clust <- FCI[FCI$CS_size == max(FCI$CS_size),"Object.List"]
N_Row <- length(Bi_Clust)
ClustV <- rep(NA, times= Data_Size)
for(i in 1:N_Row)
{
  ClustV[Bi_Clust[[i]]] <- i
}
Cons_Vctrs[[Clstrings]] <- ClustV

#cat("First consensus built")                              # Debugging