########################################################
# BUILDS THE REFINED CONSTREE GRAPHICAL REPRESENTATION #
########################################################

library(igraph)

# Define margins
par(mai=c(0.10,0,0.30,0))

# ConsTree levels colors 
palette(rainbow(length(Stabl_Cons_Vctrs)))

Vrtx_Sz <- 9
Plt_Sz <- 1
Lgnd_Sz <- 0.9

Cls_connctions <- data.frame(Stabl_Cons_Vctrs)
colnames(Cls_connctions) <- c(1:length(Stabl_Cons_Vctrs))
N_Col <- ncol(Cls_connctions)
Cons_cls_size2 <- list() 
Rmv_Inst <- NULL # list of conflicting instances that will be removed

for (i in N_Col:2) # it can start from N_Col-1 because at the 1st cons, there is no cluster that separates into 2 at the next level
{
  Sub_connctions <- data.frame(f= Cls_connctions[,i],t= Cls_connctions[,i-1])
  Freq <- data.frame(table(Sub_connctions))
  Freq <- Freq[Freq$Freq > 0,]
  Freq <- Freq[order(Freq$f,-Freq$Freq),]
  Prev <- 0
  for(j in 1:nrow(Freq))
  { 
    if (Freq$f[j] == Prev)
      {
        Rmv_Inst <- c(Rmv_Inst,which(Cls_connctions[,i]==Freq$f[j] & Cls_connctions[,i-1]==Freq$t[j]))
      }
    Prev <- Freq$f[j]  
  }
}


Rmv_Inst <- unique(Rmv_Inst)

for (i in 1:length(Stabl_Cons_Vctrs))
{
  Cons_cls_size2[[i]] <- data.frame(table(Stabl_Cons_Vctrs[[i]][-Rmv_Inst]))
}
Cls_connctions <- Cls_connctions[-Rmv_Inst,]

# build unique names for verteces in the graph, and add cluster size and color to the graph as additional meta data
Graph_frm <- data.frame(f=NULL,t=NULL) # data frame that defines the connections between clusters: f=from, t=to
Graph_Val <- NULL # cluster size of each vertex "cluster" in the graph
Graph_Col <- NULL # color for each node correspond to the consensus it belongs to
for (i in 1:N_Col)
{
  if (i< N_Col)
  {
    Sub_connctions <- unique.matrix(data.frame(f= Cls_connctions[,i],t= Cls_connctions[,i+1]))
    Graph_frm <- rbind(Graph_frm , data.frame(f= paste("L",i,Sub_connctions[,1]),t= paste("L",i+1,Sub_connctions[,2])))
  }
  Sub <- unique(Cls_connctions[,i])
  for (j in Sub)
  {
    Tmp <- Cons_cls_size2[[i]]
    Graph_Val <- c(Graph_Val,Tmp$Freq[Tmp$Var1==j])    
  }
  Graph_Col <- c(Graph_Col,rep(i,length(Cons_cls_size2[[i]]$Freq)))
}

Rt <- c(1:max(Stabl_Cons_Vctrs[[1]],na.rm = T))

Cons_graph <- graph_from_data_frame(Graph_frm, directed = FALSE, vertices = NULL)
Cons_graph <- set_vertex_attr(Cons_graph, "value", value = Graph_Val)
Cons_graph <- set_vertex_attr(Cons_graph, "color", value = Graph_Col)
Cons_graph <- set_vertex_attr(Cons_graph, "size", value = (vertex_attr(Cons_graph,"value")*Vrtx_Sz/length(Stabl_Cons_Vctrs[[1]])))
gLayout <- layout_as_tree(Cons_graph,root = Rt)
Grp_Bst <- which(V(Cons_graph)$color==Bst_Cons)


plot(Cons_graph,layout = gLayout,main = "ConsTree", mark.groups = V(Cons_graph)[Grp_Bst], mark.col=0, mark.expand= Vrtx_Sz*3, vertex.label = vertex_attr(Cons_graph,"value"),vertex.size = vertex_attr(Cons_graph,"size"),vertex.color=vertex_attr(Cons_graph,"color"),vertex.shape = "circle",vertex.label.cex=Plt_Sz,vertex.label.dist=0.5,vertex.label.degree=1.5,asp=0,xlim=c(-1,1.9),ylim=c(-1.2,1),palette = rainbow(length(Stabl_Cons_Vctrs)))
L <- length(Legnd_txt)
if (L > 13) {Lgnd_Sz <- Lgnd_Sz -((L - 12)/20)}
if (Lgnd_Sz < 0.5) {Lgnd_Sz <- 0.5}
legend(x = "topright",legend = Legnd_txt ,fill = c(unique(V(Cons_graph)$color),0,0,0),cex=Lgnd_Sz,bty='n')

legend(x = "bottom",legend = paste("# RI =",length(Rmv_Inst)) ,cex=1,bty='n')

rm(Graph_frm,Graph_Val,Sub,i,j,gLayout,Sub_connctions,Cls_connctions,Graph_Col,Lgnd_Sz,Vrtx_Sz,N_Col,Cons_cls_size2,Tmp,Rt,Prev,Freq)
par(mai=c(1.2,1.2,1,0.5))
