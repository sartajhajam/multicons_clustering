################################################
# BUILDS THE CONSTREE GRAPHICAL REPRESENTATION #
################################################

library(igraph)

# Define margins
par(mai=c(0.1,0,0.3,0))

# ConsTree levels colors 
palette(rainbow(length(Stabl_Cons_Vctrs)))

Vrtx_Sz <- 10
Plt_Sz <- 0.9
Lgnd_Sz <- 0.8

N_Col <- length(Stabl_Cons_Vctrs)
Cls_connctions <- unique(data.frame(Stabl_Cons_Vctrs))
colnames(Cls_connctions) <- c(1:N_Col)
Cls_connctions <- Cls_connctions[complete.cases(Cls_connctions),]

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
    Graph_Val <- c(Graph_Val,Cons_cls_size[[i]]$Freq[j])    
  }
  Graph_Col <- c(Graph_Col,rep(i,length(Cons_cls_size[[i]]$Freq)))
}

Rt <- c(1:max(Stabl_Cons_Vctrs[[1]],na.rm = T))

Cons_graph <- graph_from_data_frame(Graph_frm, directed = FALSE, vertices = NULL)
Cons_graph <- set_vertex_attr(Cons_graph, "value", value = Graph_Val)
Cons_graph <- set_vertex_attr(Cons_graph, "color", value = Graph_Col)
Cons_graph <- set_vertex_attr(Cons_graph, "size", value = (vertex_attr(Cons_graph,"value")*Vrtx_Sz/length(Stabl_Cons_Vctrs[[1]])))
gLayout <- layout_as_tree(Cons_graph,root = Rt)
Grp_Bst <- which(V(Cons_graph)$color==Bst_Cons)


plot(Cons_graph,layout = gLayout,main = "ConsTree", mark.groups = V(Cons_graph)[Grp_Bst], mark.col=0, mark.expand= Vrtx_Sz*3, vertex.label = vertex_attr(Cons_graph,"value"),vertex.size = vertex_attr(Cons_graph,"size"),vertex.color=vertex_attr(Cons_graph,"color"),vertex.shape = "circle",vertex.label.cex=Plt_Sz,vertex.label.dist=0.5,vertex.label.degree=1.5,asp=0, xlim=c(-1,1.5),ylim=c(-1.1,1.1),palette = rainbow(length(Stabl_Cons_Vctrs)))

L <- length(Legnd_txt)
if (L > 13) {Lgnd_Sz <- Lgnd_Sz -((L - 12)/20)}
if (Lgnd_Sz < 0.5) {Lgnd_Sz <- 0.5}
legend(x = "topright",legend = Legnd_txt ,fill = c(unique(V(Cons_graph)$color),0,0,0),cex=Lgnd_Sz,bty='n')
if ("Legnd_txt2" %in% ls()) {legend(x = "topleft",legend = Legnd_txt2 ,fill = 0,cex=1,bty='n')}
#if ("Legnd_txt_2" %in% ls()) {legend(x = "bottomright",legend = Legnd_txt_2 ,fill = 0,cex=Lgnd_Sz,bty='n')}
rm(Graph_frm,Graph_Val,Sub,i,j,gLayout,Sub_connctions,Cls_connctions,Graph_Col,Lgnd_Sz,Plt_Sz,Vrtx_Sz,N_Col,Rt,Grp_Bst,L)
par(mai=c(1.2,1.2,1,0.5))
