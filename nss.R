
#数据读取与转换
nodes_ns <- read.table("data/nodes_nss.csv", header=TRUE, sep=",", as.is=TRUE)
edges_ns <- read.table("data/edges_nss.csv", header=TRUE, sep=",", as.is=TRUE)
nodes_ns$ID <- as.character(nodes_ns$ID)
nodes_ns$Names <- as.character(nodes_ns$Names)
edges_ns$Source <- as.character(edges_ns$Source)
edges_ns$Target <- as.character(edges_ns$Target)

ndatasr = data.frame(nodes_ns$ID,nodes_ns$Names)
names(ndatasr)[1:2] = c("ID","诗人") 

#创建图对象
library(igraph)
gg2 = graph.data.frame(d = edges_ns[,c(1,2)], directed = T, vertices = nodes_ns)
gg2 = simplify(gg2)

#度数中心度
dg_ns = sort(degree(gg2), decreasing = T)
dg2 = as.data.frame(dg_ns)
dg2$ranks = c(1:5254)
names(dg2)[1:2]<-c("人气度","排名")

#中介中心度
bt_ns = sort(betweenness(gg2),decreasing = T)
bt2 = as.data.frame(bt_ns)
bt2$ranks = c(1:5254)
names(bt2)[1:2]<-c("控制能力指数","排名")

#特征向量中心性分析
eigen_ns = sort(eigen_centrality(gg2,scale = T)$vector,decreasing = T)
eigen2 = as.data.frame(eigen_ns)
eigen2$ranks = c(1:5254)
names(eigen2)[1:2]<-c("潜在价值","排名")

#无向图转换
gg22 <- as.undirected(gg2)

#聚类分析
fc2 <- cluster_fast_greedy(gg22)

ntempname1 = NULL
for(i in fc2[[3]]){
  ntempname1[i] = nodes_ns$Names[which(nodes_ns$ID==i)]
}

# ntempname1
topns1 = data.frame(ntempname1)
names(topns1) = "成员"

ntempname2 = NULL
for(i in fc2[[7]]){
  ntempname2[i] = nodes_ns$Names[which(nodes_ns$ID==i)]
}

# ntempname2
topns2 = data.frame(ntempname2)
names(topns2) = "成员"

ntempname3 = NULL
for(i in fc2[[1]]){
  ntempname3[i] = nodes_ns$Names[which(nodes_ns$ID==i)]
}

# ntempname3
topns3 = data.frame(ntempname3)
names(topns3) = "成员"
