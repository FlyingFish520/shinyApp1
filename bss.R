
#数据读取与转换
nodes_bs <- read.table("data/nodes_bss.csv", header=TRUE, sep=",", as.is=TRUE)
edges_bs <- read.table("data/edges_bss.csv", header=TRUE, sep=",", as.is=TRUE)
nodes_bs$ID <- as.character(nodes_bs$ID)
nodes_bs$Names <- as.character(nodes_bs$Names)
edges_bs$Source <- as.character(edges_bs$Source)
edges_bs$Target <- as.character(edges_bs$Target)

bdatasr = data.frame(nodes_bs$ID,nodes_bs$Names)
names(bdatasr)[1:2] = c("ID","诗人") 

#创建图对象
library(igraph)
gg1 = graph.data.frame(d = edges_bs[,c(1,2)], directed = T, vertices = nodes_bs)
gg1 = simplify(gg1)

#度数中心度
dg_bs = sort(degree(gg1), decreasing = T)
dg1 = as.data.frame(dg_bs)
dg1$ranks = c(1:3972)
names(dg1)[1:2]<-c("人气度","排名")

#中介中心度
bt_bs = sort(betweenness(gg1),decreasing = T)
bt1 = as.data.frame(bt_bs)
bt1$ranks = c(1:3972)
names(bt1)[1:2]<-c("控制能力指数","排名")

#特征向量中心性分析
eigen_bs = sort(eigen_centrality(gg1,scale = T)$vector,decreasing = T)
eigen1 = as.data.frame(eigen_bs)
eigen1$ranks = c(1:3972)
names(eigen1)[1:2]<-c("潜在价值","排名")


#无向图转换
gg12 <- as.undirected(gg1)

#聚类分析
fc1 <- cluster_fast_greedy(gg12)

btempname1 = NULL
for(i in fc1[[2]]){
  btempname1[i] = nodes_bs$Names[which(nodes_bs$ID==i)]
}

# btempname1
topbs1 = data.frame(btempname1)
names(topbs1) = "成员"

btempname2 = NULL
for(i in fc1[[9]]){
  btempname2[i] = nodes_bs$Names[which(nodes_bs$ID==i)]
}

# btempname2
topbs2 = data.frame(btempname2)
names(topbs2) = "成员"

btempname3 = NULL
for(i in fc1[[1]]){
  btempname3[i] = nodes_bs$Names[which(nodes_bs$ID==i)]
}

# btempname3
topbs3 = data.frame(btempname3)
names(topbs3) = "成员"

