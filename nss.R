
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


#概要
# sum2 = summary(gg2)
# den2 = graph.density(gg2)
# con2 = is.connected(gg2)
# d2 = diameter(gg2)
# len2 = average.path.length(gg2)
# dyce2 = dyad.census(gg2)

#度数中心度
dg_ns = sort(degree(gg2), decreasing = T)
dg2 = as.data.frame(dg_ns)
dg2$ranks = c(1:5254)
names(dg2)[1:2]<-c("人气度","排名")

# table(sort(dg,decreasing = T))
# sort(degree(gg, mode = "in"), decreasing = T)  #点入度
# table(sort(degree(gg, mode = "in"),decreasing = T))
# sort(degree(gg, mode = "out"), decreasing = T)   #点出度
# table(sort(degree(gg, mode = "out"),decreasing = T))
# hist(dg)
# 
# nodes$Names[which(dg>220)]
# nodes$Names[which(nodes$ID==24)]
# nodes$Names[which(degree(gg, mode = "out")>200)]


#中介中心度
bt_ns = sort(betweenness(gg2),decreasing = T)
bt2 = as.data.frame(bt_ns)
bt2$ranks = c(1:5254)
names(bt2)[1:2]<-c("控制能力指数","排名")

# #接近中心性分析
# sort(closeness(gg),decreasing = T)
# sort(closeness(gg))

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





# membership(fc)
# sizes(fc)
# sort(sizes(fc),decreasing = T)
# communities(fc)$'11'
# # 3   7   1  14   6  11
# 
# # "336"    "502"    "716"    "785"    "1161"
# nodes$Names[which(nodes$ID==785)]
# 
# c("3257","3595","3624","3640","4001",
#   "7055","7197","10258","10566","10749",
#   "7164","8078","11800") %in% communities(fc)$'3'
# 
# 
# #绘图
# par(mar = c(3,3,3,3))
# set.seed(14)
# 
# plot(gg,layout = layout.auto,main="南宋诗人关系网络图",
#      vertex.size = log(dg, 5)*1.5+2,
#      vertex.label = NA,
#      vertex.color = rainbow(max(fc$membership+1))[membership(fc)],
#      edge.color = grey(0.5),
#      edge.arrow.size = 0.2)

