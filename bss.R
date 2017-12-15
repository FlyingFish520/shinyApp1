
#数据读取与转换
nodes_bs <- read.table("data/nodes_bss.csv", header=TRUE, sep=",", as.is=TRUE)
edges_bs <- read.table("data/edges_bss.csv", header=TRUE, sep=",", as.is=TRUE)
nodes_bs$ID <- as.character(nodes_bs$ID)
nodes_bs$Names <- as.character(nodes_bs$Names)
edges_bs$Source <- as.character(edges_bs$Source)
edges_bs$Target <- as.character(edges_bs$Target)

#创建图对象
library(igraph)
gg1 = graph.data.frame(d = edges_bs[,c(1,2)], directed = T, vertices = nodes_bs)
gg1 = simplify(gg1)

#概要
# sum1 = summary(gg1)
# den1 = graph.density(gg1)
# con1 = is.connected(gg1)
# d1 = diameter(gg1)
# len1 = average.path.length(gg1)
# dyce1 = dyad.census(gg1)


#度数中心度
dg_bs = sort(degree(gg1), decreasing = T)
dg1 = as.data.frame(dg_bs)
dg1$ranks = c(1:3972)
names(dg1)[1:2]<-c("人气度","排名")

# table(sort(dg,decreasing = T))
# sort(degree(gg, mode = "in"), decreasing = T)  #点入度
# table(sort(degree(gg, mode = "in"),decreasing = T))
# sort(degree(gg, mode = "out"), decreasing = T)   #点出度
# table(sort(degree(gg, mode = "out"),decreasing = T))
# hist(dg)
# 
# nodes$Names[which(dg>120)]
# nodes$Names[which(nodes$ID==17)]
# nodes$Names[which(degree(gg, mode = "out")>38)]

#中介中心度
bt_bs = sort(betweenness(gg1),decreasing = T)
bt1 = as.data.frame(bt_bs)
bt1$ranks = c(1:3972)
names(bt1)[1:2]<-c("控制能力指数","排名")

# nodes$Names[which(bt>1.452192e+05)]
# 
# #接近中心性分析
# sort(closeness(gg),decreasing = T)
# sort(closeness(gg))

#特征向量中心性分析
eigen_bs = sort(eigen_centrality(gg1,scale = T)$vector,decreasing = T)
eigen1 = as.data.frame(eigen_bs)
eigen1$ranks = c(1:3972)
names(eigen1)[1:2]<-c("潜在价值","排名")


#无向图转换
gg12 <- as.undirected(gg1)

#聚类分析
fc1 <- cluster_fast_greedy(gg12)
topbs1 = as.data.frame(fc1[[2]])
topbs2 = as.data.frame(fc1[[9]])
topbs3 = as.data.frame(fc1[[1]])
names(topbs1)[1] = "ID"
names(topbs2)[1] = "ID"
names(topbs3)[1] = "ID"




# membership(fc)
# sizes(fc)
# sort(sizes(fc),decreasing = T)
# communities(fc)$'15'
# #2   9   1  17  16  15
# 
# #"58"    "114"   "233"   "321"   "535"
# nodes$Names[which(nodes$ID==535)]
# c("1488") %in% communities(fc)$'16'
# 
# #绘图
# par(mar = c(3,3,3,3))
# set.seed(14)
# 
# plot(gg,layout = layout.auto,main="北宋诗人关系网络图",
#      vertex.size = log(dg, 5)*1.5+2,
#      vertex.label = NA,
#      vertex.color = rainbow(max(fc$membership+1))[membership(fc)],
#      edge.color = grey(0.5),
#      edge.arrow.size = 0.2)
