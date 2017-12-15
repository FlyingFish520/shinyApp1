
#���ݶ�ȡ��ת��
nodes_ns <- read.table("data/nodes_nss.csv", header=TRUE, sep=",", as.is=TRUE)
edges_ns <- read.table("data/edges_nss.csv", header=TRUE, sep=",", as.is=TRUE)
nodes_ns$ID <- as.character(nodes_ns$ID)
nodes_ns$Names <- as.character(nodes_ns$Names)
edges_ns$Source <- as.character(edges_ns$Source)
edges_ns$Target <- as.character(edges_ns$Target)

#����ͼ����
library(igraph)
gg2 = graph.data.frame(d = edges_ns[,c(1,2)], directed = T, vertices = nodes_ns)
gg2 = simplify(gg2)


#��Ҫ
# sum2 = summary(gg2)
# den2 = graph.density(gg2)
# con2 = is.connected(gg2)
# d2 = diameter(gg2)
# len2 = average.path.length(gg2)
# dyce2 = dyad.census(gg2)

#�������Ķ�
dg_ns = sort(degree(gg2), decreasing = T)
dg2 = as.data.frame(dg_ns)
dg2$ranks = c(1:5254)
names(dg2)[1:2]<-c("������","����")

# table(sort(dg,decreasing = T))
# sort(degree(gg, mode = "in"), decreasing = T)  #�����
# table(sort(degree(gg, mode = "in"),decreasing = T))
# sort(degree(gg, mode = "out"), decreasing = T)   #�����
# table(sort(degree(gg, mode = "out"),decreasing = T))
# hist(dg)
# 
# nodes$Names[which(dg>220)]
# nodes$Names[which(nodes$ID==24)]
# nodes$Names[which(degree(gg, mode = "out")>200)]


#�н����Ķ�
bt_ns = sort(betweenness(gg2),decreasing = T)
bt2 = as.data.frame(bt_ns)
bt2$ranks = c(1:5254)
names(bt2)[1:2]<-c("��������ָ��","����")

# #�ӽ������Է���
# sort(closeness(gg),decreasing = T)
# sort(closeness(gg))

#�������������Է���
eigen_ns = sort(eigen_centrality(gg2,scale = T)$vector,decreasing = T)
eigen2 = as.data.frame(eigen_ns)
eigen2$ranks = c(1:5254)
names(eigen2)[1:2]<-c("Ǳ�ڼ�ֵ","����")

#����ͼת��
gg22 <- as.undirected(gg2)

#�������
fc2 <- cluster_fast_greedy(gg22)
topns1 = as.data.frame(fc2[[3]])
topns2 = as.data.frame(fc2[[7]])
topns3 = as.data.frame(fc2[[1]])
names(topns1)[1] = "ID"
names(topns2)[1] = "ID"
names(topns3)[1] = "ID"


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
# #��ͼ
# par(mar = c(3,3,3,3))
# set.seed(14)
# 
# plot(gg,layout = layout.auto,main="����ʫ�˹�ϵ����ͼ",
#      vertex.size = log(dg, 5)*1.5+2,
#      vertex.label = NA,
#      vertex.color = rainbow(max(fc$membership+1))[membership(fc)],
#      edge.color = grey(0.5),
#      edge.arrow.size = 0.2)
