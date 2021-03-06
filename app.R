
#加载包
library(shiny)
library(DT)

# source("bss.R")
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


# source("nss.R")
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


# Define UI ----
ui <- fluidPage(
  
  sidebarLayout(
   
    
    #侧边栏
    sidebarPanel(
      titlePanel("宋代诗人社会网络"),
      helpText("利用中国历代人物传记数据库，以诗人互赠诗文建立关系，构建北宋和南宋诗人互赠诗文关系网络。利用社会网络分析（SNA）法，研究北宋和南宋诗人互赠诗文关系网络的结构特征。结合诗人及朝代背景，分析宋代诗词在诗人之间的传播特征。从而在以传统感性为主的人文学科研究基础上引入理性技术手段，将定量分析与定性分析融为一体，体现“数字人文”的思想，拓展了宋代诗词文化领域的研究。"),
      br(),
      selectInput("dynasties", label = h4(strong("朝代：")),choices = c("北宋","南宋"),selected = "北宋"),
      br(),
      br(),
      
      h4(strong("热点聚焦：")),
      wellPanel(
        fluidRow(
          column(6,
                 h4("豪放派："),
                 br(),
                 textOutput("bold1"),
                 br(),
                 textOutput("bold2"),
                 br(),
                 textOutput("bold3"),
                 br(),
                 textOutput("bold4"),
                 br(),
                 textOutput("bold5"),
                 br(),
                 textOutput("bold6")),
          column(6,
                 h4("婉约派："),
                 br(),
                 textOutput("graceful1"),
                 br(),
                 textOutput("graceful2"),
                 br(),
                 textOutput("graceful3"),
                 br(),
                 textOutput("graceful4"),
                 br(),
                 textOutput("graceful5"),
                 br(),
                 textOutput("graceful6"))
        )
      ),
      br(),
      hr(),
      
      textInput("poetNum", label = h4(strong("搜索：")), value = "请输入诗人ID或姓名"),
      h5("查询结果："),
      verbatimTextOutput("name"),
      br(),
      h5(textOutput("currentTime")),
      img(src = "1.jpg", height = 50,width="100%")
    ),
    
    #主面板
    mainPanel(
      navbarPage(
        title = "SNASR：",
        
        tabPanel("网络图",
          wellPanel(
            fluidRow(
              column(12,
                     imageOutput("images",height = 900),
                     br()),
              
              column(5,offset = 7,
                     textOutput("note"))
            )
          )
        ),
        tabPanel("诗人查询",
          fluidRow(
            column(6,
                   textInput("search", label = h4(strong("诗人：")), value = "请输入诗人ID或姓名")),
            column(6,
                   helpText(strong("热门搜索："),br(),"辛弃疾，苏轼，王安石，李清照，秦观，陆游，李煜，黄庭坚，欧阳修，范仲淹，朱熹，周必大"))
          ),
          hr(),
          h4("基本信息："),
          verbatimTextOutput("resultID"),
          hr(),
          h4("诗人网络图："),
          wellPanel(plotOutput("SNA",height = 1200))
        ),
        
        tabPanel("关系查询",
          h5("注解：输入两位诗人，查询在当前朝代下他们之间是否存在联系，是主动关系还是被动关系"),
          br(),
          textInput("poet1", label = h4(strong("诗人1：")), value = "请输入诗人1的ID或姓名"),
          textInput("poet2", label = h4(strong("诗人2：")), value = "请输入诗人2的ID或姓名"),    
          hr(),
          h5("诗人1 -> 诗人2："),
          wellPanel(verbatimTextOutput("rel1")),
          br(),
          h5("诗人1 <- 诗人2："),
          wellPanel(verbatimTextOutput("rel2")),
          br(),
          h5("当前朝代："),
          wellPanel(verbatimTextOutput("currentDyn"))       
        ),
        
        tabPanel("排行榜",
            fluidRow(
              column(4,wellPanel(
                h3(strong("人气度"),align = "center"),
                helpText("该诗人与多少诗人相互关联，数值由点度中心度计算"),
                h4(strong("1"),align = "center"),
                wellPanel(
                  fluidRow(
                    column(6,
                           wellPanel(imageOutput("imgdg1",height = 100))
                    ),
                    column(6,
                           h4(strong(textOutput("namedg1"),align="center")),
                           h5(textOutput("detaildg1_id"),align="center"),
                           h5(textOutput("detaildg1_life"),align="center"),
                           h5(textOutput("detaildg1_value"),align="center")
                    )
                  ),
                  h5(textOutput("detaildg1_info"))
                ),
                h4(strong("2"),align = "center"),
                wellPanel(
                  fluidRow(
                    column(6,
                           wellPanel(imageOutput("imgdg2",height = 100))
                    ),
                    column(6,
                           h4(strong(textOutput("namedg2"),align="center")),
                           h5(textOutput("detaildg2_id"),align="center"),
                           h5(textOutput("detaildg2_life"),align="center"),
                           h5(textOutput("detaildg2_value"),align="center")
                    )
                  ),
                  h5(textOutput("detaildg2_info"))
                ),
                h4(strong("3"),align = "center"),
                wellPanel(
                  fluidRow(
                    column(6,
                           wellPanel(imageOutput("imgdg3",height = 100))
                    ),
                    column(6,
                           h4(strong(textOutput("namedg3"),align="center")),
                           h5(textOutput("detaildg3_id"),align="center"),
                           h5(textOutput("detaildg3_life"),align="center"),
                           h5(textOutput("detaildg3_value"),align="center")
                    )
                  ),
                  h5(textOutput("detaildg3_info"))
                )
              )),
              
              column(4,wellPanel(
                h3(strong("控制能力指数"),align = "center"),
                helpText("其他诗人相互联系要经过的最少人数中是否都包含该诗人，强调该诗人在其他诗人关联之间的调节控制能力，数值由中介中心度计算"),
                h4(strong("1"),align = "center"),
                wellPanel(
                  fluidRow(
                    column(6,
                           wellPanel(imageOutput("imgbt1",height = 100))
                    ),
                    column(6,
                           h4(strong(textOutput("namebt1"),align="center")),
                           h5(textOutput("detailbt1_id"),align="center"),
                           h5(textOutput("detailbt1_life"),align="center"),
                           h5(textOutput("detailbt1_value"),align="center")
                    )
                  ),
                  h5(textOutput("detailbt1_info"))
                ),
                h4(strong("2"),align = "center"),
                wellPanel(
                  fluidRow(
                    column(6,
                           wellPanel(imageOutput("imgbt2",height = 100))
                    ),
                    column(6,
                           h4(strong(textOutput("namebt2"),align="center")),
                           h5(textOutput("detailbt2_id"),align="center"),
                           h5(textOutput("detailbt2_life"),align="center"),
                           h5(textOutput("detailbt2_value"),align="center")
                    )
                  ),
                  h5(textOutput("detailbt2_info"))
                ),
                h4(strong("3"),align = "center"),
                wellPanel(
                  fluidRow(
                    column(6,
                           wellPanel(imageOutput("imgbt3",height = 100))
                    ),
                    column(6,
                           h4(strong(textOutput("namebt3"),align="center")),
                           h5(textOutput("detailbt3_id"),align="center"),
                           h5(textOutput("detailbt3_life"),align="center"),
                           h5(textOutput("detailbt3_value"),align="center")
                    )
                  ),
                  h5(textOutput("detailbt3_info"))
                )
              )),
              
              column(4,wellPanel(
                h3(strong("潜在价值"),align = "center"),
                helpText("根据相邻诗人的重要性来衡量该诗人的价值，数值由特征向量中心度计算"),
                h4(strong("1"),align = "center"),
                wellPanel(
                  fluidRow(
                    column(6,
                           wellPanel(imageOutput("imgegin1",height = 100))
                    ),
                    column(6,
                           h4(strong(textOutput("nameegin1"),align="center")),
                           h5(textOutput("detailegin1_id"),align="center"),
                           h5(textOutput("detailegin1_life"),align="center"),
                           h5(textOutput("detailegin1_value"),align="center")
                    )
                  ),
                  h5(textOutput("detailegin1_info"))
                ),
                h4(strong("2"),align = "center"),
                wellPanel(
                  fluidRow(
                    column(6,
                           wellPanel(imageOutput("imgegin2",height = 100))
                    ),
                    column(6,
                           h4(strong(textOutput("nameegin2"),align="center")),
                           h5(textOutput("detailegin2_id"),align="center"),
                           h5(textOutput("detailegin2_life"),align="center"),
                           h5(textOutput("detailegin2_value"),align="center")
                    )
                  ),
                  h5(textOutput("detailegin2_info"))
                ),
                h4(strong("3"),align = "center"),
                wellPanel(
                  fluidRow(
                    column(6,
                           wellPanel(imageOutput("imgegin3",height = 100))
                    ),
                    column(6,
                           h4(strong(textOutput("nameegin3"),align="center")),
                           h5(textOutput("detailegin3_id"),align="center"),
                           h5(textOutput("detailegin3_life"),align="center"),
                           h5(textOutput("detailegin3_value"),align="center")
                    )
                  ),
                  h5(textOutput("detailegin3_info"))
                )
              ))
            )
        ),
        
        tabPanel("帮派",
          fluidRow(
              column(5,
                     selectInput("top", 
                                 label = h4(strong("三大帮派：")),choices = c("1","2","3"),selected = "1"),
                     h5("注：展示当前朝代下紧密联系最大的三个诗人群体及其诗词中共有的风格特点")
              ),
              column(7,
                     
                     wellPanel(
                       h4("帮派信息："),
                       textOutput("info"))   
                     
              )
            ),   
          hr(),
          dataTableOutput("community"),
          wellPanel(plotOutput("bpSNA",height = 1200))
        ),
        
        tabPanel("诗人对照表",
          dataTableOutput("dataID")
        )
        
      )
    )
  )
)

# Define server logic ----
server <- function(input, output,session) {
  #侧边栏词派
  output$bold1 <- renderText({
    if(input$dynasties=="北宋"){
      return("范仲淹")
    }else{
      return("辛弃疾")
    }
  })
  output$bold2 <- renderText({
    if(input$dynasties=="北宋"){
      return("苏轼")
    }else{
      return("张元干")
    }
  })
  output$bold3 <- renderText({
    if(input$dynasties=="北宋"){
      return("贺铸")
    }else{
      return("张孝祥")
    }
  })
  output$bold4 <- renderText({
    if(input$dynasties=="北宋"){
      return("李纲")
    }else{
      return("陆游")
    }
  })
  output$bold5 <- renderText({
    if(input$dynasties=="北宋"){
      return("陈与义")
    }else{
      return("刘克庄")
    }
  })
  output$bold6 <- renderText({
    if(input$dynasties=="北宋"){
      return("叶梦得")
    }else{
      return("陈与义")
    }
  })
  
  output$graceful1 <- renderText({
    if(input$dynasties=="北宋"){
      return("晏殊")
    }else{
      return("李煜")
    }
  })
  output$graceful2 <- renderText({
    if(input$dynasties=="北宋"){
      return("秦观")
    }else{
      return("朱敦儒")
    }
  })
  output$graceful3 <- renderText({
    if(input$dynasties=="北宋"){
      return("欧阳修")
    }else{
      return()
    }
  })
  output$graceful4 <- renderText({
    if(input$dynasties=="北宋"){
      return("李清照")
    }else{
      return()
    }
  })
  output$graceful5 <- renderText({
    if(input$dynasties=="北宋"){
      return("周邦彦")
    }else{
      return()
    }
  })
  output$graceful6 <- renderText({
    if(input$dynasties=="北宋"){
      return("晏几道")
    }else{
      return()
    }
  })
  
  #侧栏诗人查询
  output$name <- renderPrint({
    if(input$dynasties=="北宋"){
      if(input$poetNum%in%nodes_bs$ID){
        x1 = input$poetNum
        x2 = nodes_bs$Names[which(nodes_bs$ID==x1)]
        
      }else if(input$poetNum%in%nodes_bs$Names){
        x2 = input$poetNum
        x1 = nodes_bs$ID[which(nodes_bs$Names==x2)]
        
      }else if(input$poetNum=="请输入诗人ID或姓名"){
        return()
      }else{
        return("输入有误或者朝代不符！")
      }
    }else{
      if(input$poetNum%in%nodes_ns$ID){
        x1 = input$poetNum
        x2 = nodes_ns$Names[which(nodes_ns$ID==x1)]
        
      }else if(input$poetNum%in%nodes_ns$Names){
        x2 = input$poetNum
        x1 = nodes_ns$ID[which(nodes_ns$Names==x2)]
        
      }else if(input$poetNum=="请输入诗人ID或姓名"){
        return()
      }else{
        return("输入有误或者朝代不符！")
      }
    }
    paste(x1,x2,sep = ",")

  })
  
  #当前时间
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste(Sys.time())
  })
  
  #网络图
  output$images <- renderImage({ 

      if(input$dynasties=="北宋"){
        return(
          list(src="www/bss.png",height = "100%",width="100%",title="北宋，涉及3972位诗人，6679对关系。图密度为0.0004234502，非常稀疏。非连通图，诗人相互联系时最多经过9人，平均经过4人。该有向图存在585对诗人互赠诗文。")
        )
      }
      else{
        return(
          list(src="www/nss.png",height = "100%",width="100%",title="南宋，涉及5254位诗人，9086对关系。图密度为0.0003292117，非常稀疏。非连通图，诗人相互联系时最多经过9人，平均经过4人。该有向图存在1065对诗人互赠诗文。")
        )
      }
    
  }, deleteFile = FALSE)
  output$note <- renderText({
    if(input$dynasties=="北宋"){
      paste("注：7111 - 黄庭坚，3767 - 苏轼，1384 - 欧阳修，...")
    }else{
      paste("注：3595 - 刘克庄，7197 - 周必大，3257 - 朱熹，...")
    }
  })
  
  #页面诗人查询
  output$resultID <- renderPrint({
    if(input$dynasties=="北宋"){
      if(input$search%in%nodes_bs$ID){
        x1 = input$search
        x2 = nodes_bs$Names[which(nodes_bs$ID==x1)]
        x3 = input$dynasties
        list(ID=x1,姓名=x2,当前朝代=x3)
      }else if(input$search%in%nodes_bs$Names){
        x2 = input$search
        x1 = nodes_bs$ID[which(nodes_bs$Names==x2)]
        x3 = input$dynasties
        list(ID=x1,姓名=x2,当前朝代=x3)
      }else if(input$search=="请输入诗人ID或姓名"){
        list("ID","姓名","当前朝代")
      }else{
        return("输入有误或者朝代不符，请修改！")
      }
    }else{
      if(input$search%in%nodes_ns$ID){
        x1 = input$search
        x2 = nodes_ns$Names[which(nodes_ns$ID==x1)]
        x3 = input$dynasties
        list(ID=x1,姓名=x2,当前朝代=x3)
      }else if(input$search%in%nodes_ns$Names){
        x2 = input$search
        x1 = nodes_ns$ID[which(nodes_ns$Names==x2)]
        x3 = input$dynasties
        list(ID=x1,姓名=x2,当前朝代=x3)
      }else if(input$search=="请输入诗人ID或姓名"){
        list("ID","姓名","当前朝代")
      }else{
        return("输入有误或者朝代不符，请修改！")
      }
    }

  })
  output$SNA <- renderPlot({
    if(input$dynasties=="北宋"){
      if(input$search%in%nodes_bs$ID || input$search%in%nodes_ns$ID){
        x1 = input$search
        x2 = nodes_bs$Names[which(nodes_bs$ID==x1)]
      }else if(input$search%in%nodes_bs$Names || input$search%in%nodes_ns$Names){
        x2 = input$search
        x1 = nodes_bs$ID[which(nodes_bs$Names==x2)]
      }else{
        return()
      }
      gn<-graph.neighborhood(gg1, order=1)
      gn1 = gn[[which(V(gg1)$name==x1)]]
      plot(gn1,width="100%",height="100%",layout=layout.auto,
           vertex.size = ifelse(V(gn1)$name==x1,15,5),vertex.color=ifelse(V(gn1)$name==x1,"red","#80B695"),
           vertex.label.dist=0.2,vertex.label=V(gn1)$Names,vertex.label.cex=1.3,vertex.label.color="black",
           edge.color = "#80B695",edge.arrow.size = 0.4)
      
    }else{
      if(input$search%in%nodes_bs$ID || input$search%in%nodes_ns$ID){
        x1 = input$search
        x2 = nodes_ns$Names[which(nodes_ns$ID==x1)]
      }else if(input$search%in%nodes_bs$Names || input$search%in%nodes_ns$Names){
        x2 = input$search
        x1 = nodes_ns$ID[which(nodes_ns$Names==x2)]
      }else{
        return()
      }
      gn<-graph.neighborhood(gg2, order=1)
      gn1 = gn[[which(V(gg2)$name==x1)]]
      plot(gn1,width="100%",height="100%",layout=layout.auto,
           vertex.size = ifelse(V(gn1)$name==x1,15,5),vertex.color=ifelse(V(gn1)$name==x1,"red","#80B695"),
           vertex.label.dist=0.2,vertex.label=V(gn1)$Names,vertex.label.cex=1.3,vertex.label.color="black",
           edge.color = "#80B695",edge.arrow.size = 0.4)
    }
  })
  
  #关系查询
  output$rel1 <- renderPrint({
    if(input$dynasties=="北宋"){
      if(input$poet1%in%nodes_bs$ID && input$poet2%in%nodes_bs$ID){
        x1 = input$poet1
        x2 = nodes_bs$Names[which(nodes_bs$ID==x1)]
        x3 = input$poet2
        x4 = nodes_bs$Names[which(nodes_bs$ID==x3)]
      }else if(input$poet1%in%nodes_bs$Names && input$poet2%in%nodes_bs$Names){
        x2 = input$poet1
        x1 = nodes_bs$ID[which(nodes_bs$Names==x2)]
        x4 = input$poet2
        x3 = nodes_bs$ID[which(nodes_bs$Names==x4)]
      }else if(input$poet1%in%nodes_bs$ID && input$poet2%in%nodes_bs$Names){
        x1 = input$poet1
        x2 = nodes_bs$Names[which(nodes_bs$ID==x1)]
        x4 = input$poet2
        x3 = nodes_bs$ID[which(nodes_bs$Names==x4)]
      }else if(input$poet1%in%nodes_bs$Names && input$poet2%in%nodes_bs$ID){
        x2 = input$poet1
        x1 = nodes_bs$ID[which(nodes_bs$Names==x2)]
        x3 = input$poet2
        x4 = nodes_bs$Names[which(nodes_bs$ID==x3)]
      }else{
        return()
      }
      
      tempOut = neighbors(gg1, x1,mode = "out") 
      if(x3 %in% tempOut$name){
        paste(x2," - > ",x4,":","主动关系，",x2,"为",x4,"作诗词碑赋，提及到该诗人")
      }else{
        return()
      }
      
    }else{
      if(input$poet1%in%nodes_ns$ID && input$poet2%in%nodes_ns$ID){
        x1 = input$poet1
        x2 = nodes_ns$Names[which(nodes_ns$ID==x1)]
        x3 = input$poet2
        x4 = nodes_ns$Names[which(nodes_ns$ID==x3)]
      }else if(input$poet1%in%nodes_ns$Names && input$poet2%in%nodes_ns$Names){
        x2 = input$poet1
        x1 = nodes_ns$ID[which(nodes_ns$Names==x2)]
        x4 = input$poet2
        x3 = nodes_ns$ID[which(nodes_ns$Names==x4)]
      }else if(input$poet1%in%nodes_ns$ID && input$poet2%in%nodes_ns$Names){
        x1 = input$poet1
        x2 = nodes_ns$Names[which(nodes_ns$ID==x1)]
        x4 = input$poet2
        x3 = nodes_ns$ID[which(nodes_ns$Names==x4)]
      }else if(input$poet1%in%nodes_ns$Names && input$poet2%in%nodes_ns$ID){
        x2 = input$poet1
        x1 = nodes_ns$ID[which(nodes_ns$Names==x2)]
        x3 = input$poet2
        x4 = nodes_ns$Names[which(nodes_ns$ID==x3)]
      }else{
        return()
      }
      
      tempOut = neighbors(gg2, x1,mode = "out") 
      if(x3 %in% tempOut$name){
        paste(x2," - > ",x4,":","主动关系，",x2,"为",x4,"作诗词碑赋，提及到该诗人")
      }else{
        return()
      }
      
    }

    

  })
  output$rel2 <- renderPrint({
    if(input$dynasties=="北宋"){
      if(input$poet1%in%nodes_bs$ID && input$poet2%in%nodes_bs$ID){
        x1 = input$poet1
        x2 = nodes_bs$Names[which(nodes_bs$ID==x1)]
        x3 = input$poet2
        x4 = nodes_bs$Names[which(nodes_bs$ID==x3)]
      }else if(input$poet1%in%nodes_bs$Names && input$poet2%in%nodes_bs$Names){
        x2 = input$poet1
        x1 = nodes_bs$ID[which(nodes_bs$Names==x2)]
        x4 = input$poet2
        x3 = nodes_bs$ID[which(nodes_bs$Names==x4)]
      }else if(input$poet1%in%nodes_bs$ID && input$poet2%in%nodes_bs$Names){
        x1 = input$poet1
        x2 = nodes_bs$Names[which(nodes_bs$ID==x1)]
        x4 = input$poet2
        x3 = nodes_bs$ID[which(nodes_bs$Names==x4)]
      }else if(input$poet1%in%nodes_bs$Names && input$poet2%in%nodes_bs$ID){
        x2 = input$poet1
        x1 = nodes_bs$ID[which(nodes_bs$Names==x2)]
        x3 = input$poet2
        x4 = nodes_bs$Names[which(nodes_bs$ID==x3)]
      }else{
        return()
      }
      
      tempIn = neighbors(gg1, x1,mode = "in") 
      if(x3 %in% tempIn$name){
        paste(x2," < -  ",x4,"：","被动关系，",x2,"被",x4,"赠诗词碑赋，文中被提及")
      }else{
        return()
      }
      
    }else{
      if(input$poet1%in%nodes_ns$ID && input$poet2%in%nodes_ns$ID){
        x1 = input$poet1
        x2 = nodes_ns$Names[which(nodes_ns$ID==x1)]
        x3 = input$poet2
        x4 = nodes_ns$Names[which(nodes_ns$ID==x3)]
      }else if(input$poet1%in%nodes_ns$Names && input$poet2%in%nodes_ns$Names){
        x2 = input$poet1
        x1 = nodes_ns$ID[which(nodes_ns$Names==x2)]
        x4 = input$poet2
        x3 = nodes_ns$ID[which(nodes_ns$Names==x4)]
      }else if(input$poet1%in%nodes_ns$ID && input$poet2%in%nodes_ns$Names){
        x1 = input$poet1
        x2 = nodes_ns$Names[which(nodes_ns$ID==x1)]
        x4 = input$poet2
        x3 = nodes_ns$ID[which(nodes_ns$Names==x4)]
      }else if(input$poet1%in%nodes_ns$Names && input$poet2%in%nodes_ns$ID){
        x2 = input$poet1
        x1 = nodes_ns$ID[which(nodes_ns$Names==x2)]
        x3 = input$poet2
        x4 = nodes_ns$Names[which(nodes_ns$ID==x3)]
      }else{
        return()
      }
      
      tempIn = neighbors(gg2, x1,mode = "in") 
      if(x3 %in% tempIn$name){
        paste(x2," < -  ",x4,"：","被动关系，",x2,"被",x4,"赠诗词碑赋，文中被提及")
      }else{
        return()
      }
      
    }

    
  })
  output$currentDyn <- renderPrint({
    if(input$dynasties=="北宋"){
      x1 = input$dynasties
      
    }else{
      x1 = input$dynasties
    }
    paste(x1)
  })
  
  #排行榜
  output$namedg1 <- renderText({
    if(input$dynasties=="北宋"){
      return("苏轼")
    }else{
      return("周必大")
    }
  })
  output$detaildg1_id <- renderText({
    if(input$dynasties=="北宋"){
      paste("ID：3767")
    }else{
      paste("ID：7197")
    }
  })
  output$detaildg1_life <- renderText({
    if(input$dynasties=="北宋"){
      paste("生卒年：1037.1.8—1101.8.24")
    }else{
      paste("生卒年：1126.8.15—1204.10.25")
    }
  })
  output$detaildg1_value <- renderText({
    if(input$dynasties=="北宋"){
      paste("人气度：385")
    }else{
      paste("人气度：590")
    }
  })
  output$detaildg1_info <- renderText({
    if(input$dynasties=="北宋"){
      paste("字子瞻，又字和仲，号铁冠道人、东坡居士，世称苏东坡、苏仙。汉族，眉州眉山（今属四川省眉山市）人，祖籍河北栾城，北宋文学家、书法家、画家。")
    }else{
      paste("字子充，一字洪道，自号平园老叟。原籍郑州管城（今河南郑州），至祖父周诜时居吉州庐陵（今江西省吉安县永和镇周家村）。南宋著名政治家、文学家，“庐陵四忠”之一。")
    }
  })
  
  output$namedg2 <- renderText({
    if(input$dynasties=="北宋"){
      return("黄庭坚")
    }else{
      return("朱熹")
    }
  })
  output$detaildg2_id <- renderText({
    if(input$dynasties=="北宋"){
      paste("ID：7111") 
    }else{
      paste("ID：3257")
    }
  })
  output$detaildg2_life <- renderText({
    if(input$dynasties=="北宋"){
      paste("生卒年：1045.8.9-1105.5.24") 
    }else{
      paste("生卒年：1130.9.15—1200.4.23")
    }
  })
  output$detaildg2_value <- renderText({
    if(input$dynasties=="北宋"){
      paste("人气度：351") 
    }else{
      paste("人气度：526")
    }
  })
  output$detaildg2_info <- renderText({
    if(input$dynasties=="北宋"){
      paste("字鲁直，号山谷道人，晚号涪翁，洪州分宁（今江西省九江市修水县）人，北宋著名文学家、书法家，为盛极一时的江西诗派开山之祖。") 
    }else{
      paste("字元晦，又字仲晦，号晦庵，晚称晦翁，谥文，世称朱文公。祖籍徽州府婺源县（今江西省婺源），出生于南剑州尤溪（今属福建省尤溪县）。宋朝著名的理学家、思想家、哲学家、教育家、诗人，闽学派的代表人物，儒学集大成者，世尊称为朱子。")
    }
  })
  
  output$namedg3 <- renderText({
    if(input$dynasties=="北宋"){
      return("欧阳修")
    }else{
      return("刘克庄")
    }
  })
  output$detaildg3_id <- renderText({
    if(input$dynasties=="北宋"){
      paste("ID：1384")
    }else{
      paste("ID：3595")
    }
  })
  output$detaildg3_life <- renderText({
    if(input$dynasties=="北宋"){
      paste("生卒年：1007.8.1－1072.9.22")
    }else{
      paste("生卒年：1187.9.3—1269.3.3")
    }
  })
  output$detaildg3_value <- renderText({
    if(input$dynasties=="北宋"){
      paste("人气度：314")
    }else{
      paste("人气度：468")
    }
  })
  output$detaildg3_info <- renderText({
    if(input$dynasties=="北宋"){
      paste("字永叔，号醉翁，晚号六一居士，汉族，吉州永丰（今江西省吉安市永丰县）人，北宋政治家、文学家，且在政治上负有盛名。“唐宋八大家”之一，并与韩愈、柳宗元、苏轼被后人合称“千古文章四大家”。")
    }else{
      paste("初名灼，字潜夫，号后村，福建省莆田市人。南宋豪放派诗人、词人、诗论家。诗属江湖诗派，内容开阔，多言谈时政，反映民生之作，早年学晚唐体，晚年诗风趋向江西诗派。词深受辛弃疾影响，多豪放之作，散文化、议论化倾向也较突出。")
    }
  })
 
  
  output$namebt1 <- renderText({
    if(input$dynasties=="北宋"){
      return("苏轼")
    }else{
      return("朱熹")
    }
  })
  output$detailbt1_id <- renderText({
    if(input$dynasties=="北宋"){
      paste("ID：3767")
    }else{
      paste("ID：3257")
    }
  })
  output$detailbt1_life <- renderText({
    if(input$dynasties=="北宋"){
      paste("生卒年：1037.1.8—1101.8.24")
    }else{
      paste("生卒年：1130.9.15—1200.4.23")
    }
  })
  output$detailbt1_value <- renderText({
    if(input$dynasties=="北宋"){
      paste("控制能力指数：6.19e+05")
    }else{
      paste("控制能力指数：1.30e+06")
    }
  })
  output$detailbt1_info <- renderText({
    if(input$dynasties=="北宋"){
      paste("又字和仲，号铁冠道人、东坡居士，世称苏东坡、苏仙。汉族，眉州眉山（今属四川省眉山市）人，祖籍河北栾城，北宋文学家、书法家、画家。")
    }else{
      paste("又字仲晦，号晦庵，晚称晦翁，谥文，世称朱文公。祖籍徽州府婺源县（今江西省婺源），出生于南剑州尤溪（今属福建省尤溪县）。宋朝著名的理学家、思想家、哲学家、教育家、诗人，闽学派的代表人物，儒学集大成者，世尊称为朱子。")
    }
  })
  
  output$namebt2 <- renderText({
    if(input$dynasties=="北宋"){
      return("黄庭坚")
    }else{
      return("周必大")
    }
  })
  output$detailbt2_id <- renderText({
    if(input$dynasties=="北宋"){
      paste("ID：7111") 
    }else{
      paste("ID：7197")
    }
  })
  output$detailbt2_life <- renderText({
    if(input$dynasties=="北宋"){
      paste("生卒年：1045.8.9-1105.5.24") 
    }else{
      paste("生卒年：1126.8.15—1204.10.25")
    }
  })
  output$detailbt2_value <- renderText({
    if(input$dynasties=="北宋"){
      paste("控制能力指数：4.44e+05") 
    }else{
      paste("控制能力指数：9.12e+05")
    }
  })
  output$detailbt2_info <- renderText({
    if(input$dynasties=="北宋"){
      paste("号山谷道人，晚号涪翁，洪州分宁（今江西省九江市修水县）人，北宋著名文学家、书法家，为盛极一时的江西诗派开山之祖。") 
    }else{
      paste("一字洪道，自号平园老叟。原籍郑州管城（今河南郑州），至祖父周诜时居吉州庐陵（今江西省吉安县永和镇周家村）。南宋著名政治家、文学家，“庐陵四忠”之一。")
    }
  })

  output$namebt3 <- renderText({
    if(input$dynasties=="北宋"){
      return("欧阳修")
    }else{
      return("刘克庄")
    }
  })
  output$detailbt3_id <- renderText({
    if(input$dynasties=="北宋"){
      paste("ID：1384")
    }else{
      paste("ID：3595")
    }
  })
  output$detailbt3_life <- renderText({
    if(input$dynasties=="北宋"){
      paste("生卒年：1007.8.1－1072.9.22")
    }else{
      paste("生卒年：1187.9.3—1269.3.3")
    }
  })
  output$detailbt3_value <- renderText({
    if(input$dynasties=="北宋"){
      paste("控制能力指数：3.47e+05")
    }else{
      paste("控制能力指数：6.65e+05")
    }
  })
  output$detailbt3_info <- renderText({
    if(input$dynasties=="北宋"){
      paste("字永叔，号醉翁，晚号六一居士，汉族，吉州永丰（今江西省吉安市永丰县）人，北宋政治家、文学家，且在政治上负有盛名。“唐宋八大家”之一，并与韩愈、柳宗元、苏轼被后人合称“千古文章四大家”。")
    }else{
      paste("初名灼，字潜夫，号后村，福建省莆田市人。南宋豪放派诗人、词人、诗论家。诗属江湖诗派，内容开阔，多言谈时政，反映民生之作，早年学晚唐体，晚年诗风趋向江西诗派。词深受辛弃疾影响，多豪放之作，散文化、议论化倾向也较突出。")
    }
  })
  
  
  output$nameegin1 <- renderText({
    if(input$dynasties=="北宋"){
      return("苏轼")
    }else{
      return("朱熹")
    }
  })
  output$detailegin1_id <- renderText({
    if(input$dynasties=="北宋"){
      paste("ID：3767")
    }else{
      paste("ID：3257")
    }
  })
  output$detailegin1_life <- renderText({
    if(input$dynasties=="北宋"){
      paste("生卒年：1037.1.8—1101.8.24")
    }else{
      paste("生卒年：1130.9.15—1200.4.23")
    }
  })
  output$detailegin1_value <- renderText({
    if(input$dynasties=="北宋"){
      paste("潜在价值：1.00")
    }else{
      paste("潜在价值：1.00")
    }
  })
  output$detailegin1_info <- renderText({
    if(input$dynasties=="北宋"){
      paste("字子瞻，又字和仲，号铁冠道人、东坡居士，世称苏东坡、苏仙。汉族，眉州眉山（今属四川省眉山市）人，祖籍河北栾城，北宋文学家、书法家、画家。")
    }else{
      paste("字元晦，又字仲晦，号晦庵，晚称晦翁，谥文，世称朱文公。祖籍徽州府婺源县（今江西省婺源），出生于南剑州尤溪（今属福建省尤溪县）。宋朝著名的理学家、思想家、哲学家、教育家、诗人，闽学派的代表人物，儒学集大成者，世尊称为朱子。")
    }
  })
  
  output$nameegin2 <- renderText({
    if(input$dynasties=="北宋"){
      return("黄庭坚")
    }else{
      return("周必大")
    }
  })
  output$detailegin2_id <- renderText({
    if(input$dynasties=="北宋"){
      paste("ID：7111") 
    }else{
      paste("ID：7197")
    }
  })
  output$detailegin2_life <- renderText({
    if(input$dynasties=="北宋"){
      paste("生卒年：1045.8.9-1105.5.24") 
    }else{
      paste("生卒年：1126.8.15—1204.10.25")
    }
  })
  output$detailegin2_value <- renderText({
    if(input$dynasties=="北宋"){
      paste("潜在价值：0.93") 
    }else{
      paste("潜在价值：0.88")
    }
  })
  output$detailegin2_info <- renderText({
    if(input$dynasties=="北宋"){
      paste("字鲁直，号山谷道人，晚号涪翁，洪州分宁（今江西省九江市修水县）人，北宋著名文学家、书法家，为盛极一时的江西诗派开山之祖。") 
    }else{
      paste("字子充，一字洪道，自号平园老叟。原籍郑州管城（今河南郑州），至祖父周诜时居吉州庐陵（今江西省吉安县永和镇周家村）。南宋著名政治家、文学家，“庐陵四忠”之一。")
    }
  })
  
  output$nameegin3 <- renderText({
    if(input$dynasties=="北宋"){
      return("欧阳修")
    }else{
      return("楼钥")
    }
  })
  output$detailegin3_id <- renderText({
    if(input$dynasties=="北宋"){
      paste("ID：1384")
    }else{
      paste("ID：3624")
    }
  })
  output$detailegin3_life <- renderText({
    if(input$dynasties=="北宋"){
      paste("生卒年：1007.8.1－1072.9.22")
    }else{
      paste("生卒年：1137～1213")
    }
  })
  output$detailegin3_value <- renderText({
    if(input$dynasties=="北宋"){
      paste("潜在价值：0.85")
    }else{
      paste("潜在价值：0.56")
    }
  })
  output$detailegin3_info <- renderText({
    if(input$dynasties=="北宋"){
      paste("字永叔，号醉翁，晚号六一居士，汉族，吉州永丰（今江西省吉安市永丰县）人，北宋政治家、文学家，且在政治上负有盛名。“唐宋八大家”之一，并与韩愈、柳宗元、苏轼被后人合称“千古文章四大家”。")
    }else{
      paste("南宋大臣、文学家。字大防，又字启伯，号攻媿主人，明州鄞县（今属浙江宁波）人。")
    }
  })
            
  
  output$imgdg1 <- renderImage({ 
    if(input$dynasties=="北宋"){
      return(
        list(src="www/ss.jpg",height = "100%",width="100%")
      )
    }
    else{
      return(
        list(src="www/zbd.jpg",height = "100%",width="100%")
      )
    }
  }, deleteFile = FALSE)
  output$imgdg2 <- renderImage({ 
    if(input$dynasties=="北宋"){
      return(
        list(src="www/htj.jpg",height = "100%",width="100%")
      )
    }
    else{
      return(
        list(src="www/zx.png",height = "100%",width="100%")
      )
    }
  }, deleteFile = FALSE)
  output$imgdg3 <- renderImage({ 
    if(input$dynasties=="北宋"){
      return(
        list(src="www/oyx.jpg",height = "100%",width="100%")
      )
    }
    else{
      return(
        list(src="www/lkz.jpg",height = "100%",width="100%")
      )
    }
  }, deleteFile = FALSE)
  
  output$imgbt1 <- renderImage({ 
    if(input$dynasties=="北宋"){
      return(
        list(src="www/ss.jpg",height = "100%",width="100%")
      )
    }
    else{
      return(
        list(src="www/zx.png",height = "100%",width="100%")
      )
    }
  }, deleteFile = FALSE)
  output$imgbt2 <- renderImage({ 
    if(input$dynasties=="北宋"){
      return(
        list(src="www/htj.jpg",height = "100%",width="100%")
      )
    }
    else{
      return(
        list(src="www/zbd.jpg",height = "100%",width="100%")
      )
    }
  }, deleteFile = FALSE)
  output$imgbt3 <- renderImage({ 
    if(input$dynasties=="北宋"){
      return(
        list(src="www/oyx.jpg",height = "100%",width="100%")
      )
    }
    else{
      return(
        list(src="www/lkz.jpg",height = "100%",width="100%")
      )
    }
  }, deleteFile = FALSE)
  
  output$imgegin1 <- renderImage({ 
    if(input$dynasties=="北宋"){
      return(
        list(src="www/ss.jpg",height = "100%",width="100%")
      )
    }
    else{
      return(
        list(src="www/zx.png",height = "100%",width="100%")
      )
    }
  }, deleteFile = FALSE)
  output$imgegin2 <- renderImage({ 
    if(input$dynasties=="北宋"){
      return(
        list(src="www/htj.jpg",height = "100%",width="100%")
      )
    }
    else{
      return(
        list(src="www/zbd.jpg",height = "100%",width="100%")
      )
    }
  }, deleteFile = FALSE)
  output$imgegin3 <- renderImage({ 
    if(input$dynasties=="北宋"){
      return(
        list(src="www/oyx.jpg",height = "100%",width="100%")
      )
    }
    else{
      return(
        list(src="www/ly.jpg",height = "100%",width="100%")
      )
    }
  }, deleteFile = FALSE)
  
  #帮派
  output$community <- renderDataTable({
    if(input$dynasties=="北宋"){
      if(input$top=="1"){
        datatable(topbs1)
      }else if(input$top=="2"){
        datatable(topbs2)
      }else{
        datatable(topbs3)
      }
    }else{
      if(input$top=="1"){
        datatable(topns1)
      }else if(input$top=="2"){
        datatable(topns2)
      }else{
        datatable(topns3)
      }
    }
  })
  output$info <- renderText({
    if(input$dynasties=="北宋"){
      if(input$top=="1"){
        return("北宋时期，帮派成员人数有748人。其中，晏殊（2073），欧阳修（1384），苏舜钦（7064），尹洙（7104），张方平（217），宋祁（1558）等是该诗人群体的核心人物，影响力大。成员的诗词风格大多受其影响，婉转含蓄，结构深细缜密，音律婉转和谐，语言圆润清丽，具有婉约派的风格特点。")
      }else if(input$top=="2"){
        return("北宋时期，帮派成员人数有413人。其中，苏轼（3767），贺铸（18063），李纲（8078），杨时（2022），李之仪（3484），毛滂（3672）等是该诗人群体的核心人物，影响力大。成员的诗词风格大多受其影响，创作视野较为广阔，气象恢弘雄放，喜用诗文的手法、句法写词，语词宏博，用事较多，不拘守音律，然而有时失之平直，甚至涉于狂怪叫嚣，具有豪放派的风格特点。")
      }else{
        return("北宋时期，帮派成员人数有331人。其中，王安石（1762），曾巩（7364），刘攽（1220）等是该诗人群体的核心人物，影响力大。成员的诗词风格大多受其影响，说理与修辞，含蓄深沉、深婉不迫，写物咏怀吊古，意境空阔苍茫，形象淡远纯朴，自然淳朴。")
      }
    }else{
      if(input$top=="1"){
        return("南宋时期，帮派成员人数有721人。其中，周必大（7197），陆游（3640），张元干（19699），张孝祥（3144），陈与义（8004）等是该诗人群体的核心人物，影响力大。成员的诗词风格大多受其影响，词风慷慨悲凉，相激相慰，悲壮慷慨的高亢之调，以爱国恢复的壮词宏声组成雄阔的阵容，具有豪放派的风格特点。")
      }else if(input$top=="2"){
        return("南宋时期，帮派成员人数有673人。其中，朱熹（3257），真德秀（10258），黄干（11134），陈宓（10892），张栻（7164）等是该诗人群体的核心人物，影响力大。成员的诗词风格大多受其影响，体现理学思想，自然和谐，毫不牵强，颇有意境")
      }else{
        return("南宋时期，帮派成员人数有606人。其中，刘克庄（3595），方大琮（11328），林光朝（10612），林希逸（11087），姚勉（27644）等是该诗人群体的核心人物，影响力大。成员的诗词风格大多受其影响，词风慷慨悲凉，相激相慰，反映民生，悲壮慷慨，饱含爱国情怀，具有豪放派的风格特点。")
      }
    }
  })
  output$bpSNA <- renderPlot({
    if(input$dynasties=="北宋"){
      if(input$top=="1"){
        sg1 = subgraph(gg12,fc1[[2]])
        plot(sg1,width="100%",height="100%",layout=layout.auto,
             vertex.size = log(degree(sg1), 2)*1+3,vertex.color="#80B695",
             vertex.label.dist=0.1,vertex.label.cex=1.3,vertex.label.color="black",
             edge.color = "#80B695",edge.arrow.size = 0.4)
      }else if(input$top=="2"){
        sg1 = subgraph(gg12,fc1[[9]])
        plot(sg1,width="100%",height="100%",layout=layout.auto,
             vertex.size = log(degree(sg1), 2)*1+3,vertex.color="#80B695",
             vertex.label.dist=0.1,vertex.label.cex=1.3,vertex.label.color="black",
             edge.color = "#80B695",edge.arrow.size = 0.4)
      }else{
        sg1 = subgraph(gg12,fc1[[1]])
        plot(sg1,width="100%",height="100%",layout=layout.auto,
             vertex.size = log(degree(sg1), 2)*1+3,vertex.color="#80B695",
             vertex.label.dist=0.1,vertex.label.cex=1.3,vertex.label.color="black",
             edge.color = "#80B695",edge.arrow.size = 0.4)
      }
    }else{
      if(input$top=="1"){
        sg1 = subgraph(gg22,fc2[[3]])
        plot(sg1,width="100%",height="100%",layout=layout.auto,
             vertex.size = log(degree(sg1), 2)*1+3,vertex.color="#80B695",
             vertex.label.dist=0.1,vertex.label.cex=1.3,vertex.label.color="black",
             edge.color = "#80B695",edge.arrow.size = 0.4)
      }else if(input$top=="2"){
        sg1 = subgraph(gg22,fc2[[7]])
        plot(sg1,width="100%",height="100%",layout=layout.auto,
             vertex.size = log(degree(sg1), 2)*1+3,vertex.color="#80B695",
             vertex.label.dist=0.1,vertex.label.cex=1.3,vertex.label.color="black",
             edge.color = "#80B695",edge.arrow.size = 0.4)
      }else{
        sg1 = subgraph(gg22,fc2[[1]])
        plot(sg1,width="100%",height="100%",layout=layout.auto,
             vertex.size = log(degree(sg1), 2)*1+3,vertex.color="#80B695",
             vertex.label.dist=0.1,vertex.label.cex=1.3,vertex.label.color="black",
             edge.color = "#80B695",edge.arrow.size = 0.4)
      }
    }
  })

  #ID page
  output$dataID <- renderDataTable({
    if(input$dynasties=="北宋"){
      datatable(bdatasr)
    }else{
      datatable(ndatasr)
    }
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)





