library(shiny)
library(DT)

source("bss.R")
source("nss.R")

# Define UI ----
ui <- fluidPage(
  
  sidebarLayout(
    
    #侧边栏
    sidebarPanel(
      titlePanel("宋代诗人社会网络"),
      helpText("利用中国历代人物传记数据库，以诗人与诗人互相赠送诗文建立关系，构建北宋和南宋诗人互赠诗文关系网络。从网络的派系、度数中心度、中介中心度、接近中心度、特征向量中心度等出发，研究分析北宋和南宋诗人互赠诗文关系网络的结构特征，结合诗人及朝代背景，从而分析宋代诗词在诗人之间的传播特征。"),
      br(),
      selectInput("dynasties", label = "朝代：",choices = c("北宋","南宋"),selected = "北宋"),
      br(),
      
      h4(strong("热点聚焦：")),
      wellPanel(

        fluidRow(
          column(6,
                 radioButtons("radio1", h4("豪放派："),
                              choices = list("辛弃疾" = 1, "苏轼" = 2,"张元干" = 3),selected = 1)),
          column(6,
                 radioButtons("radio2", h4("婉约派："),
                              choices = list("李煜" = 1, "秦观" = 2,"李清照" = 3),selected = 1)),
          column(3,
                 actionButton("view", "确认",width = 100))
          
        )
        
      ),

      br(),
      br(),
      br(),
      br(),
      
      
      hr(),
      textInput("poetNum", label = "帮助：", value = "请输入诗人ID或姓名"),
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
          # h5("注解：数字代表诗人编号"),
          wellPanel(
            fluidRow(
              column(12,
                     imageOutput("images",height = 850),
                     br()),
              
              column(5,offset = 7,
                     textOutput("note"))
            )
          )
          # wellPanel(
          #   fluidRow(
          #     column(6,
          #            wellPanel(plotOutput("bold"))),
          #     column(6,
          #            wellPanel(plotOutput("graceful")))
          # 
          #     
          #   )
          # )
        ),
        
        tabPanel("诗人查询",
          fluidRow(
            column(6,
                   textInput("search", label = "诗人：", value = "请输入诗人ID或姓名")),
            column(6,
                   helpText(strong("热门搜索："),"辛弃疾，苏轼，王安石，李清照，秦观，陆游，李煜，黄庭坚，欧阳修，范仲淹，朱熹，周必大"))
          ),
          hr(),
          h4("基本信息："),
          verbatimTextOutput("resultID"),
          # textOutput("resultName"),
          # textOutput("resultDyn"),
          hr(),
          h4("诗人网络图："),
          wellPanel(plotOutput("SNA"))
        ),
        
        tabPanel("关系查询",
          h5("注解：输入两位诗人，查询在当前朝代下他们之间是否存在联系，是主动关系还是被动关系"),
          br(),
          textInput("poet1", label = "诗人1：", value = "请输入诗人1的ID或姓名"),
          textInput("poet2", label = "诗人2：", value = "请输入诗人2的ID或姓名"),    
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
            radioButtons("index", width="100%", h3("排名指标："),choices = list("人气度（该诗人与多少诗人相互关联，数值由点度中心度计算）" = 1, "控制能力指数（其他诗人相互联系要经过的最少人数中是否都包含该诗人，强调该诗人在其他诗人关联之间的调节控制能力，数值由中介中心度计算）" = 2,"潜在价值（根据相邻诗人的重要性来衡量该诗人的价值，数值由特征向量中心度计算）" = 3),selected = 1),
            dataTableOutput("rank")
        ),
        
        tabPanel("帮派",
          h5("注解：展示当前朝代下紧密联系最大的三个帮派群体"),
          selectInput("top", label = "Top：",choices = c("1","2","3"),selected = "1"),
          dataTableOutput("community")
        ),
        
        tabPanel("ID",
          dataTableOutput("dataID")
        )
        
      )
    )
  )
)

# Define server logic ----
server <- function(input, output,session) {
  
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
  # dataInput <- eventReactive(input$view, {
  #   bold = input$radio1
  #   graceful = input$radio2
  # }, ignoreNULL = FALSE)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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

    
    
    
    
    # if(input$dynasties=="北宋"){
    #   return(
    #     list(src="www/bss.png",height = "100%",width="100%",title="北宋，涉及3972位诗人，6679对关系。图密度为0.0004234502，非常稀疏。非连通图，诗人相互联系时最多经过9人，平均经过4人。该有向图存在585对诗人互赠诗文。")
    #   )
    # }
    # else{
    #   return(
    #     list(src="www/nss.png",height = "100%",width="100%",title="南宋，涉及5254位诗人，9086对关系。图密度为0.0003292117，非常稀疏。非连通图，诗人相互联系时最多经过9人，平均经过4人。该有向图存在1065对诗人互赠诗文。")
    #   )
    # }
    
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
        list(x1,x2,x3)
      }else if(input$search%in%nodes_bs$Names){
        x2 = input$search
        x1 = nodes_bs$ID[which(nodes_bs$Names==x2)]
        x3 = input$dynasties
        list(x1,x2,x3)
      }else if(input$search=="请输入诗人ID或姓名"){
        # return()
        list("ID","姓名","朝代")
      }else{
        return("输入有误或者朝代不符，请修改！")
      }
    }else{
      if(input$search%in%nodes_ns$ID){
        x1 = input$search
        x2 = nodes_ns$Names[which(nodes_ns$ID==x1)]
        x3 = input$dynasties
        list(x1,x2,x3)
      }else if(input$search%in%nodes_ns$Names){
        x2 = input$search
        x1 = nodes_ns$ID[which(nodes_ns$Names==x2)]
        x3 = input$dynasties
        list(x1,x2,x3)
      }else if(input$search=="请输入诗人ID或姓名"){
        # return()
        list("ID","姓名","朝代")
      }else{
        return("输入有误或者朝代不符，请修改！")
      }
    }
    # paste("ID：", x1)

  })
  # output$resultName <- renderText({
  #   if(input$dynasties=="北宋"){
  #     if(input$search%in%nodes_bs$ID){
  #       x1 = input$search
  #       x2 = nodes_bs$Names[which(nodes_bs$ID==x1)]
  # 
  #     }else if(input$search%in%nodes_bs$Names){
  #       x2 = input$search
  #       x1 = nodes_bs$ID[which(nodes_bs$Names==x2)]
  # 
  #     }else if(input$search=="请输入诗人ID或姓名"){
  #       return()
  #     }else{
  #       return("输入有误或者朝代不符，请修改！")
  #     }
  #   }else{
  #     if(input$search%in%nodes_ns$ID){
  #       x1 = input$search
  #       x2 = nodes_ns$Names[which(nodes_ns$ID==x1)]
  # 
  #     }else if(input$search%in%nodes_ns$Names){
  #       x2 = input$search
  #       x1 = nodes_ns$ID[which(nodes_ns$Names==x2)]
  # 
  #     }else if(input$search=="请输入诗人ID或姓名"){
  #       return()
  #     }else{
  #       return("输入有误或者朝代不符，请修改！")
  #     }
  #   }
  #   paste("姓名：", x2)
  # })
  # output$resultDyn <- renderText({
  #   if(input$dynasties=="北宋"){
  #       x3 = input$dynasties
  # 
  #   }else{
  #       x3 = input$dynasties
  #   }
  #   paste("当前朝代：", x3)
  # })
  output$SNA <- renderPlot({
    if(input$dynasties=="北宋"){
      if(input$search%in%nodes_bs$ID || input$search%in%nodes_ns$ID){
        x1 = input$search
        x2 = nodes_bs$Names[which(nodes_bs$ID==x1)]
        x3 = input$dynasties
        # x4 = degree(gg1, v=x1 )
        # x5 = betweenness(gg1, v=x1)
        
      }else if(input$search%in%nodes_bs$Names || input$search%in%nodes_ns$Names){
        x2 = input$search
        x1 = nodes_bs$ID[which(nodes_bs$Names==x2)]
        x3 = input$dynasties
        # x4 = degree(gg1,v = x1 )
        
      }else{
        return()
      }
      gn<-graph.neighborhood(gg1, order=1)
      gn1 = gn[[which(V(gg1)$name==x1)]]
      
      plot(gn1,width="100%",height="100%",layout=layout.auto,
          vertex.size = ifelse(V(gn1)$name==x1,15,8),
          vertex.label.dist=0.1,vertex.label=V(gn1)$Names,
          vertex.color=ifelse(V(gn1)$name==x1,"red","green"),
          edge.color = grey(0.5),edge.arrow.size = 0.4)
      
      
    }else{
      if(input$search%in%nodes_bs$ID || input$search%in%nodes_ns$ID){
        x1 = input$search
        x2 = nodes_ns$Names[which(nodes_ns$ID==x1)]
        x3 = input$dynasties
        # x4 = degree(gg2,v = x1 )
        
      }else if(input$search%in%nodes_bs$Names || input$search%in%nodes_ns$Names){
        x2 = input$search
        x1 = nodes_ns$ID[which(nodes_ns$Names==x2)]
        x3 = input$dynasties
        # x4 = degree(gg2,v = x1 )
        
      }else{
        return()
      }
      gn<-graph.neighborhood(gg2, order=1)
      gn1 = gn[[which(V(gg2)$name==x1)]]
      plot(gn1,width="100%",height="100%",layout=layout.auto,
           vertex.size = ifelse(V(gn1)$name==x1,15,8),
           vertex.label.dist=0.1,vertex.label=V(gn1)$Names,
           vertex.color=ifelse(V(gn1)$name==x1,"red","green"),
           edge.color = grey(0.5),edge.arrow.size = 0.4)
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
      
      tempOut = neighbors(gg1, x1,mode = "out") #
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
      
      tempOut = neighbors(gg2, x1,mode = "out") #
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
      
      tempIn = neighbors(gg1, x1,mode = "in") #
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
      
      tempIn = neighbors(gg2, x1,mode = "in") #
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
  output$rank <- renderDataTable({
    if(input$dynasties=="北宋"){
      if(input$index==1){
        datatable(dg1)
      }else if(input$index==2){
        datatable(bt1)
      }else{
        datatable(eigen1)
      }
    }else{
      if(input$index==1){
        datatable(dg2)
      }else if(input$index==2){
        datatable(bt2)
      }else{
        datatable(eigen2)
      }
    }
  })
  
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
  
  
  #ID page
  output$dataID <- renderDataTable({
    if(input$dynasties=="北宋"){
      datatable(nodes_bs)
    }else{
      datatable(nodes_ns)
    }
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)





