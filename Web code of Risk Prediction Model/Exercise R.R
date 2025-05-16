library(gbm)
library(glmnet)
library(caret)
library(ggplot2)
library(pROC)
library(ggforce)
library(shiny)
library(gridExtra)
ui <- fluidPage(
  titlePanel("Machine-Learning-Driven Predictive Modeling for Bone Metastasis in Neuroblastoma"),     
  sidebarLayout(      
    sidebarPanel(
      
     
      fluidRow(
        column(8,
               numericInput("BMM.",h5("BMM.(%)"),
                            min = 0,max = 20,value = 10))),
      
      fluidRow(
        column(8,
               numericInput("WBC",h5("WBC(*10^9/L)"),
                            min = 1,max = 25,value = 15))),
      
      fluidRow(
        column(8,
               numericInput("PBE.",h5("PBE.(%)"),
                            min = 0,max = 25,value = 15))),
     
      fluidRow(
        column(8,
               numericInput("PBM",h5("PBM(*10^9/L)"),
                            min = 0,max = 5,value = 3))),
      
      fluidRow(
        column(8,
               numericInput("HGB",h5("HGB(g/L)"),
                            min = 0,max = 200,value = 100))),
      
      fluidRow(
        column(8,
               numericInput("NSE",h5("NSE(ng/mL)"),
                            min = 9.9,max = 2120,value = 300))),
     
      actionButton("goButton", "Predict")
    ),
    
    mainPanel(
      print(""),
      plotOutput("piediagram")
    )
    
  ))


server <- function(input,output){observeEvent(input$goButton, {
  output$piediagram<-renderPlot({
    
    var=c("Result","BMM.","WBC","PBE.","PBM","HGB","NSE")
    
    dev=read.csv("dev.csv",header = T,encoding = "GBK")
    dev$Result = factor(dev$Result,levels = c(0,1),labels = c('No','Yes'))
    dev = dev[,var]
    
    set.seed(100)
    train.control <- trainControl(method = 'repeatedcv',
                                  number = 5, 
                                  repeats = 3, 
                                  classProbs = TRUE, 
                                  summaryFunction = twoClassSummary)
    
    
    gbm.tune.grid = expand.grid(
      n.trees = 100,
      interaction.depth = 5,
      shrinkage = 0.1,
      n.minobsinnode = 10
    )
    
    
    
    set.seed(100)
    model = train(Result~.,
                  data = dev,
                  tuneGrid = gbm.tune.grid,  
                  metric='ROC',             
                  method= "gbm",           
                  trControl=train.control)
    
    save(model,file = "model.RData")
    load("model.RData")
    
    vaddata=data.frame(BMM.=input$BMM.,WBC=input$WBC,
                       PBE.=input$PBE.,PBM= input$PBM,HGB = input$HGB,NSE = input$NSE)
    
    vaddata <- as.data.frame(lapply(vaddata, function(x) {
      if (is.character(x)) {
        return(as.numeric(x))
      }
      return(x)
    }))
    
    test_pro = predict(model, newdata = vaddata, type = 'prob')[2]
    test_pro = as.numeric(test_pro[,1])
    
    
    ratio = c(test_pro*100,(1-test_pro)*100)
    disease = c("Risk", "non-Risk")
    
    A = data.frame(ratio, disease)
    A$ratio=round(A$ratio)
    ggplot(A)+
      geom_arc_bar(data=A,
                   stat = "pie",
                   aes(x0=0,y0=0,r0=1,r=2,
                       amount=ratio,fill=disease
                   ))+
      theme_bw()+
      theme(
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 25),
        legend.spacing.x = unit(1,"cm"),
        legend.key.width = unit(1, "cm"),
        plot.title = element_text(hjust=0.5,face = "bold",size = 8),
        legend.title = element_blank()
      )+
      #labs(title = "Male individuals")+
      scale_x_continuous(breaks = NULL)+
      scale_y_continuous(breaks = NULL)+
      scale_fill_manual(values=c("#FFC0CB", "#7ac7e0"),
                        breaks=c("Risk","non-Risk"))+
      geom_text(x=0.1, y=0.3, label ="Risk", fontface=2,size=9)+
      geom_text(x=0.1, y=-0.3, label =scales::percent(test_pro, 0.01),
                fontface=2,size=9)
    
  })
})
  
}


shinyApp(ui = ui,server = server)