library(gbm)
library(glmnet)
library(caret)
library(ggplot2)
library(pROC)
library(ggforce)
library(shiny)
library(gridExtra)
ui <- fluidPage(
  titlePanel("your web name"),   
  sidebarLayout(             
    sidebarPanel(
     
      fluidRow(
        column(8,
               numericInput("your varianle1",h5("your varianle1(variable1 Unit)"),
                            min = ,max = ,value = ))),
      
      fluidRow(
        column(8,
               numericInput("your varianle2",h5("your varianle2(variable2 Unit)"),
                            min = ,max = ,value = ))),
      
      actionButton("goButton", "Predict")
    ),
    
    mainPanel(
      print(""),
      plotOutput("piediagram")
    )
    
  ))
