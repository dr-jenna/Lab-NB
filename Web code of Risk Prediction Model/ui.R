library(gbm)
library(glmnet)
library(caret)
library(ggplot2)
library(pROC)
library(ggforce)
library(shiny)
library(gridExtra)
ui <- fluidPage(
  titlePanel("Machine-Learning-Driven Predictive Modeling for Bone Metastasis in Neuroblastoma"),     #Big title
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
