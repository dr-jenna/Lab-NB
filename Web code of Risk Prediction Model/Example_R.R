library(gbm)
library(glmnet)
library(caret)
library(ggplot2)
library(pROC)
library(ggforce)
library(shiny)
library(gridExtra)
setwd()
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
vaddata=data.frame(BMM.=0.5,WBC=9.03,PBE.=0.2,PBM=0.51,HGB=92,NSE=343.9) 
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
  scale_fill_manual(values=c("Risk" = "#FFC0CB", "non-Risk" = "#7ac7e0"),
                    breaks=c("Risk","non-Risk"))+
  geom_text(x=0.1, y=0.3, label ="Risk", fontface=2,size=9)+
  geom_text(x=0.1, y=-0.3, label =scales::percent(test_pro, 0.01),
            fontface=2,size=9)
