setwd()
library(survival)
library(survminer)
library(foreign)
library(glmnet)
library(timeROC)
library(pROC)
library(plotROC)
library(ggplot2)
trainplot_data = read.csv()
trainplot_data <- na.omit(trainplot_data)
trainplot_data$Bone <- NULL
trainplot_data$Result <- NULL
dead=subset(trainplot_data,trainplot_data$Event==1)
survival=subset(trainplot_data,trainplot_data$Event==0)
time=12  
dead$Result=ifelse(dead$Time>time,0,1)
survival$Result=0
ROC_data_1=rbind(dead,survival)

time=24   
dead$Result=ifelse(dead$Time>time,0,1)
survival$Result=0
ROC_data_2=rbind(dead,survival)

time=36   
dead$Result=ifelse(dead$Time>time,0,1)
survival$Result=0
ROC_data_3=rbind(dead,survival)

ROC1<-roc(response=ROC_data_1$Result,predictor=ROC_data_1$GBM)
ROC2<-roc(response=ROC_data_2$Result,predictor=ROC_data_2$GBM)
ROC3<-roc(response=ROC_data_3$Result,predictor=ROC_data_3$GBM)

allroc=list(`1year` = ROC1,
            `2year`=ROC2,
            `3year`=ROC3)

AUC1=round(pROC::auc(ROC1),3)
AUC2=round(pROC::auc(ROC2),3)
AUC3=round(pROC::auc(ROC3),3)

CI1=ci.auc(ROC1)
CI2=ci.auc(ROC2)
CI3=ci.auc(ROC3)

plotroc = pROC::ggroc(allroc,
                      size=1,
                      legacy.axes = T
)+theme_bw()+
  labs(title = 'ROC curve')+
  theme(
    plot.title = element_text(hjust = 0.5,size = 15,face="bold"),
    axis.text=element_text(size=12,face="bold"),
    legend.title = element_blank(),
    legend.text = element_text(size=12,face="bold"),
    legend.position=c(0.6,0.15),
    legend.background = element_blank(),
    axis.title.y = element_text(size=12,face="bold"),
    axis.title.x = element_text(size=12,face="bold"),
    panel.border = element_rect(color="black",size=1),
    panel.background = element_blank())+
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),        
               colour='grey', 
               linetype = 'dotdash'
  )+
  scale_colour_discrete(
    breaks=c("1year","2year","3year"),#
    labels=c(paste0("1-year(AUC=",sprintf("%0.3f", AUC1),",95%CI(",
                    sprintf("%0.3f", CI1[1]),"-",sprintf("%0.3f", CI2[3]),")"),
             paste0("2-year(AUC=",sprintf("%0.3f", AUC2),",95%CI(",
                    sprintf("%0.3f", CI2[1]),"-",sprintf("%0.3f", CI2[3]),")"),
             paste0("3-year(AUC=",sprintf("%0.3f", AUC3),",95%CI(",
                    sprintf("%0.3f", CI3[1]),"-",sprintf("%0.3f", CI3[3]),")")
    ))
pdf(paste0("TrainROC_survival.pdf"),5,5,family = "serif")
print(plotroc)
dev.off()

testplot_data = read.csv(,header = T)
testplot_data <- na.omit(testplot_data)
testplot_data$Bone <- NULL
testplot_data$Result <- NULL

dead=subset(testplot_data,testplot_data$Event==1)
survival=subset(testplot_data,testplot_data$Event==0)
time=12  
dead$Result=ifelse(dead$Time>time,0,1)
survival$Result=0
ROC_data_1=rbind(dead,survival)

time=24   
dead$Result=ifelse(dead$Time>time,0,1)
survival$Result=0
ROC_data_2=rbind(dead,survival)

time=36  
dead$Result=ifelse(dead$Time>time,0,1)
survival$Result=0
ROC_data_3=rbind(dead,survival)

ROC1<-roc(response=ROC_data_1$Result,predictor=ROC_data_1$GBM)
ROC2<-roc(response=ROC_data_2$Result,predictor=ROC_data_2$GBM)
ROC3<-roc(response=ROC_data_3$Result,predictor=ROC_data_3$GBM)

allroc=list(`1year` = ROC1,
            `2year`=ROC2,
            `3year`=ROC3)

AUC1=round(pROC::auc(ROC1),3)
AUC2=round(pROC::auc(ROC2),3)
AUC3=round(pROC::auc(ROC3),3)

CI1=ci.auc(ROC1)
CI2=ci.auc(ROC2)
CI3=ci.auc(ROC3)

plotroc = pROC::ggroc(allroc,
                      size=1,
                      legacy.axes = T
)+theme_bw()+
  labs(title = 'ROC curve')+
  theme(
    plot.title = element_text(hjust = 0.5,size = 15,face="bold"),
    axis.text=element_text(size=12,face="bold"),
    legend.title = element_blank(),
    legend.text = element_text(size=12,face="bold"),
    legend.position=c(0.6,0.15),
    legend.background = element_blank(),
    axis.title.y = element_text(size=12,face="bold"),#element_blank()
    axis.title.x = element_text(size=12,face="bold"),
    panel.border = element_rect(color="black",size=1),
    panel.background = element_blank())+
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),       
               colour='grey', 
               linetype = 'dotdash'
  )+
  scale_colour_discrete(
    breaks=c("1year","2year","3year"),#
    labels=c(paste0("1-year(AUC=",sprintf("%0.3f", AUC1),",95%CI(",
                    sprintf("%0.3f", CI1[1]),"-",sprintf("%0.3f", CI2[3]),")"),
             paste0("2-year(AUC=",sprintf("%0.3f", AUC2),",95%CI(",
                    sprintf("%0.3f", CI2[1]),"-",sprintf("%0.3f", CI2[3]),")"),
             paste0("3-year(AUC=",sprintf("%0.3f", AUC3),",95%CI(",
                    sprintf("%0.3f", CI3[1]),"-",sprintf("%0.3f", CI3[3]),")")
    ))
pdf(paste0("TestROC_survival.pdf"),5,5,family = "serif")
print(plotroc)
dev.off()

km_data = trainplot_data
Med =  median(km_data$GBM, na.rm = TRUE)
km_data$RiskGroup = ifelse(km_data$GBM > Med, "high", "low")
newdata = km_data[,c(1,2,ncol(km_data))]
fit = survfit(Surv(Time, Event) ~ RiskGroup, data = newdata)
plot=ggsurvplot(fit, data = km_data,
                title = "Trainset",
                surv.median.line = "hv", 
                legend.title = "Risk",
                legend.labs = c("high", "low"), 
                legend="top",
                pval = TRUE, 
                pval.method = TRUE, 
                conf.int = TRUE, 
                risk.table = T, 
                xlab ="Time(months)",
                #tables.height = 0.2, 
                tables.theme = theme_survminer(),
                palette = c("#ED0000FF",  "#0099B4FF"), 
                ggtheme = theme_survminer() 
                
)
pdf("Train_risk_survival.pdf",5,5)
print(plot,newpage = FALSE)
dev.off()

km_data = testplot_data
km_data$RiskGroup = ifelse(km_data$GBM > Med, "high", "low")
newdata = km_data[,c(1,2,ncol(km_data))]
fit = survfit(Surv(Time, Event) ~ RiskGroup, data = newdata)
plot=ggsurvplot(fit, data = km_data,
                title = "Testset",
                surv.median.line = "hv", 
                legend.title = "Risk", 
                legend.labs = c("high", "low"), 
                legend="top",
                pval = TRUE, 
                pval.method = TRUE, 
                conf.int = TRUE, 
                risk.table = T, 
                xlab ="Time(months)",
                tables.theme = theme_survminer(), 
                palette = c("#ED0000FF",  "#0099B4FF"), 
                ggtheme = theme_survminer() 
                
)
pdf("Test_risk_survival.pdf",5,5)
print(plot,newpage = FALSE)
dev.off()
