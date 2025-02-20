library(plotROC)
library(pROC)
library(ggplot2)
library(survival)
library(survminer)
library(foreign)
library(glmnet)
library(randomForestSRC)
library(CoxBoost)
library(survivalsvm)
library(caret)
library(gbm)
library(riskRegression)
library(pec)
library(tidyr)
library(dcurves)
library(ggDCA)
library(timeROC)
library(survcomp)
library(survex)
library(ModelMetrics)
library(rms)
library(autoReg)
library(readr)
library(pbapply)
library(ggimage)
library(rsvg)
library(rlang)
library(tidyverse)
library(reshape2)
library(openxlsx)
library(DALEX)
library(dplyr)
setwd()
traindata = read.csv()
testdata = read.csv()
table(is.na(traindata))
traindata <- na.omit(traindata)
colnames(traindata)[colnames(data) == "status"] <- "Event"
colnames(traindata)[colnames(data) == "time"] <- "Time"
traindata[ , 4:6] <- lapply(traindata[ , 4:6], as.factor)

rt=traindata
cox_fit = coxph(Surv(Time,Event)~.,data=rt)
reuslt_cox = autoReg(cox_fit,uni=TRUE,milti=TRUE,threshold=0.05)
reuslt_cox
write.csv(reuslt_cox,"训练集单因素多因素Cox回归.csv",row.names = F)

sigvar=c(variables)
traindata = traindata[,c("Time","Event",sigvar)]
testdata = testdata[,c("Time","Event",sigvar)]
traindata$variables <- factor(traindata$variables, levels = c(), labels = c())
testdata$variables <- factor(testdata$variables, levels = c(), labels = c())
nomogram_fit = cph(Surv(Time, Event) ~ variables,
                   x = T, y = T, surv = T,
                   data = traindata) 
print(nomogram_fit) 

dd = datadist(traindata) 
options(datadist = "dd") 

med = Quantile(nomogram_fit) 
surv = Survival(nomogram_fit) 
nom = nomogram(nomogram_fit, fun = list(function(x) surv(12, x),
                                        function(x) surv(12*2, x),
                                        function(x) surv(12*3, x)),
               funlabel = c("1-year Survival Probability",
                            "2-year Survival Probability",
                            "3-year Survival Probability"), lp = T)
pdf("Nomogram.pdf",10,7,family = "serif")
plot(nom) 
dev.off()

model = coxph(Surv(Time,Event)~.,data=traindata)
traindata$Nomogram = predict(model,newdata = traindata,type = 'risk')
testdata$Nomogram = predict(model,newdata = testdata,type = 'risk')


formula_str = paste("Surv(Time, Event) ~", paste(colnames(traindata)[3:(ncol(traindata)-1)], collapse = "+"))
formula_cox = as.formula(formula_str)

pdf(file="calibration_train.pdf", width=5, height=5)

f = cph(formula_cox, x=T, y=T, surv=T, data=traindata, time.inc=12)
cal = calibrate(f, cmethod="KM", method="boot", u=12, m=(nrow(traindata)/3), B=1000)
plot(cal, xlim=c(0,1), ylim=c(0,1),
     xlab="Nomogram-predicted OS (%)", ylab="Observed OS (%)", lwd=1.5, col="#00468BFF", sub=F)

f = cph(formula_cox, x=T, y=T, surv=T, data=traindata, time.inc=24)
cal = calibrate(f, cmethod="KM", method="boot", u=24, m=(nrow(traindata)/3), B=1000)
plot(cal, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", lwd=1.5, col="#ED0000FF", sub=F, add=T)

f = cph(formula_cox, x=T, y=T, surv=T, data=traindata, time.inc=36)
cal = calibrate(f, cmethod="KM", method="boot", u=36, m=(nrow(traindata)/3), B=1000)
plot(cal, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="",  lwd=1.5, col="#42B540FF", sub=F, add=T)
legend('bottomright', c('1-year', '2-year', '3-year'),
       col=c("#00468BFF" ,"#ED0000FF" ,"#42B540FF"), lwd=1.5, bty = 'n')
dev.off()




formula_str = paste("Surv(Time, Event) ~", paste(colnames(testdata)[3:ncol(testdata)], collapse = "+"))
formula_cox = as.formula(formula_str)

pdf(file="calibration_test.pdf", width=5, height=5)

f = cph(formula_cox, x=T, y=T, surv=T, data=testdata, time.inc=12)
cal = calibrate(f, cmethod="KM", method="boot", u=12, m=(nrow(testdata)/3), B=1000)
plot(cal, xlim=c(0,1), ylim=c(0,1),
     xlab="Nomogram-predicted OS (%)", ylab="Observed OS (%)", lwd=1.5, col="#00468BFF", sub=F)

f = cph(formula_cox, x=T, y=T, surv=T, data=testdata, time.inc=24)
cal = calibrate(f, cmethod="KM", method="boot", u=24, m=(nrow(testdata)/3), B=1000)
plot(cal, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", lwd=1.5, col="#ED0000FF", sub=F, add=T)

f = cph(formula_cox, x=T, y=T, surv=T, data=testdata, time.inc=36)
cal = calibrate(f, cmethod="KM", method="boot", u=36, m=(nrow(testdata)/3), B=1000)
plot(cal, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="",  lwd=1.5, col="#42B540FF", sub=F, add=T)
legend('bottomright', c('1-year', '2-year', '3-year'),
       col=c("#00468BFF" ,"#ED0000FF" ,"#42B540FF"), lwd=1.5, bty = 'n')
dev.off()


traindca_data = traindata[,c(1,2)]
traindca_data$Nomogram = c(1-summary(survfit(model,newdata = traindata),times=12)$surv)
traindca=dcurves::dca(Surv(Time, Event) ~ Nomogram, data = traindca_data,time = 12)
pdf("Train_DCA_1year.pdf",6,5.5,family = "serif")
as_tibble(traindca) %>%
  dplyr::filter(!is.na(net_benefit)) %>%
  ggplot(aes(x = threshold, y = net_benefit, color = label)) +
  geom_line(lwd=1.2) +
  coord_cartesian(ylim = c(-0.05, 0.5)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = "Threshold Probability", y = "Net Benefit", color = "") +
  
  theme_classic()
dev.off()

traindca_data = traindata[,c(1,2)]
traindca_data$Nomogram = c(1-summary(survfit(model,newdata = traindata),times=24)$surv)
traindca=dcurves::dca(Surv(Time, Event) ~ Nomogram, data = traindca_data,time = 24)
pdf("Train_DCA_2year.pdf",6,5.5,family = "serif")
as_tibble(traindca) %>%
  dplyr::filter(!is.na(net_benefit)) %>%
  ggplot(aes(x = threshold, y = net_benefit, color = label)) +
  geom_line(lwd=1.2) +
  coord_cartesian(ylim = c(-0.05, 0.5)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = "Threshold Probability", y = "Net Benefit", color = "") +
  
  theme_classic()
dev.off()

traindca_data = traindata[,c(1,2)]
traindca_data$Nomogram = c(1-summary(survfit(model,newdata = traindata),times=36)$surv)
traindca=dcurves::dca(Surv(Time, Event) ~ Nomogram, data = traindca_data,time = 36)
pdf("Train_DCA_3year.pdf",6,5.5,family = "serif")
as_tibble(traindca) %>%
  dplyr::filter(!is.na(net_benefit)) %>%
  ggplot(aes(x = threshold, y = net_benefit, color = label)) +
  geom_line(lwd=1.2) +
  coord_cartesian(ylim = c(-0.05, 0.5)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = "Threshold Probability", y = "Net Benefit", color = "") +
 
  theme_classic()
dev.off()

testdca_data = testdata[,c(1,2)]
testdca_data$Nomogram = c(1-summary(survfit(model,newdata = testdata),times=12)$surv)
testdca=dcurves::dca(Surv(Time, Event) ~ Nomogram, data = testdca_data,time = 12)
pdf("Test_DCA_1year.pdf",6,5.5,family = "serif")
as_tibble(testdca) %>%
  dplyr::filter(!is.na(net_benefit)) %>%
  ggplot(aes(x = threshold, y = net_benefit, color = label)) +
  geom_line(lwd=1.2) +
  coord_cartesian(ylim = c(-0.05, 0.5)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = "Threshold Probability", y = "Net Benefit", color = "") +
  
  theme_classic()
dev.off()

testdca_data = testdata[,c(1,2)]
testdca_data$Nomogram = c(1-summary(survfit(model,newdata = testdata),times=24)$surv)
testdca=dcurves::dca(Surv(Time, Event) ~ Nomogram, data = testdca_data,time = 24)
pdf("Test_DCA_2year.pdf",6,5.5,family = "serif")
as_tibble(testdca) %>%
  dplyr::filter(!is.na(net_benefit)) %>%
  ggplot(aes(x = threshold, y = net_benefit, color = label)) +
  geom_line(lwd=1.2) +
  coord_cartesian(ylim = c(-0.05, 0.5)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = "Threshold Probability", y = "Net Benefit", color = "") +
  
  theme_classic()
dev.off()


testdca_data = testdata[,c(1,2)]
testdca_data$Nomogram = c(1-summary(survfit(model,newdata = testdata),times=36)$surv)
testdca=dcurves::dca(Surv(Time, Event) ~ Nomogram, data = testdca_data,time = 36)
pdf("Test_DCA_3year.pdf",6,5.5,family = "serif")
as_tibble(testdca) %>%
  dplyr::filter(!is.na(net_benefit)) %>%
  ggplot(aes(x = threshold, y = net_benefit, color = label)) +
  geom_line(lwd=1.2) +
  coord_cartesian(ylim = c(-0.05, 0.5)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = "Threshold Probability", y = "Net Benefit", color = "") +
  
  theme_classic()
dev.off()



dead=subset(traindata,traindata$Event==1)
survival=subset(traindata,traindata$Event==0)
time=12   
dead$Result=ifelse(dead$Time>time,0,1)
survival$Result=0
ROC_data_1year=rbind(dead,survival)


dead=subset(traindata,traindata$Event==1)
survival=subset(traindata,traindata$Event==0)
time=24   
dead$Result=ifelse(dead$Time>time,0,1)
survival$Result=0
ROC_data_2year=rbind(dead,survival)


dead=subset(traindata,traindata$Event==1)
survival=subset(traindata,traindata$Event==0)
time=36 
dead$Result=ifelse(dead$Time>time,0,1)
survival$Result=0
ROC_data_3year=rbind(dead,survival)


ROC_1year<-roc(response=ROC_data_1year$Result,predictor=ROC_data_1year$Nomogram)
AUC_1year=round(pROC::auc(ROC_1year),3)
CI_1year=ci.auc(ROC_1year)

ROC_2year<-roc(response=ROC_data_2year$Result,predictor=ROC_data_2year$Nomogram)
AUC_2year=round(pROC::auc(ROC_2year),3)
CI_2year=ci.auc(ROC_2year)

ROC_3year<-roc(response=ROC_data_3year$Result,predictor=ROC_data_3year$Nomogram)
AUC_3year=round(pROC::auc(ROC_3year),3)
CI_3year=ci.auc(ROC_3year)


allroc=list(ROC_1year=ROC_1year,ROC_2year=ROC_2year,ROC_3year=ROC_3year)

plot_roc = pROC::ggroc(allroc,
                       size=1,
                       legacy.axes = T
)+theme_bw()+
  
  labs(title = 'ROC curve')+
  theme(
    plot.title = element_text(hjust = 0.5,size = 15,face="bold"),
    axis.text=element_text(size=12,face="bold"),
    legend.title = element_blank(),
    legend.text = element_text(size=9,face="bold"),
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
    breaks=c("ROC_1year","ROC_2year","ROC_3year"),#
    labels=c(paste0("Nomogram 1 year(AUC=",sprintf("%0.3f", AUC_1year),",95%CI(",
                    sprintf("%0.3f", CI_1year[1]),"-",sprintf("%0.3f", CI_1year[3]),")"),
             paste0("Nomogram 2 year(AUC=",sprintf("%0.3f", AUC_2year),",95%CI(",
                    sprintf("%0.3f", CI_2year[1]),"-",sprintf("%0.3f", CI_2year[3]),")"),
             paste0("Nomogram 3 year(AUC=",sprintf("%0.3f", AUC_3year),",95%CI(",
                    sprintf("%0.3f", CI_3year[1]),"-",sprintf("%0.3f", CI_3year[3]),")")
    ))
pdf(paste0("Train_ROC.pdf"),5,5,family = "serif")
print(plot_roc)
dev.off()



dead=subset(testdata,testdata$Event==1)
survival=subset(testdata,testdata$Event==0)
time=12   
dead$Result=ifelse(dead$Time>time,0,1)
survival$Result=0
ROC_data_1year=rbind(dead,survival)


dead=subset(testdata,testdata$Event==1)
survival=subset(testdata,testdata$Event==0)
time=24  
dead$Result=ifelse(dead$Time>time,0,1)
survival$Result=0
ROC_data_2year=rbind(dead,survival)


dead=subset(testdata,testdata$Event==1)
survival=subset(testdata,testdata$Event==0)
time=36   
dead$Result=ifelse(dead$Time>time,0,1)
survival$Result=0
ROC_data_3year=rbind(dead,survival)


ROC_1year<-roc(response=ROC_data_1year$Result,predictor=ROC_data_1year$Nomogram)
AUC_1year=round(pROC::auc(ROC_1year),3)
CI_1year=ci.auc(ROC_1year)

ROC_2year<-roc(response=ROC_data_2year$Result,predictor=ROC_data_2year$Nomogram)
AUC_2year=round(pROC::auc(ROC_2year),3)
CI_2year=ci.auc(ROC_2year)

ROC_3year<-roc(response=ROC_data_3year$Result,predictor=ROC_data_3year$Nomogram)
AUC_3year=round(pROC::auc(ROC_3year),3)
CI_3year=ci.auc(ROC_3year)


allroc=list(ROC_1year=ROC_1year,ROC_2year=ROC_2year,ROC_3year=ROC_3year)

plot_roc = pROC::ggroc(allroc,
                       size=1,
                       legacy.axes = T
)+theme_bw()+
  
  labs(title = 'ROC curve')+
  theme(
    plot.title = element_text(hjust = 0.5,size = 15,face="bold"),
    axis.text=element_text(size=12,face="bold"),
    legend.title = element_blank(),
    legend.text = element_text(size=9,face="bold"),
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
    breaks=c("ROC_1year","ROC_2year","ROC_3year"),
    labels=c(paste0("Nomogram 1 year(AUC=",sprintf("%0.3f", AUC_1year),",95%CI(",
                    sprintf("%0.3f", CI_1year[1]),"-",sprintf("%0.3f", CI_1year[3]),")"),
             paste0("Nomogram 2 year(AUC=",sprintf("%0.3f", AUC_2year),",95%CI(",
                    sprintf("%0.3f", CI_2year[1]),"-",sprintf("%0.3f", CI_2year[3]),")"),
             paste0("Nomogram 3 year(AUC=",sprintf("%0.3f", AUC_3year),",95%CI(",
                    sprintf("%0.3f", CI_3year[1]),"-",sprintf("%0.3f", CI_3year[3]),")")
    ))
pdf(paste0("Test_ROC.pdf"),5,5,family = "serif")
print(plot_roc)
dev.off()


km_data=traindata[,c(1,2,ncol(traindata))]

Median=median(km_data$Nomogram)
km_data$Nomogram=ifelse(km_data$Nomogram>Median,"high","low")

fit = survfit(Surv(Time, Event) ~ Nomogram, data = km_data)
plot=ggsurvplot(fit, data = km_data,
                title = "Trainset",
                surv.median.line = "hv",
                legend.title = "Risk",
                legend.labs = c("high", "low"), 
                legend=c(0.9, 0.9),
                pval = TRUE, 
                pval.method = TRUE, 
                conf.int = TRUE, 
                risk.table = T, 
                xlab ="Time(months)",
                        
                tables.theme = theme_survminer(),
                palette = c("#ED0000FF",  "#0099B4FF"),
                ggtheme = theme_survminer() # Change ggplot2 theme
                
)
pdf("Train_risk_survival.pdf",5,5)
print(plot,newpage = FALSE)
dev.off()


km_data=testdata[,c(1,2,ncol(testdata))]
Median=median(km_data$Nomogram)
km_data$Nomogram=ifelse(km_data$Nomogram>Median,"high","low")

fit = survfit(Surv(Time, Event) ~ Nomogram, data = km_data)
plot=ggsurvplot(fit, data = km_data,
                title = "Testset",
                surv.median.line = "hv", 
                legend.title = "Risk",
                legend.labs = c("high", "low"),
                legend=c(0.9, 0.9),
                pval = TRUE, 
                pval.method = TRUE,
                conf.int = TRUE, 
                risk.table = T, 
                xlab ="Time(months)",
                tables.theme = theme_survminer(), 
                palette = c("#ED0000FF",  "#0099B4FF"), 
                ggtheme = theme_survminer() # Change ggplot2 theme
)
pdf("Test_risk_survival.pdf",5,5)
print(plot,newpage = FALSE)
dev.off()

colnames(traindata)
km_data=traindata[,c(1:(ncol(traindata)-1))]
for (var in colnames(km_data)[3:ncol(km_data)]) {
  newdata = km_data[,c("Time","Event",var)]
  colnames(newdata) = c("Time","Event","var_name")
  fit = survfit(Surv(Time, Event) ~ var_name, data = newdata)
  plot=ggsurvplot(fit, data = newdata,
                  surv.median.line = "hv", 
                  legend=c(0.8, 0.8),
                  legend.title = paste0(var), 
                  legend.labs = levels(newdata$var_name),
                  pval = TRUE, 
                  pval.method = TRUE, 
                  conf.int = TRUE,
                  risk.table = F, 
                  tables.height = 0.2,
                  tables.theme = theme_cleantable(), 
                  palette = c("#ED0000FF" ,"#00468BFF", "#42B540FF" ,"#0099B4FF" ),
                  ggtheme = theme_bw() # Change ggplot2 theme
  )
  pdf(paste0(var,"_KM_plot.pdf"),5,5,family = "serif")
  print(plot,newpage = FALSE)
  dev.off()
}



