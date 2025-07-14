library(survival)
library(survminer)
library(foreign)
library(glmnet)
library(timeROC)
library(pROC)
library(plotROC)
library(ggplot2)
setwed("your folder path for saving results")
trainplot_data = read.csv("your traindata file path")
trainplot_data <- na.omit(trainplot_data)
testplot_data = read.csv("your testdata file path")
testplot_data <- na.omit(testplot_data)
km_data = trainplot_data
Med =  median(km_data$best_model_prob, na.rm = TRUE)
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
km_data$RiskGroup = ifelse(km_data$best_model_prob > Med, "high", "low")
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
