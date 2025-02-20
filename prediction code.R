library(pbapply)
library(ggimage)
library(rsvg)
library(rlang)
library(tidyverse)
library(reshape2)
library(openxlsx)
library(DALEX)
library(readr)
library(gbm)
library(dplyr)
library(caret)
library(ggplot2)
library(pROC)
library(rms)
library(rmda)
library(dcurves)
library(Hmisc)
library(ResourceSelection)
library(DynNom)
library(survey)
library(caret)
library(foreign)
library(plotROC)
library(survival)
library(shapper)
library(iml)
library(e1071)
library(ROCR)
library(corrplot)
library(lattice)
library(Formula)
library(SparseM)
library(survival)
library(riskRegression)
library(pheatmap)
library(fastshap)
library(naivebayes)
library(ingredients)
library(mlr3)
library(table1)
library(tableone)
library(adabag)
library(RColorBrewer)
library(VIM)
library(mice)
library(autoReg)
library(cvms)
library(tibble)
library(plotROC)
library(pROC)
library(ggplot2)
library(cvms)
library(tibble)
library(corrplot)
library(data.table)
library(pheatmap)
library(ComplexHeatmap)
library(RColorBrewer)
library(circlize)
library(ROSE)
library(DMwR)
library(scales)
library(catboost)
library(lightgbm)
library(plotROC)
library(pROC)
library(ggplot2)
library(kernelshap)
library(shapviz)
setwd()
data=read.csv()
str(data)
data$Bone<-as.factor(data$Bone)
set.seed(100)
inTrain = createDataPartition(y=data[,"Bone"], p=0.7, list=F)
traindata = data[inTrain,]
testdata = data[-inTrain,]
write.csv(traindata,"dev.csv",row.names = F)
write.csv(testdata,"vad.csv",row.names = F)
dev=traindata
vad=testdata
dev <- dev %>% rename(Result = Bone)
vad <- vad %>% rename(Result = Bone)
str(dev)
str(vad)
library(glmnet)
set.seed(100)
lassox=as.matrix(dev[,c(2:ncol(dev))])
lassoy=data.matrix(dev[,1])
fit = glmnet(lassox, lassoy,family='binomial',
             alpha=1)
pdf("lasso.pdf",5,5)
plot(fit, xvar = "lambda", label = FALSE, lwd = 2, xlab = expression(Log(lambda)))
dev.off()
lasso.cv = cv.glmnet(x=lassox, y=lassoy, family="binomial", nfold=10,
                     alpha=1)
pdf("lasso.cv.pdf",5,5)
plot(lasso.cv,lwd=2)
abline(v=log(c(lasso.cv$lambda.min,lasso.cv$lambda.1se)), lty="dashed",lwd=2)
dev.off()

best_lambda <- lasso.cv$lambda.min
best_lambda
best_model <- glmnet(lassox, lassoy, alpha = 1, family="binomial", lambda = best_lambda)
coef=coef(best_model)
index <- which(coef != 0)
actCoef <- coef[index]

lassovar=row.names(coef)[index]
lassocoef=data.frame(var=lassovar,coef=actCoef)
print(lassocoef)
write.csv(lassocoef,"lasso_ceof.csv",row.names = F)
lassovar=lassovar[-1]
lassovar

var=c("Result","variables")
dev = dev[,var]
vad = vad[,var]
dev$Result <- factor(dev$Result, levels = c(0, 1), labels = c("No", "Yes"))
vad$Result <- factor(vad$Result, levels = c(0, 1), labels = c("No", "Yes"))
str(vad)

set.seed(100)
models = c("glm","svmRadial","gbm","nnet","kknn","nb","rf")

models_names = list(Logistic="glm",SVM="svmRadial",GBM="gbm",NeuralNetwork="nnet",KNN="kknn",RandomForest="rf",NaiveBayes="nb")

glm.tune.grid = NULL
svm.tune.grid = expand.grid(sigma = 0.001, C = 0.09)
gbm.tune.grid = expand.grid(
  n.trees = 100,
  interaction.depth = 5,
  shrinkage = 0.1,
  n.minobsinnode = 10
)
nnet.tune.grid = expand.grid(size = 6,decay = 0.6)
knn.tune.grid = expand.grid(kmax = 12 ,distance = 1,kernel = "optimal")
rf.tune.grid = expand.grid(mtry = 10)
nb.tune.grid = expand.grid(fL = 1, 
                           usekernel = c(TRUE, FALSE), 
                           adjust = c(1, 1.5, 2))


Tune_table = list(glm = glm.tune.grid,
                  svmRadial = svm.tune.grid,
                  gbm = gbm.tune.grid,
                  nnet = nnet.tune.grid,
                  kknn = knn.tune.grid,
                  rf = rf.tune.grid,
                  nb = nb.tune.grid
                  
)

train_probe = data.frame(Result = dev$Result)
test_probe = data.frame(Result = vad$Result)
importance = list()
ML_calss_model = list()
set.seed(100)
train.control <- trainControl(
  method = 'repeatedcv',
  number = 5,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

pb = txtProgressBar(min = 0, max = length(models), style = 3)
for (i in seq_along(models)) {
  model <- models[i]
  model_name <- names(models_names)[which(models_names == model)]  
  set.seed(100)
  fit = train(Result~.,
              data = dev,
              tuneGrid = Tune_table[[model]],
              metric='ROC',
              method= model,
              trControl=train.control)
  
  train_Pro = predict(fit, newdata = dev, type = 'prob')
  test_Pro = predict(fit, newdata = vad, type = 'prob')
  
  train_probe[[model_name]] <- train_Pro$Yes
  test_probe[[model_name]] <- test_Pro$Yes
  
  ML_calss_model[[model_name]] = fit  
  importance[[model_name]] = varImp(fit, scale = TRUE) 
  
  setTxtProgressBar(pb, i)
}
close(pb)  

for(model_name in names(models_names)){

  imp = importance[[model_name]]

  imp_table <- as.data.frame(imp$importance)
  imp_table$Features <- rownames(imp_table)

  if ("Yes" %in% colnames(imp_table)) {
    fill_col <- "Yes"
  } else if ("Overall" %in% colnames(imp_table)) {
    fill_col <- "Overall"
  } else {
    stop("Neither 'Yes' nor 'Overall' column found in importance table.")
  }

  g = ggplot(imp_table, aes(x = !!sym(fill_col), y = reorder(Features, !!sym(fill_col))))
  p2 = g + geom_bar(aes(fill = !!sym(fill_col)), stat = "identity", width = 0.6, position = position_stack(reverse = TRUE), size = 1) +
    theme_classic() + scale_fill_gradient() +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.position = "none",
          axis.text = element_text(size = 10, face = "bold", color = "black"),
          axis.title.x = element_text(size = 12, face = "bold", color = "black"),
          axis.title.y = element_text(size = 12, face = "bold", color = "black"),
          legend.title = element_text(size = 12), legend.text = element_text(size = 12)) +
    labs(x = "Importance Scores", y = "Features", title = paste0(model_name, " "))
  
  pdf(paste0(model_name, "_important.pdf"), 7, 5, family = "serif")
  print(p2)
  dev.off()
}



models_names = list(Logistic="glm",SVM="svmRadial",GBM="gbm",NeuralNetwork="nnet",
                    KNN="kknn",NaiveBayes="nb",RandomForest="rf")#

Train = train_probe
Test = test_probe


datalist = list(Train= train_probe,
                Test= test_probe)

for (newdata_tt in names(datalist)) {
  
  newdata = datalist[[newdata_tt]]

  formula = as.formula(paste0("Result ~ ", paste(colnames(newdata)[2:ncol(newdata)], collapse = " + ")))
  trellis.par.set(caretTheme())
  cal_obj = calibration(formula, data = newdata, class = 'Yes',cuts = 4)
  caldata=as.data.frame(cal_obj$data)
  caldata=na.omit(caldata)

  Calibrat_plot=ggplot(data = caldata,aes(x=midpoint,
                                          y=Percent,
                                          group = calibModelVar,
                                          color=calibModelVar))+
    geom_point(size=1)+
    geom_line(linewidth=0.65)+
    geom_abline(slope = 1, intercept = 0 ,color="black",linetype = 'dotdash')+
    xlab("Bin Midpoint")+
    ylab("Observed Event Percentage")+
    theme_bw() 
    theme(
      plot.title = element_text(hjust = 0.5,size = 15,face="bold"),
      axis.text=element_text(size=12,face="bold"),
      legend.position=c(0.9,0.3),
      legend.background = element_blank(),
      axis.title.y = element_text(size=12,face="bold"),
      axis.title.x = element_text(size=12,face="bold"),
      panel.border = element_rect(color="black",size=1),
      panel.background = element_blank())+
    scale_color_discrete(name = "Model")+coord_cartesian(xlim = c(0, max(caldata$midpoint, na.rm = TRUE)),
                                                         ylim = c(0, max(caldata$Percent, na.rm = TRUE))) 
  pdf(paste0(newdata_tt,"Calibration.pdf"),5,5,family = "serif")
  print(Calibrat_plot)
  dev.off()
  
  
  ROC_list = list()
  ROC_label = list()
  AUC_metrics = data.frame()
  Evaluation_metrics = data.frame(Model = NA,Threshold=NA,Accuracy=NA,Sensitivity=NA,Specificity=NA,Precision=NA,F1=NA)

  for (model_name in names(models_names)) {
    
    ROC = roc(response=newdata$Result,predictor=newdata[,model_name])
    AUC = round(auc(ROC),3)
    CI = ci.auc(ROC)
    label = paste0(model_name," (AUC=",sprintf("%0.3f", AUC),",95%CI:",sprintf("%0.3f", CI[1]),"-",sprintf("%0.3f", CI[3]),")")
    
    bestp = ROC$thresholds[
      which.max(ROC$sensitivities+ROC$specificities-1)
    ]
    
    predlab = as.factor(ifelse(newdata[,model_name] > bestp,"Yes","No"))
    
    index_table = confusionMatrix(data = predlab,
                                  reference = newdata$Result,
                                  positive = "Yes",
                                  mode="everything")
    
    mydata = data.frame(reference=newdata$Result,prediction=predlab)
    mytibble =  as.tibble(table(mydata))
    
    confusion_plot = plot_confusion_matrix(mytibble,
                                           target_col = "reference",
                                           prediction_col = "prediction",
                                           counts_col = "n",
                                           sub_col = NULL,
                                           class_order = NULL,
                                           add_sums = FALSE,
                                           add_counts = TRUE,
                                           add_normalized = TRUE,
                                           add_row_percentages = F,
                                           add_col_percentages = F,
                                           diag_percentages_only = FALSE,
                                           rm_zero_percentages = TRUE,
                                           rm_zero_text = TRUE,
                                           add_zero_shading = TRUE,
                                           amount_3d_effect = 1,
                                           add_arrows = TRUE,
                                           counts_on_top = FALSE,
                                           palette = "Blues",
                                           intensity_by = "counts",
                                           intensity_lims = NULL,
                                           intensity_beyond_lims = "truncate",
                                           theme_fn = ggplot2::theme_minimal,
                                           place_x_axis_above = TRUE,
                                           rotate_y_text = TRUE,
                                           digits = 1,
                                           font_counts = font(),
                                           font_normalized = font(),
                                           font_row_percentages = font(),
                                           font_col_percentages = font(),
                                           arrow_size = 0.048,
                                           arrow_nudge_from_text = 0.065,
                                           tile_border_color = NA,
                                           tile_border_size = 0.1,
                                           tile_border_linetype = "solid",
                                           sums_settings = sum_tile_settings(),
                                           darkness = 0.8)
    
    pdf(paste0(newdata_tt,model_name,"_cm_plot.pdf"),5,5,family = "serif")
    print(confusion_plot)
    dev.off()
    Evaluation_metrics = rbind(Evaluation_metrics,c(Model = model_name, Threshold = bestp,  
                                                    Accuracy = sprintf("%0.3f",index_table[["overall"]][["Accuracy"]]),
                                                    Sensitivity = sprintf("%0.3f",index_table[["byClass"]][["Sensitivity"]]),
                                                    Specificity = sprintf("%0.3f",index_table[["byClass"]][["Specificity"]]),
                                                    Precision = sprintf("%0.3f",index_table[["byClass"]][["Precision"]]),
                                                    F1 = sprintf("%0.3f",index_table[["byClass"]][["F1"]])  )
    )
    
    ROC_label[[model_name]] = label
    ROC_list[[model_name]] = ROC
    
  }
  write.csv(Evaluation_metrics,paste0(newdata_tt,"_Evaluation_metrics.csv"),row.names = F)
  
  
  
  ROC_plot=pROC::ggroc(ROC_list,size=1.5,legacy.axes = T)+theme_bw()+
    labs(title = ' ROC curve')+
    theme(plot.title = element_text(hjust = 0.5,size = 15,face="bold"),
          axis.text=element_text(size=12,face="bold"),
          legend.title = element_blank(),
          legend.text = element_text(size=12,face="bold"),
          legend.position=c(0.7,0.25),
          legend.background = element_blank(),
          axis.title.y = element_text(size=12,face="bold"),
          axis.title.x = element_text(size=12,face="bold"),
          panel.border = element_rect(color="black",size=1),
          panel.background = element_blank())+
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),colour='grey',linetype = 'dotdash')+
    scale_colour_discrete(
      breaks=c(names(models_names)),
      labels=c(ROC_label))
  pdf(paste0(newdata_tt,"_ROC.pdf"),7,7,family = "serif")
  print(ROC_plot)
  dev.off()
  
  dca_data = newdata
  dca_data$Result=ifelse(dca_data$Result=="Yes",1,0)
  
  DCA_list = list()
  for (model_name in names(models_names)) {
    dca_formula = as.formula(paste("Result ~", model_name))
    set.seed(100)
    dca_curvers = decision_curve(dca_formula, 
                                 data = dca_data, 
                                 study.design = "cohort", 
                                 bootstraps = 50 
    )
    DCA_list[[model_name]] = dca_curvers
  }
  
  dca = setNames(DCA_list, names(DCA_list))

  pdf(paste0(newdata_tt,"DCA.pdf"),7,7,family = "serif")
  plot_decision_curve(dca, curve.names = c(names(models_names)),
                      cost.benefit.axis = F, 
                      confidence.intervals = "none" ,
                      lwd = 2,
                      legend.position ="topright")+theme(
                        plot.title = element_text(hjust = 0.5,size = 15,face="bold"),
                        axis.text=element_text(size=12,face="bold"),
                        legend.title = element_blank(),
                        legend.text = element_text(size=12,face="bold"),
                     
                        legend.background = element_blank(),
                        axis.title.y = element_text(size=12,face="bold"),
                        axis.title.x = element_text(size=12,face="bold"),
                        panel.border = element_rect(color="black",size=1),
                        panel.background = element_blank())
  dev.off()
  write.csv(dca_data,paste0(newdata_tt,"_PRplot.csv"),row.names = F)
}

calculate_brier_score <- function(pred_prob, actual) {

  actual_numeric = as.numeric(as.factor(actual)) - 1

  mean((pred_prob - actual_numeric)^2)
}

Brier_Scores_All = data.frame(DataSet = character(), Model = character(), BrierScore = numeric())

for (newdata_tt in names(datalist)) {
  
  newdata = datalist[[newdata_tt]]
  
  Brier_Scores = data.frame(Model = character(), BrierScore = numeric())
  
  for (model_name in names(models_names)) {

    pred_prob = newdata[, model_name]
    actual = newdata$Result

    brier = calculate_brier_score(pred_prob, actual)
    
    Brier_Scores = rbind(Brier_Scores, data.frame(Model = model_name, BrierScore = round(brier, 4)))
  }
  
  Brier_Scores$DataSet = newdata_tt
  

  Brier_Scores_All = rbind(Brier_Scores_All, Brier_Scores)
}

write.csv(Brier_Scores_All, "Brier_Scores_All.csv", row.names = FALSE)


n_train = 
n_test =

names(models_names)

best_Model = 

explain_kernel = kernelshap(ML_calss_model[[best_Model]], dev[1:n_train,-1], bg_X = vad[1:n_test,-1])  

shap_value = shapviz(explain_kernel,X_pred = dev[1:n_train,-1], interactions = TRUE) 

pdf(paste0("SHAP_",best_Model,"_sv_force_.pdf"),7,5)
sv_force(shap_value$Yes, row_id = 12,size = 9)+
  ggtitle(label = paste0("",best_Model))+
  theme(plot.title = element_text(hjust = 0.5,face = "bold",color = "black"))
dev.off()

pdf(paste0("SHAP_",best_Model,"_importance_beeswarm_.pdf"),7,5)
sv_importance(shap_value$Yes, kind = "beeswarm", 
              viridis_args = list(begin = 0.25, end = 0.85, option = "B"),#A-H
              show_numbers = F)+
  ggtitle(label = paste0("",best_Model))+
  theme_bw()+
  ggtitle(label = paste0("",best_Model))+
  theme(plot.title = element_text(hjust = 0.5,face = "bold",color = "black"))
dev.off()


pdf(paste0("SHAP_",best_Model,"_importance_bar_.pdf"),7,5)
sv_importance(shap_value$Yes, kind = "bar", show_numbers = F,
              fill = "#fca50a",
              class = "Yes")+
  theme_bw()+
  ggtitle(label = paste0("",best_Model))+
  theme(plot.title = element_text(hjust = 0.5,face = "bold",color = "black"))
dev.off()

pdf(paste0("SHAP_",best_Model,"_waterfall_.pdf"),5,5)
sv_waterfall(shap_value$Yes, row_id = 12,
             fill_colors = c("#f7d13d", "#a52c60"))+
  theme_bw()+
  ggtitle(label = paste0("",best_Model))+
  theme(plot.title = element_text(hjust = 0.5,face = "bold",color = "black"))
dev.off()


