library(gdata)
library(pROC)
library(ResourceSelection)
library(randomForest)
library(survival)
library(survminer)
library(dplyr)
library(finalfit)
library("chron")
library(ggplot2)
library("gtsummary")
library(plyr)
library(sjPlot)
library(sjmisc)
library(sjstats)
library("xlsx")
library(partykit)
library(stringr)
library(lares)
library(ggcorrplot)
library(caret)
library(grid)
library("lattice")
library("ROCR")
library(h2o)
library(jsonlite)
library (plyr)
library(ModelMetrics)
library(ggfortify)
library(corrplot)
library(ggplot2)
library(gridBase)
library(gridExtra)
library(PASWR2)
library(party)
library(repmis)
library(mgcv)
library(rpart)
library(party)
library(rpart.plot)
library(DT)
library(rpart.plot)
library(webshot)
library(markdown)
library(knitr)
library(knitLatex)
library(knitrBootstrap)
library(datasets)
library(aod)
library(epiR)
library(PredictABEL)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
library(gridExtra)
library(dplyr)
library(lattice)
library(ggplot2)
library(cluster)
library(mlbench)
library(randomForest)
library(forestFloor)
library(AUC)
library(gnm)
library(pROC)
library(Epi)
library(caTools)
library(lubridate)
library(Distance)
library(ResourceSelection)
library(PredictABEL)
library(e1071)
library(splitstackshape)
library(tidyr)
library(downloader)
library(reshape)
library(reshape2)
library(sjmisc)
library(stringi)
library(forecast)
library(plyr)
library(TTR)
library(earth)
library(gbm)
library(splines)
library(parallel)
library(papeR)
library(data.table) 
library(rfUtilities)
library(sp)
library("papeR")
library(xts)
library(zoo)
library("arsenal")
library(timeSeries)

setwd("~/Desktop/Nürtingen Projekt")
DATA <- read.csv("DATA_1A.csv", header = TRUE)
DATA[is.na(DATA)] = 0
#We are changing letters for gender from German to English. 
DATA$Gender <- as.factor(str_replace(DATA$Gender, "W", "F"))
DATA$Gender <- as.factor(str_replace(DATA$Gender, "M ", "M"))
#Due to the fact that none of the sample patients belonged to either of the two study groups (ICU and non-ICU group), ASA I (n=10) and ASA IV (n=2) were excluded. 
DATA <- subset(DATA, !DATA$ASA==1 & !DATA$ASA==4)
#Converting the characters to factor 
DATA$Patients <- as.factor(DATA$Patients)
DATA$ICU.Admission <- as.factor(DATA$ICU.Admission)
DATA$Gender <- as.factor(DATA$Gender)
DATA$ASA <- as.factor(DATA$ASA)

# We created a 3D list, in which rows were parameters, columns were time, and depth was the number of patients (we can imagine the 3-dimensional arrays), as shown in Figure 1.    The pivot function was used to reshape data.  While Static variables and  response variables follow the flattening function. 
Flatening <- function (DATA){
  
  Data_flatening <- function(DATA) {  
    Staticvariable <-DATA[1,c(2:6,8,9)]
    ICU.Admission <- DATA[1, 17]
    Dynamicvaribale <- DATA[, c(7,10:16)]
    
    Dynamicvaribale_F <-  data.frame(pivot_wider(Dynamicvaribale, names_from = Time, values_from = c("Sevo", "Noradrenalin", "Akrinor", "SBP", "DBP","HR", "SpO2")))
    Data <- data.frame(Staticvariable,Dynamicvaribale_F,  ICU.Admission)
    Data
  }
  
  split_list_data <- split(DATA, DATA$Patients)
  Patient_list<- lapply(split_list_data, FUN = Data_flatening)
  
  data = rbindlist(Patient_list)
  data
  
}

Patients_data<- Flatening(DATA)
#Calculating the Propofol and Sufentanil per weight
Patients_data$Sufentanil <- round(Patients_data$Sufentanil/Patients_data$Weight, 2)
Patients_data$Propofol <- round(Patients_data$Propofol/Patients_data$Weight,2)

#Calculating the cumulative Noradrenaline and Akrinor in 20 minutes of Anesthesia.
Noradrenalin_0_min <- round(Patients_data$Noradrenalin_0.00.min/Patients_data$Weight,2)
Noradrenalin_5_min <- round((Patients_data$Noradrenalin_0.05.min+Patients_data$Noradrenalin_0.00.min)/Patients_data$Weight,2)
Noradrenalin_10_min <- round((Patients_data$Noradrenalin_0.10.min+Patients_data$Noradrenalin_0.05.min+Patients_data$Noradrenalin_0.00.min)/Patients_data$Weight,2)
Noradrenalin_15_min <- round((Patients_data$Noradrenalin_0.15.min+Patients_data$Noradrenalin_0.10.min+Patients_data$Noradrenalin_0.05.min+Patients_data$Noradrenalin_0.00.min)/Patients_data$Weight,2)
Noradrenalin_20_min <- round((Patients_data$Noradrenalin_0.20.min+Patients_data$Noradrenalin_0.15.min+Patients_data$Noradrenalin_0.10.min+Patients_data$Noradrenalin_0.05.min+Patients_data$Noradrenalin_0.00.min)/Patients_data$Weight,2)

Akrinor_0_min <- round(Patients_data$Akrinor_0.00.min/Patients_data$Weight,2)
Akrinor_5_min <- round((Patients_data$Akrinor_0.05.min+Patients_data$Akrinor_0.00.min)/Patients_data$Weight,2)
Akrinor_10_min <- round((Patients_data$Akrinor_0.10.min+Patients_data$Akrinor_0.05.min+Patients_data$Akrinor_0.00.min)/Patients_data$Weight,2)
Akrinor_15_min <- round((Patients_data$Akrinor_0.15.min+Patients_data$Akrinor_0.10.min+Patients_data$Akrinor_0.05.min+Patients_data$Akrinor_0.00.min)/Patients_data$Weight,2)
Akrinor_20_min <- round((Patients_data$Akrinor_0.15.min+Patients_data$Akrinor_0.15.min+Patients_data$Akrinor_0.10.min+Patients_data$Akrinor_0.05.min+Patients_data$Akrinor_0.00.min)/Patients_data$Weight,2)

#Eliminating the columns with non-cumulative Noradrenaline and Akrinor in 20 minutes of Anesthesia from the dataset.
Patients_data <- Patients_data[, -c("Noradrenalin_0.00.min","Noradrenalin_0.05.min", "Noradrenalin_0.10.min", "Noradrenalin_0.15.min", "Noradrenalin_0.20.min",
                                   "Akrinor_0.00.min","Akrinor_0.05.min", "Akrinor_0.10.min", "Akrinor_0.10.min", "Akrinor_0.15.min", "Akrinor_0.20.min" )]

# Adding the cumulative Noradrenaline and Akrinor in 20 minutes of Anesthesia in the dataset. 
Patients_data <- data.frame(Patients_data[, c(1:32)], Noradrenalin_0_min=Noradrenalin_0_min, Noradrenalin_5_min=Noradrenalin_5_min, Noradrenalin_10_min=Noradrenalin_10_min, 
                            Noradrenalin_15_min=Noradrenalin_15_min, Noradrenalin_20_min=Noradrenalin_20_min, 
                            Akrinor_0_min=Akrinor_0_min, Akrinor_5_min=Akrinor_5_min, 
                            Akrinor_10_min=Akrinor_10_min, Akrinor_15_min=Akrinor_15_min, 
                            Akrinor_20_min=Akrinor_20_min, Patients_data[, 33])



dim(Patients_data)

#Initiating the deep learning  with H2O package
h2o.init()
DATA = as.h2o(Patients_data)
#Data separation 
DATA_split <- h2o.splitFrame(data = DATA, ratios = 0.2, seed = 1234)
train <- DATA_split[[1]]
test <- DATA_split[[2]]


#A model with optimal values was selected automatically through supervised training. 
hyper_params <- list(activation = c("Rectifier", "Maxout", "Tanh", "RectifierWithDropout", "MaxoutWithDropout", "TanhWithDropout"), 
                     hidden = list(c(50, 50, 50, 50), c(200, 200), c(200, 200, 200), c(200, 200, 200, 200)), 
                     epochs = c(50, 100, 200), 
                     l1 = c(0, 0.00001, 0.0001), 
                     l2 = c(0, 0.00001, 0.0001), 
                     adaptive_rate = c(TRUE, FALSE), 
                     rate = c(0, 0.1, 0.005, 0.001), 
                     rate_annealing = c(1e-8, 1e-7, 1e-6), 
                     rho = c(0.9, 0.95, 0.99, 0.999), 
                     epsilon = c(1e-10, 1e-8, 1e-6, 1e-4), 
                     momentum_start = c(0, 0.5),
                     momentum_stable = c(0.99, 0.5, 0), 
                     input_dropout_ratio = c(0, 0.1, 0.2)
)

search_criteria <- list(strategy = "RandomDiscrete",
                        max_runtime_secs = 10*3600,
                        max_models = 100,
                        stopping_metric = "AUC", 
                        stopping_tolerance = 0.00001, 
                        stopping_rounds = 5, 
                        seed = 1234
)


models_dl <- h2o.grid(algorithm = "deeplearning", grid_id = "grd_deepLearning", x = 1:42, y = 43, training_frame = train, validation_frame = test, nfolds = 0, hyper_params = hyper_params, search_criteria = search_criteria, stopping_metric = "AUC", stopping_tolerance = 1e-3, stopping_rounds = 2, seed = 1234)

models_dl_sort <- h2o.getGrid(grid_id = "grd_deepLearning", sort_by = "auc", decreasing = TRUE)
models_dl_best <- h2o.getModel(models_dl_sort@model_ids[[1]])

# Probability calculation and prediction accuracy and discrimination between groups
pred_MLEMA_deepL_test<- as.data.frame(h2o.predict(models_dl_best, test))

M_deepL_RTest <- data.frame(Test <- as.data.frame(test), RISK=pred_MLEMA_deepL_test$p1)


AUC_model <- function(DL_RTest) {  
  roc <- plot.roc(DL_RTest$ICU.Admission, DL_RTest$RISK, main="Confidence intervals", percent=FALSE,
                  ci=TRUE, # compute AUC (of AUC by default)
                  print.auc=FALSE) # print the AUC (will contain the CI))
  
  Best_closest.topleft_roc <- coords(roc, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy",
                                                        "tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity",
                                                        "1-sensitivity", "1-accuracy", "1-npv", "1-ppv",
                                                        "precision", "recall"),best.method="closest.topleft", transpose = TRUE )
  
  specificity <- Best_closest.topleft_roc[2]
  
  sensitivity <- Best_closest.topleft_roc[3]
  
  accuracy <- Best_closest.topleft_roc[4]
  
  auc <- roc$auc
  CI <- as.vector(roc$ci)
  
  Stat <- data.frame( AUC=auc, CI95lower=CI[1], CI95upper=CI[3], Specificity=specificity, Sensitivity=sensitivity, 
                      Accuracy=accuracy)
  Stat
}

AUC_model(M_deepL_RTest)


###########################################################################################################

#For Practical use during univariate analysis, names of Column were shortened. 

Patients_data <- rename.vars(Patients_data, from = c("Sevo_0.00.min", "Sevo_0.05.min", "Sevo_0.10.min", "Sevo_0.15.min", "Sevo_0.20.min"), 
                             to = c("Sevo_0_min", "Sevo_5_min","Sevo_10_min", "Sevo_15_min", "Sevo_20_min"))

Patients_data <- rename.vars(Patients_data, from = c("HR_0.00.min", "HR_0.05.min", "HR_0.10.min", "HR_0.15.min", "HR_0.20.min"), 
                             to = c("HR_0_min", "HR_5_min","HR_10_min", "HR_15_min", "HR_20_min"))

Patients_data <- rename.vars(Patients_data, from = c("SBP_0.00.min", "SBP_0.05.min", "SBP_0.10.min", "SBP_0.15.min", "SBP_0.20.min"), 
                             to = c("SBP_0_min", "SBP_5_min","SBP_10_min", "SBP_15_min", "SBP_20_min"))

Patients_data <- rename.vars(Patients_data, from = c("DBP_0.00.min", "DBP_0.05.min", "DBP_0.10.min", "DBP_0.15.min", "DBP_0.20.min"), 
                             to = c("DBP_0_min", "DBP_5_min","DBP_10_min", "DBP_15_min", "DBP_20_min"))

Patients_data <- rename.vars(Patients_data, from = c("SpO2_0.00.min", "SpO2_0.05.min", "SpO2_0.10.min", "SpO2_0.15.min", "SpO2_0.20.min"), 
                             to = c("SpO2_0_min", "SpO2_5_min","SpO2_10_min", "SpO2_15_min", "SpO2_20_min"))

#-------------------------------------------------------------------------------------------------------------------------------------------------------------#

#Deep learning univariate analysis function, here we wrote the code that will calculate direct  AUC with 95% CI, including specificity, sensitivity, and accuracy with corresponded probability cut-off according to Youden Index (Table 4).

Univariate_stat <- function(DATA, n) {
  h2o.init()
  DATA <- as.h2o(Patients_data)
  M_LEMA_RF <- h2o.deeplearning(x = n, y = 43, training_frame = DATA)
  pred_M_LEMA_RF<- as.data.frame(h2o.predict(M_LEMA_RF, DATA))
  data_uni<- data.frame(as.data.frame(Parameter <- DATA[, n]) , as.data.frame(ICU.Admission <- DATA[, "ICU.Admission"]), RISK=pred_M_LEMA_RF$p1)
  
  roc <- plot.roc(data_uni$ICU.Admission, data_uni$RISK, main="Confidence intervals", percent=FALSE,
                  ci=TRUE, # compute AUC (of AUC by default)
                  print.auc=FALSE) # print the AUC (will contain the CI))
  
  Best_closest.topleft_roc <- coords(roc, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy",
                                                        "tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity",
                                                        "1-sensitivity", "1-accuracy", "1-npv", "1-ppv",
                                                        "precision", "recall"),best.method="youden",  best.weights=c(1, 0.5), transpose = TRUE)
  
  Threshold_Best_closest.topleft_roc <- Best_closest.topleft_roc[1]
  
  
  Data_DL_threshold  <- subset(data_uni[,1], data_uni[,3]<Threshold_Best_closest.topleft_roc)
  
  Threshold_Best_closest.topleft <- Best_closest.topleft_roc[1]
  
  specificity <- Best_closest.topleft_roc[2]
  
  sensitivity <- Best_closest.topleft_roc[3]
  
  accuracy <- Best_closest.topleft_roc[4]
  
  summary_threshold_values <- summary(Data_DL_threshold)
  min_threshold_values <- as.vector(summary_threshold_values[1])
  max_threshold_values <- as.vector(summary_threshold_values[6])
  
  auc <- roc$auc
  CI <- as.vector(roc$ci)
  
  Stat <- data.frame( AUC=auc, CI95lower=CI[1], CI95upper=CI[3],Cutoff_probabilityt=Threshold_Best_closest.topleft, Specificity=specificity, Sensitivity=sensitivity, 
                      Accuracy=accuracy, min_threshold_values=min_threshold_values, max_threshold_values=max_threshold_values)
  Stat
  
  
}

Age <- Univariate_stat(DATA, "Age")
Gender <- Univariate_stat(DATA, "Gender")
ASA <- Univariate_stat(DATA, "ASA")
Height <- Univariate_stat(DATA, "Height")
Weight <- Univariate_stat(DATA, "Weight")


SBP_0_min <- Univariate_stat(DATA, "SBP_0_min")
SBP_5_min <- Univariate_stat(DATA, "SBP_5_min")
SBP_10_min <- Univariate_stat(DATA, "SBP_10_min")
SBP_15_min <- Univariate_stat(DATA, "SBP_15_min")
SBP_20_min <- Univariate_stat(DATA, "SBP_20_min")


DBP_0_min <- Univariate_stat(DATA, "DBP_0_min")
DBP_5_min <- Univariate_stat(DATA, "DBP_5_min")
DBP_10_min <- Univariate_stat(DATA, "DBP_10_min")
DBP_15_min <- Univariate_stat(DATA, "DBP_15_min")
DBP_20_min <- Univariate_stat(DATA, "DBP_20_min")


HR_0_min <- Univariate_stat(DATA, "HR_0_min")
HR_5_min <- Univariate_stat(DATA, "HR_5_min")
HR_10_min <- Univariate_stat(DATA, "HR_10_min")
HR_15_min <- Univariate_stat(DATA, "HR_15_min")
HR_20_min <- Univariate_stat(DATA, "HR_20_min")


SpO2_0_min <- Univariate_stat(DATA, "SpO2_0_min")
SpO2_5_min <- Univariate_stat(DATA, "SpO2_5_min")
SpO2_10_min <- Univariate_stat(DATA, "SpO2_10_min")
SpO2_15_min <- Univariate_stat(DATA, "SpO2_15_min")
SpO2_20_min <- Univariate_stat(DATA, "SpO2_20_min")


##By patient that receives Noradrenaline or Akrinor performed a univariate analysis. 

DATA <- subset(DATA, DATA$Noradrenalin_20_min>0)
Noradrenalin_20_min <- Univariate_stat(DATA, "Noradrenalin_20_min")

DATA <- subset(DATA, DATA$Noradrenalin_15_min>0)
Noradrenalin_15_min <- Univariate_stat(DATA, "Noradrenalin_15_min")

DATA <- subset(DATA, DATA$Noradrenalin_10_min>0)
Noradrenalin_10_min <- Univariate_stat(DATA, "Noradrenalin_10_min")

DATA <- subset(DATA, DATA$Noradrenalin_5_min>0)
Noradrenalin_5_min <- Univariate_stat(DATA, "Noradrenalin_5_min")

DATA <- subset(DATA, DATA$Akrinor_20_min>0)
Akrinor_20_min <- Univariate_stat(DATA, "Akrinor_20_min")

DATA <- subset(DATA, DATA$Akrinor_15_min>0)
Akrinor_15_min <- Univariate_stat(DATA, "Akrinor_15_min")

DATA <- subset(DATA, DATA$Akrinor_10_min>0)
Akrinor_10_min <- Univariate_stat(DATA, "Akrinor_10_min")

DATA <- subset(DATA, DATA$Akrinor_5_min>0)
Akrinor_5_min <- Univariate_stat(DATA, "Akrinor_5_min")


#####################################################################################
#Function for Figure 2, including the univariate deep learning probability and parameter values for Preoperative heart rate (HR), preoperative SpO2 in room air, systolic (SBP), diastolic (DBP) blood pressure, and cumulative noradrenaline at 5 minutes, and cumulative Akrinor at 15 minutes after induction of general anesthesia. The Youden index and the corresponding parameters determined optimal probability cut-off points.

PLOT <- function(DATA, n, unit) {
  h2o.init()
  DATA <- as.h2o(DATA)
  M_LEMA_RF <- h2o.deeplearning(x = n, y = 40, training_frame = DATA)
  pred_M_LEMA_RF<- as.data.frame(h2o.predict(M_LEMA_RF, DATA))
  data_uni<- data.frame(as.data.frame(Parameter <- DATA[, n]) , as.data.frame(ICU.Admission <- DATA[, "ICU.Admission"]), RISK=pred_M_LEMA_RF$p1)
  
  
  roc <- roc(data_uni$ICU.Admission,data_uni$RISK, main="Confidence intervals", percent=FALSE,
             ci=TRUE, # compute AUC (of AUC by default)
             print.auc=FALSE) # print the AUC (will contain the CI))
  
  
  
  Best_youden_roc <- coords(roc, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy",
                                               "tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity",
                                               "1-sensitivity", "1-accuracy", "1-npv", "1-ppv",
                                               "precision", "recall"),best.method="youden" , transpose = TRUE)
  
  Threshold_youden_roc <- Best_youden_roc[1]
  Data_DL_threshold  <- subset(data_uni[,1], data_uni[,3]<=Threshold_youden_roc)
  
  
  par(mar = c(4, 4, 4, 4))
  
  alpha = 150 # Transparent points
  palette(c(rgb(96, 171, 246, alpha = alpha, maxColorValue = 255),
            rgb(246, 96, 106, alpha = alpha, maxColorValue = 255)))
  
  par(new=TRUE)
  plot(data_uni[,1],  data_uni[,3], bg = data_uni[,2], pch = 21, cex = 1.2, axes=FALSE, xlim=range(data_uni[,1]), ylim=c(0,1), xlab="", ylab="", main=" ")
  axis(2, ylim=c(0,1),col="black",las=0.5)  ## las=1 makes horizontal labels
  axis(1, data_uni[,1])
  axis(4, ylim=c(0, 1, by = 0.1),col="black", las=0.5)
  
  
  lines(range(data_uni[,1]), c(Threshold_youden_roc, Threshold_youden_roc), lwd = 1, col = "gray31")
  
  lines( c(max(Data_DL_threshold), max(Data_DL_threshold)), c(-0.5,Threshold_youden_roc), lwd = 0.5, col = "gray31")
  
  mtext(side = 1, line = 2, paste(unit), lwd = 0.3,  col="gray31")
  mtext(side = 2, line = 2, 'DL probability', lwd = 0.3,  col="gray31")
  mtext(side = 3, line = 2, 'Sensitivity/Specificity', lwd = 0.3,  col="gray31")
  mtext(side = 4, line = 2, paste("Probability cut-off", round(Threshold_youden_roc,2)), lwd = 0.3,  col="gray31")
  
  
  box()
  
  par(new=TRUE)
  plot(roc$specificities, roc$thresholds, type="l", 
       col="firebrick", xlab=" ", ylab=" ", ann=FALSE, axes=FALSE, ylim = c(0,1) ,  xlim = c(0,1)) 
  
  lines(roc$sensitivities, roc$thresholds, ylim = c(0,1) , xlim = c(0,1), type="l", col = "dodgerblue3")
  axis(3, ylim=c(0,1, by = 1,0),col="black",las=0.5)
  
  
}

SBP_5_min <- PLOT(DATA, "SBP_5_min", "Systolic Blood Pressure in 5 Minutes (mmHg)")
DBP_5_min <- PLOT(DATA, "DBP_5_min", "Diastolic Blood Pressure in 5 Minutes (mmHg)")
HR_0_min <- PLOT(DATA, "HR_0_min", "Preoperative Heart Rate (bpm)")
SpO2_0_min <- PLOT(DATA, "SpO2_0_min", "Preoperative SpO2 in Room Air (%)")

DATA <- subset(DATA, DATA$Noradrenalin_5_min>0)
Noradrenalin_5_min <- PLOT(DATA, "Noradrenalin_5_min", "Cumulative Noradrenaline in 5 Minutes (mcg/kg)")

DATA <- subset(DATA, DATA$Akrinor_15_min>0)
Akrinor_15_min <- PLOT(DATA, "Akrinor_15_min", "Cumulative Akrinor in 15 Minutes (ml/kg)")

######################################################################################################################

#Code for a descriptive analysis of patient characteristics and hemodynamic values between the ICU-admission and non-ICU admission groups as a binary response.  Tables 1, 2, and 3.


Patients_data_ASAII <- subset(Patients_data, Patients_data$ASA==2)
Patients_data_ASAIII <- subset(Patients_data, Patients_data$ASA==3)

explanatory=c("Gender","Age","Height", "Weight", "Propofol", "Sufentanil")
dependent = "ICU.Admission"

Patients_ASAII_60 <- subset(Patients_data_ASAII, Patients_data_ASAII$Age<66)
Patients_ASAII_60 %>% summary_factorlist(dependent, explanatory, p=TRUE, add_dependent_label=TRUE,  p_cont_para = "t.test") 

Patients_ASAII_70 <- subset(Patients_data_ASAII, Patients_data_ASAII$Age>65)
Patients_ASAII_70 %>% summary_factorlist(dependent, explanatory, p=TRUE, add_dependent_label=TRUE,  p_cont_para = "t.test") 


Patients_ASAIII_60 <- subset(Patients_data_ASAIII, Patients_data_ASAIII$Age <66)
Patients_ASAIII_60 %>% summary_factorlist(dependent, explanatory, p=TRUE, add_dependent_label=TRUE,  p_cont_para = "t.test") 

Patients_ASAIII_70 <- subset(Patients_data_ASAIII, Patients_data_ASAIII$Age>65 & Patients_data_ASAIII$Age<76)
Patients_ASAIII_70 %>% summary_factorlist(dependent, explanatory, p=TRUE, add_dependent_label=TRUE,  p_cont_para = "t.test") 

Patients_ASAIII_80 <- subset(Patients_data_ASAIII, Patients_data_ASAIII$Age>75 & Patients_data_ASAIII$Age<86)
Patients_ASAIII_80 %>% summary_factorlist(dependent, explanatory, p=TRUE, add_dependent_label=TRUE,  p_cont_para = "t.test") 

Patients_ASAIII_90 <- subset(Patients_data_ASAIII, Patients_data_ASAIII$Age>85)
Patients_ASAIII_90 %>% summary_factorlist(dependent, explanatory, p=TRUE, add_dependent_label=TRUE,  p_cont_para = "t.test") 


#----------------------------------------------------------------------------------------------------------------------------------


explanatory=c("Sevo_0_min", "Sevo_5_min","Sevo_10_min", "Sevo_15_min", "Sevo_20_min", "HR_0_min", "HR_5_min","HR_10_min", "HR_15_min", "HR_20_min", "SBP_0_min", "SBP_5_min","SBP_10_min", "SBP_15_min", "SBP_20_min",
              "DBP_0_min", "DBP_5_min","DBP_10_min", "DBP_15_min", "DBP_20_min", "SpO2_0_min", "SpO2_5_min","SpO2_10_min", "SpO2_15_min", "SpO2_20_min", 
              "Noradrenalin_0_min", "Noradrenalin_5_min", "Noradrenalin_10_min", "Noradrenalin_15_min", "Noradrenalin_20_min", "Akrinor_0_min","Akrinor_5_min", "Akrinor_10_min", "Akrinor_15_min", "Akrinor_20_min" )
dependent = "ICU.Admission"

Patients_data %>% summary_factorlist(dependent, explanatory, p=TRUE, add_dependent_label=TRUE,  p_cont_para = "t.test") 


########################################################################################################################################

