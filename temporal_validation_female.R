library(tidyverse)
library(Hmisc)
library(foreign)
library(ROCR)
library(pROC)
library(rms)
library(haven)
library(mice)
library(foreign)
library(dplyr)
library(ggplot2)
library(GGally)
library(glmnet)
library(rsq)

data <- read_csv("data/data_female.csv")
data_cc <- data %>% 
  drop_na()

#Temporal validation
data_20082009 <- data_cc %>% 
  filter(SEIREKI!="2010")
data_2010 <- data_cc %>% 
  filter(SEIREKI=="2010") 

LR1_training<-lrm(OAB_2~age+bmi+Delivery+menopause+smoke+alcohol+walking
                  +HT+HL+DM+MI+STROKE+Renal_disease+CANCER+Depression+Insomnia+SAS
                  +OABSS1_1+OABSS2_1+OABSS3_1+OABSS4_1,
                  data = data_20082009,
                  x=T, y=T,linear.predictors = T)
LR1_testing<-lrm(OAB_2~age+bmi+Delivery+menopause+smoke+alcohol+walking
                 +HT+HL+DM+MI+STROKE+Renal_disease+CANCER+Depression+Insomnia+SAS
                 +OABSS1_1+OABSS2_1+OABSS3_1+OABSS4_1,
                 data = data_2010,
                 x=T, y=T,linear.predictors = T)

LR2_training<-lrm(OAB_2~age+bmi+Delivery+menopause+smoke+alcohol+walking
                  +HT+HL+DM+MI+STROKE+Renal_disease+CANCER+Depression+Insomnia+SAS
                  +OABSS1_1+OABSS2_1+OABSS3_1+OABSS4_1
                  +HbA1c+eGFR+BNP,
                  data = data_20082009,
                  x=T, y=T,linear.predictors = T)
LR2_testing<-lrm(OAB_2~age+bmi+Delivery+menopause+smoke+alcohol+walking
                 +HT+HL+DM+MI+STROKE+Renal_disease+CANCER+Depression+Insomnia+SAS
                 +OABSS1_1+OABSS2_1+OABSS3_1+OABSS4_1
                 +HbA1c+eGFR+BNP,
                 data = data_2010,
                 x=T, y=T,linear.predictors = T)

#LASSO_training_model1
set.seed(42)
cv.lasso1_training <- cv.glmnet(x=LR1_training$x, y=LR1_training$y, alpha = 1, family=c("binomial")) 
#Fit the model on the training data 
LR.L1_training <- glmnet(x=LR1_training$x, 
                         y=LR1_training$y,
                         alpha = 1, 
                         lambda = cv.lasso1_training$lambda.min, 
                         family=c("binomial")) 
#Fit the model on the test data 
p.LASSO1_temporal_testing=data.frame(predict(LR.L1_training, newx =LR1_testing$x , type = "response"))

#Discrimination
rocLR.L1_testing=roc(LR1_testing$y ~ p.LASSO1_temporal_testing$s0)
auc(rocLR.L1_testing)
ci(roc(LR1_testing$y ~ p.LASSO1_temporal_testing$s0))
plot(rocLR.L1_testing, legacy.axes = TRUE, asp=NA)

#Calibration
val.prob(p.LASSO1_temporal_testing$s0, data_2010$OAB_2, m=100, cex=1)[c("Intercept","Slope")]


#LASSO_training_model2
set.seed(42)
cv.lasso2_training <- cv.glmnet(x=LR2_training$x, y=LR2_training$y, alpha = 1, family=c("binomial")) 
#Fit the model on the training data 
LR.L2_training <- glmnet(x=LR2_training$x, 
                         y=LR2_training$y,
                         alpha = 1, 
                         lambda = cv.lasso2_training$lambda.min, 
                         family=c("binomial")) 
#Fit the model on the test data 
p.LASSO2_temporal_testing=data.frame(predict(LR.L2_training, newx =LR2_testing$x , type = "response"))

#Discrimination
rocLR.L2_testing=roc(LR2_testing$y ~ p.LASSO2_temporal_testing$s0)
auc(rocLR.L2_testing)
ci(roc(LR2_testing$y ~ p.LASSO2_temporal_testing$s0))
plot(rocLR.L2_testing, legacy.axes = TRUE, asp=NA)

#Calibration
val.prob(p.LASSO2_temporal_testing$s0, data_2010$OAB_2, m=100, cex=1)[c("Intercept","Slope")]

