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
library(caret)

data <- read_csv("data/data_female.csv")
data_cc <- data %>% 
  drop_na()

##################
#set up original data
LR1<-lrm(OAB_2~age+bmi+Delivery+menopause+smoke+alcohol+walking
         +HT+HL+DM+MI+STROKE+Renal_disease+CANCER+Depression+Insomnia+SAS
         +OABSS1_1+OABSS2_1+OABSS3_1+OABSS4_1,
         data = data_cc,
         x=T, y=T,linear.predictors = T)

LR2<-lrm(OAB_2~age+bmi+Delivery+menopause+smoke+alcohol+walking
         +HT+HL+DM+MI+STROKE+Renal_disease+CANCER+Depression+Insomnia+SAS
         +OABSS1_1+OABSS2_1+OABSS3_1+OABSS4_1
         +HbA1c+eGFR+BNP,
         data = data_cc,
         x=T, y=T,linear.predictors = T)

##model1
#fit models in original sample
set.seed(42)
cv.lasso1 <- cv.glmnet(x=LR1$x, y=LR1$y, alpha = 1, family=c("binomial")) 
LR.L1 <- glmnet(x=LR1$x, y=LR1$y, alpha = 1, lambda = cv.lasso1$lambda.min, family=c("binomial")) 
coef(LR.L1) 

df <- data.frame(as.matrix(coef(LR.L1)))
write.csv(df, file = "data/LR.L1_female.csv")

##model2
#fit models in original sample
set.seed(42)
cv.lasso2 <- cv.glmnet(x=LR2$x, y=LR2$y, alpha = 1, family=c("binomial")) 
LR.L2 <- glmnet(x=LR2$x, y=LR2$y, alpha = 1, lambda = cv.lasso2$lambda.min, family=c("binomial")) 
coef(LR.L2) 

df <- data.frame(as.matrix(coef(LR.L2)))
write.csv(df, file = "data/LR.L2_female.csv")

#predict probability of an event given covariates 
p.LASSO1=data.frame(predict(LR.L1, newx =LR1$x , type = "response"))
p.LASSO2=data.frame(predict(LR.L2, newx =LR2$x , type = "response"))

##model performance
#Descrimination
rocLR.L1=roc(LR1$y ~ p.LASSO1$s0)
auc(rocLR.L1)
ci(roc(LR1$y ~ p.LASSO1$s0))
plot(rocLR.L1, legacy.axes = TRUE, asp=NA)
rocLR.L2=roc(LR2$y ~ p.LASSO2$s0)
auc(rocLR.L2)
ci(roc(LR2$y ~ p.LASSO2$s0))
plot(rocLR.L2, legacy.axes = TRUE, asp=NA)

#Calibration
val.prob(p.LASSO1$s0, data_cc$OAB_2, m=400, cex=1)[c("Intercept","Slope")]
val.prob(p.LASSO2$s0, data_cc$OAB_2, m=400, cex=1)[c("Intercept","Slope")]


#decision curve analysis
#Source file to use dca command
DCA_data <- as.data.frame(data_cc)
source("dca.R")
DCA_data$Model1 = predict(LR.L1, newx =LR1$x , type = "response")
a=dca(data=DCA_data, outcome="OAB_2", predictors="Model1")
DCA_data$Model2 = predict(LR.L2, newx =LR2$x , type = "response")

#Run decision curve
b=dca(data=DCA_data, outcome="OAB_2", predictors=c("Model1","Model2")) 
