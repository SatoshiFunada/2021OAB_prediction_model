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

#Exclude missing data
data_cc <- data %>% 
  drop_na()

#define useful quantities
bootsam=list()
apparent.AUC1=c()
apparent.AUC2=c()
apparent.intercept.1=c()
apparent.intercept.2=c()
apparent.slope.1=c()
apparent.slope.2=c()

orig.AUC1=c()
orig.AUC2=c()
orig.slope.1=c()
orig.slope.2=c()
orig.intercept.1=c()
orig.intercept.2=c()

optimism.AUC.1=c()
optimism.AUC.2=c()
optimism.intercept.1=c()
optimism.intercept.2=c()
optimism.slope.1=c()
optimism.slope.2=c()

Nboot=200

#bootstrap
set.seed(42)
for(i in 1:Nboot) { 
  sam=sample(nrow(data_cc), replace=TRUE)
  bootsam[[i]]=data_cc[sam, ]
}
head(bootsam[[1]])

#set up original data
LR1.orig<-lrm(OAB_2~age+bmi+smoke+alcohol+walking+Delivery+menopause
              +HT+HL+DM+STROKE+MI+CANCER+SAS+Depression+Insomnia+Renal_disease
              +OABSS1_1+OABSS2_1+OABSS3_1+OABSS4_1,
              data = data_cc,
              x=T, y=T,linear.predictors = T)

LR2.orig<-lrm(OAB_2~age+bmi+smoke+alcohol+walking+Delivery+menopause
              +HT+HL+DM+STROKE+MI+CANCER+SAS+Depression+Insomnia+Renal_disease
              +OABSS1_1+OABSS2_1+OABSS3_1+OABSS4_1
              +HbA1c+eGFR+BNP,
              data =data_cc,
              x=T, y=T,linear.predictors = T)

#fit models in original sample
set.seed(42)
cv1 <- cv.glmnet(x=LR1.orig$x, y=LR1.orig$y, alpha = 1, family=c("binomial")) 
LR1 <- glmnet(x=LR1.orig$x, y=LR1.orig$y, alpha = 1, lambda = cv1$lambda.min, family=c("binomial")) 

set.seed(42)
cv2 <- cv.glmnet(x=LR2.orig$x, y=LR2.orig$y, alpha = 1, family=c("binomial")) 
LR2 <- glmnet(x=LR2.orig$x, y=LR2.orig$y, alpha = 1, lambda = cv2$lambda.min, family=c("binomial")) 

#calculate apparent performance
p1=data.frame(predict(LR1, newx =LR1.orig$x , type = "response"))
p2=data.frame(predict(LR2, newx =LR2.orig$x , type = "response"))
roc1=roc(LR1.orig$y ~ p1$s0)
AUC1=roc1$auc
roc2=roc(LR2.orig$y ~ p2$s0)
AUC2=roc2$auc

calib1=val.prob(p1$s0, data_cc$OAB_2, m=100, cex=.5)[c("Intercept","Slope")]
calib2=val.prob(p2$s0, data_cc$OAB_2, m=100, cex=.5)[c("Intercept","Slope")]

# BOOTSTRAP

for(i in 1:Nboot) { 
  #set up data in bootstrap sample
  LR1.boot<-lrm(OAB_2~age+bmi+smoke+alcohol+walking+Delivery+menopause
                +HT+HL+DM+STROKE+MI+CANCER+SAS+Depression+Insomnia+Renal_disease
                +OABSS1_1+OABSS2_1+OABSS3_1+OABSS4_1,
                data = bootsam[[i]],
                x=T, y=T,linear.predictors = T)
  
  LR2.boot<-lrm(OAB_2~age+bmi+smoke+alcohol+walking+Delivery+menopause
                +HT+HL+DM+STROKE+MI+CANCER+SAS+Depression+Insomnia+Renal_disease
                +OABSS1_1+OABSS2_1+OABSS3_1+OABSS4_1
                +HbA1c+eGFR+BNP,
                data = bootsam[[i]],
                x=T, y=T,linear.predictors = T)
  
  #fit LASSO in bootstrap sample i
  cv.lasso1 <- cv.glmnet(x=LR1.boot$x, y=LR1.boot$y, alpha = 1, family=c("binomial")) 
  cv.lasso2 <- cv.glmnet(x=LR2.boot$x, y=LR2.boot$y, alpha = 1, family=c("binomial")) 
  
  LR.L1 <- glmnet(x=LR1.boot$x, y=LR1.boot$y, alpha = 1, lambda = cv.lasso1$lambda.min, family=c("binomial")) 
  LR.L2 <- glmnet(x=LR2.boot$x, y=LR2.boot$y, alpha = 1, lambda = cv.lasso2$lambda.min, family=c("binomial")) 
  
  #caclulate performance in bootstrap sample i
  p.LASSO1=data.frame(predict(LR.L1, newx =LR1.boot$x , type = "response"))
  p.LASSO2=data.frame(predict(LR.L2, newx =LR2.boot$x , type = "response"))
  rocLR.L1=roc(LR1.boot$y ~ p.LASSO1$s0)
  apparent.AUC1[i]=rocLR.L1$auc
  rocLR.L2=roc(LR2.boot$y ~ p.LASSO2$s0)
  apparent.AUC2[i]=rocLR.L2$auc
  
  apparent.intercept.1[i]=val.prob(p.LASSO1$s0, LR1.boot$y, m=100, cex=.5)[c("Intercept","Slope")][1]
  apparent.slope.1[i]=val.prob(p.LASSO1$s0, LR1.boot$y, m=100, cex=.5)[c("Intercept","Slope")][2]
  
  apparent.intercept.2[i]=val.prob(p.LASSO2$s0, LR2.boot$y, m=100, cex=.5)[c("Intercept","Slope")][1]
  apparent.slope.2[i]=val.prob(p.LASSO2$s0, LR2.boot$y, m=100, cex=.5)[c("Intercept","Slope")][2]
  
  #caclulate performance in original sample 
  p.LASSO1.orig=data.frame(predict(LR.L1, newx =LR1.orig$x , type = "response"))
  p.LASSO2.orig=data.frame(predict(LR.L2, newx =LR2.orig$x , type = "response"))
  rocLR.L1.orig=roc(LR1.orig$y ~ p.LASSO1.orig$s0)
  orig.AUC1[i]=rocLR.L1.orig$auc
  rocLR.L2.orig=roc(LR2.orig$y ~ p.LASSO1.orig$s0)
  orig.AUC2[i]=rocLR.L2.orig$auc
  orig.intercept.1[i]=val.prob(p.LASSO1.orig$s0, LR1.orig$y, m=100, cex=.5)[c("Intercept","Slope")][1]
  orig.slope.1[i]=val.prob(p.LASSO1.orig$s0, LR1.orig$y, m=100, cex=.5)[c("Intercept","Slope")][2]
  
  orig.intercept.2[i]=val.prob(p.LASSO2.orig$s0, LR2.orig$y, m=100, cex=.5)[c("Intercept","Slope")][1]
  orig.slope.2[i]=val.prob(p.LASSO2.orig$s0, LR2.orig$y, m=100, cex=.5)[c("Intercept","Slope")][2]
  
  print(i)
}

#calculate optimism
optimism.AUC.1=mean(apparent.AUC1-orig.AUC1)
optimism.AUC.2=mean(apparent.AUC2-orig.AUC2)
optimism.intercept.1=mean(-abs(apparent.intercept.1)+abs(orig.intercept.1))
optimism.slope.1=mean(-abs(1-apparent.slope.1)+abs(1-orig.slope.1))
optimism.intercept.2=mean(-abs(apparent.intercept.2)+abs(orig.intercept.2))
optimism.slope.2=mean(-abs(1-apparent.slope.2)+abs(1-orig.slope.2))


optimism.corrected.AUC1=AUC1-optimism.AUC.1
optimism.corrected.AUC2=AUC2-optimism.AUC.2
optimism.corrected.AUC1
optimism.corrected.AUC2

optimism.corrected.calib1=calib1+c(optimism.intercept.1, optimism.slope.1)
optimism.corrected.calib2=calib2+c(optimism.intercept.2, optimism.slope.2)
optimism.corrected.calib1
optimism.corrected.calib2
