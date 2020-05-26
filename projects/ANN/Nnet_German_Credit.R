rm(list=ls(all=TRUE))
setwd("/home/dev/work/Insofe/20140208-ANN/rcode_Neural-Networks")

library(neuralnet)
library(dummies)
library(vegan)

source("./LoadGermanCreditData.R")

#Bank problem
table(gc$RESPONSE)
table(gc.train$RESPONSE)
table(gc.test$RESPONSE)

nn=neuralnet(RESPONSE~ 
               DURATION+NEW_CAR+USED_CAR+FURNITURE+RADIO_TV+
               EDUCATION+RETRAINING+AMOUNT+INSTALL_RATE+CO_APPLICANT+
               GUARANTOR+REAL_ESTATE+PROP_UNKN_NONE+AGE+OTHER_INSTALL+
               RENT+OWN_RES+NUM_CREDITS+NUM_DEPENDENTS+TELEPHONE+
               FOREIGN+CHK_ACCT+HISTORY+SAV_ACCT+EMPLOYMENT+
               PRESENT_RESIDENT+JOB+MALE_DIV+MALE_MAR_or_WID+MALE_SINGLE, 
             data=gc.train,
             hidden=c(2,2))

out <- cbind(nn$covariate,
             nn$net.result[[1]])

head(out) # to view top records in the data set
plot.new()
plot(nn)

#Data preparation for classification matrix
p=as.data.frame(nn$net.result)
colnames(p)="pred"
pred_class <- factor(ifelse(p$pred > 0.5, 1, 0))
a <- table(pred_class, 
           gc.train$RESPONSE)
recall <- a[2,2]/(a[2,1]+a[2,2])*100
recall

#retrieving required columns from the data
test_data2=subset(gc.test,
                  select=-c(OBS,RESPONSE))
new.output <- compute(nn,
                      covariate=test_data2)
p=as.data.frame(new.output$net.result)
colnames(p)="pred"
pred_class <- factor(ifelse(p$pred > 0.5, 1, 0))
a <- table(pred_class,gc.test$RESPONSE)
recall <- a[2,2]/(a[2,1]+a[2,2])*100
recall

#Play with different node structures (3), (2,2), (4,3)

