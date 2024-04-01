library(lava)
library(nnet)
library(data.table)
library(tidyverse)
library(neuralnet)
library(corrplot)
library(pROC)
library(foreach)
library(tableone)

#https://cran.r-project.org/web/packages/nnet/nnet.pdf
## use simuldated dataset for now
coefs <-  data.table::fread("../../cvd_simulation/data/coefficients.txt")

source("../../cvd_simulation/functions/synthesizeDD.R")
source("../../cvd_simulation/functions/sim_sig_data.R")
data_specs <- synthesizeDD(coefs)
N_time=4
dt <- data.table(sim_sig_data(data_specs,
                              n=5000,
                              N_time=N_time,
                              iter = 1)[[1]])

set.seed(111)
train_index <- as.logical(rbinom(n=nrow(dt),size=1,prob=0.1))
train <- dt[train_index==FALSE]
test <- dt[train_index==TRUE]

#simdata <- dt[,.(age_base,sex,ie_type,code5txt,quartile_income)]
simdata <- dt[sample(nrow(dt),nrow(train),replace=F),.(age_base,sex,ie_type,code5txt,quartile_income)]
preds <- matrix(nrow=nrow(test),ncol=length(dt)-5)
nnet_models <- as.list(names(dt)[6:length(names(dt))])
for(out in 1:(length(names(dt))-
              5)){
  (outcome <- names(dt)[out+5])
  (predictors <- names(train)[1:(grep(outcome,names(train))-1)])
  (thisformula <- as.formula(paste0(outcome,"~",paste0(predictors,collapse="+"))))

  nnet_models[[out]] <- neuralnet(thisformula, train,
                        hidden = 1,
                        stepmax=1e7,
                        act.fct = 'logistic',
                        algorithm = "rprop+")
   cutoffs <- seq(0.05,0.95,by=0.05)
   aucs <- matrix(nrow=length(cutoffs))
   foreach(j=1:length(cutoffs))%do%{
     pred_j <- as.numeric(predict(nnet_models[[out]],
                                             newdata=simdata,type='response')>cutoffs[j])
     roc_j <- roc(response=train[[outcome]],pred_j)
     aucs[j] <- auc(roc_j)
   }
   ##select the one that minimizes auc
   simdata[,outcome] <- as.numeric(predict(nnet_models[[out]],
                                           newdata=simdata,type='response')>cutoffs[min(grep(min(aucs),aucs))])

  simdata[,outcome] <- predict(nnet_models[[out]],
                                         newdata=simdata,type='response')

  # Test also with glm
  # glm_model <- glm(thisformula, family='binomial',data=train)
  # preds[,out] <- predict(glm_model, newdata=test,type="response")
  # simdata[,outcome] <- as.numeric(predict(glm_model,newdata=simdata,type='response')>mean(dt[[outcome]]))
}

corrplot(cor(as.matrix(dt)), method = "color")
corrplot(cor(as.matrix(simdata)), method = "color")
#corrplot(cor(as.matrix(preds)), method = "color")


CreateCatTable(data=dt,vars = names(dt)[6:20])
CreateCatTable(data=simdata,vars = names(simdata)[6:20])


# nnet_model <-nnet(mace_hf_4~glp1_1+insulin_1,
#                   x=as.matrix(train[,.(glp1_1, insulin_1)]),
#                   #x=as.matrix(train[,-c(outcome),with=F]),
#                   # y=as.matrix(train[,c(outcome),with=F]),size=1)
#                   y=as.matrix(train$mace_hf_4),size=1,MaxNWts = 10000)


