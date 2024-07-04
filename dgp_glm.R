library(lava)
library(nnet)
library(data.table)
library(tidyverse)
library(neuralnet)
library(corrplot)
library(pROC)
library(foreach)
library(tableone)
dt <-data.table(read.csv("./data/fakedataset.csv"))
dt <-dt[,-c("X")]
N_time <- max(as.numeric(sapply(names(dt),function(x){substr(x,nchar(x),nchar(x))})),na.rm=TRUE)

R=500#0 #number of dataset repetitions
set.seed(200)

simdata_list <-foreach(j=1:R)%do%{
  print(paste0("begin iteration: ",j))
  train_index <- as.logical(rbinom(n=nrow(dt),size=1,prob=0.1))
  train <- dt[train_index==FALSE]
  test <- dt[train_index==TRUE]

  #simdata <- dt[,.(age_base,sex,ie_type,code5txt,quartile_income)]
  simdata <- dt[sample(nrow(dt),nrow(train),replace=F),.(age_base,sex,ie_type,code5txt,quartile_income)]
  preds <- data.frame(matrix(nrow=nrow(simdata),ncol=length(dt)-5))
  models <- as.list(names(dt)[6:length(names(dt))])
  cutoffs <- seq(0.05,0.95,by=0.05)
  mses <- matrix(nrow=length(cutoffs))
  ##ignores censoring

  for(out in 1:(length(names(dt))-5)){
    (outcome <- names(dt)[out+5])
    (predictors <- names(train)[1:(grep(outcome,names(train))-1)])
    (thisformula <- as.formula(paste0(outcome,"~",paste0(predictors,collapse="+"))))
    glm_model <- glm(thisformula, family='binomial',data=train)

     mses <- matrix(nrow=length(cutoffs))

          foreach(j=1:length(cutoffs))%do%{
            pred_j <- as.numeric(predict(glm_model,newdata=simdata,type='response')>cutoffs[j])
            mses[j] <- (sum(train[[outcome]]-pred_j)^2)/nrow(dt)
            }
    # ##select the first one that minimizes auc
     simdata[,outcome] <- as.numeric(predict(glm_model,
                                             newdata=simdata,type='response') +rnorm(nrow(simdata),0,0.1)>cutoffs[min(grep(min(mses),mses))])

  }
  return(simdata)

}

saveRDS(simdata_list,file=paste0("./tmp/simdata_list_",as.character(R),"_t",as.character(N_time),"_glm.RDS"))

