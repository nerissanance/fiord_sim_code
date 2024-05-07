library(lava)
library(nnet)
library(data.table)
library(tidyverse)
library(neuralnet)
library(corrplot)
library(pROC)
library(foreach)
library(tableone)
library(glmnet)
library(doParallel)
library(doRNG)

#(cl <- makeCluster(detectCores()))
#registerDoParallel(cl)


dt <-data.table(read.csv("./data/fakedataset.csv"))
dt <-dt[,-c("X")]

R=1000 #number of dataset repetitions
#registerDoRNG(seed = 123)
set.seed(200)

simdata_list <-foreach(j=1:R)%do%{ #%dopar%

  print(paste0("begin iteration: ",j))
  train_index <- as.logical(rbinom(n=nrow(dt),size=1,prob=0.1))
  train <- (dt[train_index==FALSE])
  test <- (dt[train_index==TRUE])

  simdata <- dt[sample(nrow(dt),nrow(train),replace=F),.(age_base,sex,ie_type,code5txt,quartile_income)]
  preds <- data.frame(matrix(nrow=nrow(simdata),ncol=length(dt)-5))
  models <- as.list(names(dt)[6:length(names(dt))])
  cutoffs <- seq(0.05,0.95,by=0.05)
  mses <- matrix(nrow=length(cutoffs))
  ##ignores censoring

  for(out in 1:(length(names(dt))-5)){
    (outcome <- names(dt)[out+5])
    (predictors <- names(train)[1:(grep(outcome,names(train))-1)])

    #create model matrix which includes all pairwise interaction terms
    x <- model.matrix(as.formula( ~ .^2),train[,predictors,with=F])

    glmnet_model <- cv.glmnet(x=x,y=as.matrix(train[,outcome,with=F]), alpha=0, family='binomial')
    lambdamin <- glmnet_model$lambda.min
    mses <- matrix(nrow=length(cutoffs))

    foreach(k=1:length(cutoffs))%do%{
      pred_k <- as.numeric(predict(glmnet_model,newx=model.matrix(as.formula( ~ .^2),simdata,s = "lambda.min"),#"lambda.1se"
                                   type='response')>cutoffs[k])
      mses[k] <- (sum(train[[outcome]]-pred_k)^2)/nrow(dt)
    }
    # ##select the first cutoff that minimizes auc
    simdata[,outcome] <- as.numeric(predict(glmnet_model,
                                            newx=model.matrix(as.formula( ~ .^2), simdata),
                                            type='response')>cutoffs[min(grep(min(mses),mses))])

  }
  return(simdata)

}

#stopCluster(cl)
saveRDS(simdata_list,file="./tmp/simdata_list_1000_glmnet.RDS")


##correlation plot
corrplot(cor(as.matrix(dt)), method = "color")
for(i in 1:length(simdata_list)){
  corrplot(cor(as.matrix(simdata_list[[i]])), method = "color")
}

CreateCatTable(data=dt,vars = names(dt)[6:20])
CreateCatTable(data=simdata_list[[1]],vars = names(simdata)[6:20])
CreateContTable(data=preds,vars = names(simdata)[6:20])


#transport::wasserstein(dt$age_base, simdata_list[[1]]$age_base, p=1, tplan=NULL, costm=NULL, prob=TRUE)
#waddR::

## find truth
outcome <- "mace_hf_4"
(predictors <- names(train)[!names(dt)%in%outcome])
x <- model.matrix(as.formula( ~ .^2),dt[,predictors,with=F])
glmnet_model <- cv.glmnet(x=x,y=as.matrix(train[,outcome,with=F]), alpha=0, family='binomial')
dt_a1 <- data.table(copy(dt))
dt_a0 <- data.table(copy(dt))
dt_a1[, names(dt_a1)[(grepl("glp",names(dt_a1)))] := .(1)]
dt_a0[, names(dt_a0)[(grepl("glp",names(dt_a0)))] := .(0)]
(Y1 <- mean(predict(glmnet_model,newx=model.matrix(as.formula( ~ .^2), dt_a1[,predictors,with=F]),type='response')))
(Y0 <- mean(predict(glmnet_model,newx=model.matrix(as.formula( ~ .^2), dt_a0[,predictors,with=F]),type='response')))
(Y1-Y0)
