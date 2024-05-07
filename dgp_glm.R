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

R=1000 #number of dataset repetitions
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
                                             newdata=simdata,type='response')>cutoffs[min(grep(min(mses),mses))])

    #simdata[,outcome] <- as.numeric(predict(glm_model,newdata=simdata,type='response')>mean(dt[[outcome]]))
    #preds[[outcome]] <- predict(glm_model,newdata=simdata,type='response')

  }
  return(simdata)

}



##correlation plot
corrplot(cor(as.matrix(dt)), method = "color")
for(i in 1:3){
corrplot(cor(as.matrix(simdata_list[[i]])), method = "color")
}

CreateCatTable(data=dt,vars = names(dt)[6:20])
CreateCatTable(data=simdata,vars = names(simdata)[6:20])
CreateContTable(data=preds,vars = names(simdata)[6:20])


#transport::wasserstein(dt$age_base, simdata_list[[1]]$age_base, p=1, tplan=NULL, costm=NULL, prob=TRUE)
#waddR::



## find truth
outcome <- "mace_hf_4"
(predictors <- names(train)[!names(train)%in%outcome])
(thisformula <- as.formula(paste0(outcome,"~",paste0(predictors,collapse="+"))))
glm_model <- glm(thisformula, family='binomial',data=dt)
dt_a1 <- data.table(copy(dt))
dt_a0 <- data.table(copy(dt))
dt_a1[, names(dt)[(grepl("glp",names(dt)))] := .(1)]
dt_a0[, names(dt)[(grepl("glp",names(dt)))] := .(0)]
mean(predict(glm_model,newdata=dt_a1,type='response'))-mean(predict(glm_model,newdata=dt_a0,type='response'))

-1/(1+exp(-sum(glm_model$coefficients[names(glm_model$coefficients)[(grepl("glp",names(glm_model$coefficients)))]])))




saveRDS(simdata_list,file="./tmp/simdata_list_1000_glm.RDS")
