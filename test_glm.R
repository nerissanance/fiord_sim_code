library(lava)
library(nnet)
library(data.table)
library(tidyverse)
library(neuralnet)
library(corrplot)
library(pROC)
library(foreach)
library(tableone)

dt <-read.csv("./data/fakedataset.csv")

train_index <- as.logical(rbinom(n=nrow(dt),size=1,prob=0.1))
train <- dt[train_index==FALSE]
test <- dt[train_index==TRUE]

#simdata <- dt[,.(age_base,sex,ie_type,code5txt,quartile_income)]
simdata <- dt[sample(nrow(dt),nrow(train),replace=F),.(age_base,sex,ie_type,code5txt,quartile_income)]
preds <- matrix(nrow=nrow(test),ncol=length(dt)-5)
models <- as.list(names(dt)[6:length(names(dt))])

for(out in 1:(length(names(dt))-5)){
  (outcome <- names(dt)[out+5])
  (predictors <- names(train)[1:(grep(outcome,names(train))-1)])
  (thisformula <- as.formula(paste0(outcome,"~",paste0(predictors,collapse="+"))))
  glm_model <- glm(thisformula, family='binomial',data=train)
  simdata[,outcome] <- as.numeric(predict(glm_model,newdata=simdata,type='response')>mean(dt[[outcome]]))
}

corrplot(cor(as.matrix(dt)), method = "color")
corrplot(cor(as.matrix(simdata)), method = "color")


CreateCatTable(data=dt,vars = names(dt)[6:20])
CreateCatTable(data=simdata,vars = names(simdata)[6:20])
