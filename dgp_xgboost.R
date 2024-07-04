library(lava)
library(nnet)
library(data.table)
library(tidyverse)
library(neuralnet)
library(corrplot)
library(pROC)
library(foreach)
library(tableone)
library(xgboost)
library(doParallel)
library(doRNG)

(cl <- makeCluster(detectCores()/2))
registerDoParallel(cl)


dt <-data.table(read.csv("./data/fakedataset.csv"))
dt <-dt[,-c("X")]

R=200 #number of dataset repetitions
registerDoRNG(seed = 123)
# rng <- RNGseq( length(simdata_list) * nrow(specs), seed=1234)
N_time <- max(as.numeric(sapply(names(dt),function(x){substr(x,nchar(x),nchar(x))})),na.rm=TRUE)
#set.seed(200)

simdata_list <-foreach(j=1:R)%dopar%{
  library(xgboost)
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
  
    #cross-validation settings
    cv_control =
      caret::trainControl(method = "repeatedcv",
                          # folds
                          number = 2L,
                          # Number of complete sets of folds to compute
                          repeats = 2L,
                          classProbs = TRUE)#,
                          #summaryFunction = twoClassSummary) 
    
    (xgb_grid = expand.grid(
      # Number of trees to fit, aka boosting iterations
      nrounds = c(100, 300, 500, 700, 900),
      # Depth of the trees
      max_depth = c(1, 6), 
      # Learning rate (smaller=slower) 
      eta = c(0.0001, 0.01, 0.2),
      gamma = 0,
      colsample_bytree = 1.0,
      subsample = 1.0,
      min_child_weight = 10L))
    
   
    model = caret::train(x=x , y = train[[outcome]], 
                         method = "xgbTree",
                         metric = "ROC",
                         trControl = cv_control,
                         tuneGrid = xgb_grid,
                         verbose = TRUE)
  #  xgboost_model <- xgboost(data = x, label=train[[outcome]], params=gbmGrid,objective = "binary:logistic", verbose = 2)
    min(xgb_testparams$evaluation_log$test_logloss_mean)
    mses <- matrix(nrow=length(cutoffs))

    foreach(k=1:length(cutoffs))%do%{
      pred_k <- as.numeric(predict(xgboost_model,newdata=model.matrix(as.formula( ~ .^2),simdata)
                                   )>cutoffs[k])
      mses[k] <- (sum(train[[outcome]]-pred_k)^2)/nrow(dt)
    }
    # ##select the first cutoff that minimizes auc
    simdata[,outcome] <- as.numeric(predict(xgboost_model,
                                            newdata=model.matrix(as.formula( ~ .^2), simdata),
                                            )+rnorm(nrow(simdata),0,0.1)>cutoffs[min(grep(min(mses),mses))])

  }
  return(simdata)

}

stopCluster(cl)
saveRDS(simdata_list,file=paste0("./tmp/simdata_list_",as.character(R),"_t",as.character(N_time),"_xgboost.RDS"))


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
