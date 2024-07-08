##Purpose: to compare between empirical influence curve and a nonparametric bootstrap for inference
##Runs:
#   -the final analytic strategy specs for n iterations to get empirical IC for R iterations
#   -a non-parametric bootstrap of n_boot iterations on each estimate of R
library(ltmle)
library(doParallel)
library(tidyverse)
library(SuperLearner)
library(data.table)
library(doRNG)

#import functions
sapply(paste0("./functions/",list.files(path="./functions/")),source,.GlobalEnv)

####SET PARAMS:
model <- "glm"
reps <- 200
N_time <- 4
n_boot <- 2#00 #bootstrap iterations
final_lib <- c("SL.mean","glm","SL.glm.interaction","SL.earth","SL.xgboost","SL.randomForest")
trunc_level <- c(0.001,1)
####END PARAMS

###load data
#N_time and reps identify the data--this needs to be the same data as what you used for the estimator comparison
dataname <-paste0("simdata_list_",as.character(reps),"_","t",as.character(N_time),"_",model)
simdata_list <- readRDS(paste0("./tmp/",dataname,".RDS"))

### register paralleization and set seed
(cl <-makeCluster(detectCores()-2))
registerDoParallel(cl)
registerDoRNG(seed = 123)


ci_df <- foreach(i = 1:length(simdata_list),.combine='rbind') %dopar% {
    library(ltmle)
    try(result1 <- run_ltmle(data=simdata_list[[i]],
                             SL.library=final_lib,
                             trunc_level=trunc_level,
                             varmethod="ic"))
    try(result2 <- run_ltmle(data=simdata_list[[i]],
                             SL.library=final_lib,
                             trunc_level=trunc_level,
                             varmethod="tmle"))

    try(sum1 <- summary(result1))
    try(sum2 <- summary(result2))
    if(!is.null(sum1)){
        ic_ci <- cbind("ic", sum1$effect.measures$ATE$estimate, sum1$effect.measures$ATE$CI)
    }else{
      ic_ci <- c("ic","","")
    }
  if(!is.null(sum2)){
      tmle_ci <- cbind("tmle", sum2$effect.measures$ATE$estimate, sum2$effect.measures$ATE$CI)
    }else{
    tmle_ci <- c("tmle","","")
  }

  return(data.frame(rbind(ic_ci,tmle_ci)))
  }
stopCluster(cl)

saveRDS(data.frame(ci_df),paste0("./tmp/ci_est_",dataname,".RDS"))
