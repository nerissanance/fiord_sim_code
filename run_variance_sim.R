##Purpose: to compare between empirical influence curve and a nonparametric bootstrap for inference
##Runs:
#   -the final analytic strategy specs for n iterations to get empirical IC for R iterations
#   -a non-parametric bootstrap of n_boot iterations on each estimate of R
library(ltmle)

#import functions
sapply(paste0("./functions/",list.files(path="./functions/")),source,.GlobalEnv)

####SET PARAMS: 
model <- "glm"
#bootstrap iterations
reps <- 200
N_time <- 4 
n_boot <- 2#00
final_lib <- c("SL.mean","glm","SL.glm.interaction","SL.earth","SL.xgboost","SL.randomForest")
trunc_level <- c(0.001,1)
####END PARAMS

###load data
#N_time and reps identify the data--this needs to be the same data as what you used for the estimator comparison
dataname <-paste0("simdata_list_",as.character(reps),"_","t",as.character(N_time),"_",model)
simdata_list <- readRDS(paste0("./tmp/",dataname,".RDS"))


try(result <- run_ltmle(data=simdata_list[[i]], 
                        SL.library=SL.library, 
                        trunc_level=specs$trunc_levels[j,],
                        varmethod="tmle"))

