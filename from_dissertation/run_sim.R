# Load required packages
library(ltmle)
library(doParallel)
library(tidyverse)
library(SuperLearner)
library(data.table)
library(doRNG)

# Set up parallel backend
(cl <-makeCluster(detectCores()-30))
registerDoParallel(cl)


### SET HERE
model <- "glm"
reps <- 200
N_time <- 4
## END

###load data
dataname <-paste0("simdata_list_",as.character(reps),"_","t",as.character(N_time),"_",model)
simdata_list <- readRDS(paste0("./tmp/",dataname,".RDS"))
dataname <-paste0("simdata_list_",as.character(reps),"_","t",as.character(N_time),"_",model)

# Set parameters: test truncation levels, SL libraries (later variance estimation)
trunc_levels <- rbind(c(0.01,1),c(0.001,1),c(0,1)) # Example truncation levels, adjust as needed
libs <- rbind(c("glm"),c("SL.mean,glm,SL.glm,SL.glm.interaction"),c("SL.mean,glm,SL.glm.interaction,SL.earth,SL.xgboost,SL.randomForest"))

(specs <- expand_grid(libs,trunc_levels))
# specs_list <- (strsplit(specs$libs,","))
# lapply(specs_list,FUN = function(x)replace("SL.glm.interaction",c("x","x"),c(SL.glm.interaction")))
#replace interaction algorithm to add a screener:


#set psi0 for now, need to verify later
psi0 <- -0.13711
#attr(SL.library, "return.fit") == TRUE
rng <- RNGseq( length(simdata_list) * nrow(specs), seed=1234)


##input: simdata_list: list of simulated datasets
# Function to run analysis with ltmle package
run_ltmle <- function(data, SL.library, trunc_level) {
  est_obj <- ltmle(data,
        Anodes=names(data)[grepl("glp",names(data))],
        Cnodes=names(data)[grepl("censor",names(data))],
        Lnodes=names(data)[grepl("insulin",names(data))],
        Ynodes=names(data)[grepl("mace",names(data))],
        survivalOutcome = TRUE,
        SL.library=SL.library,
        gbounds=trunc_level,
        abar = list(rep(1,length(names(data)[grepl("glp",names(data))])),
                    rep(0,length(names(data)[grepl("glp",names(data))]))))
}


# Main simulation loop using foreach
starttime <- Sys.time()
# Run analysis for each library/truncation level combination
est_df <- foreach(i = 1:length(simdata_list),.combine='rbind') %:% #
                foreach(j = 1:nrow(specs),.combine='cbind',r=rng[(i - 1) * row(specs) + 1:row(specs)]) %dopar% {
                  print(paste0("data iteration: ",i,"; specs iteration: ",j))
                  print(paste0("time elapsed:",Sys.time()-starttime))
                  library(ltmle)
                  SL.library <- as.list(unlist(strsplit(specs$libs[j],",")))
                  # Run ltmle analysis
                  try(result <- run_ltmle(data=simdata_list[[i]], SL.library=SL.library, trunc_level=specs$trunc_levels[j,]))
                  if(exists("result")){
                    result.summary <- summary(result,"tmle")
                    result.ipw.summary <- summary(result,"iptw")
                    result.combine <- rbind(result.summary$effect.measures$ATE$estimate,result.ipw.summary$effect.measures$ATE$estimate)
                    result.combine <- data.frame(result.combine,c("tmle","ipw"))
                    names(result.combine) <- c(paste0("est",j),"type")
                    return(result.combine)
                }else{ return(NA)}
                }

# Stop parallel backend
stopCluster(cl)
est_df <- data.table(est_df)
est_df

output_results <- function(est_df,thistype){ 
  ## separate out estimators
  est_matrix <- as.matrix(est_df[type==thistype,(names(est_df)[grepl("est",names(est_df))]),with=FALSE])
  # Output results
   (bias <- colMeans(est_matrix)-psi0)
   (mse <- colMeans((est_matrix-psi0)^2))
   (sd_ests <- apply(est_matrix,2,function(x)sqrt(var(x))))
  oraclecov <- as.vector(foreach(i=1:ncol(est_matrix),.combine="cbind")%do%{
   ub <- (est_matrix[,i])+(1.96*sd_ests[i])
   lb <- (est_matrix[,i])-(1.96*sd_ests[i])
    mean(ifelse(psi0>=lb & psi0<=ub,1,0))
  })
  ## output results table
  specs <-data.table(specs)
  setnames(specs,"libs.V1","SL.library",skip_absent=TRUE)
  specs[,truncation:= paste0("[",trunc_levels.V1,",",trunc_levels.V2,"]"),]
  (output <- data.table(cbind(toupper(thistype),specs[,c("SL.library","truncation")],bias,mse,as.vector(oraclecov),sd_ests)))
  names(output) <- c("Estimator","SL.library","truncation_levels","bias","mse","o_coverage","sd_estimator")
  rownames(output) <-NULL
  return(output)
}

results_ipw <- output_results(est_df,thistype="ipw")
results_tmle <- output_results(est_df,thistype="tmle")



save(list=c("est_df","specs"),file = paste0("./tmp/est_matrix_",dataname,"_Psi0",as.character(psi0),".RData"))
write.csv(rbind(results_tmle,results_ipw),paste0("./output/simresults_",dataname,"_psi0_",as.character(psi0),"_",dataname,"_",Sys.Date(),".csv"))
print(paste0("time elapsed: ",Sys.time()-starttime))