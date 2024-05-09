# Load required packages
library(ltmle)
library(doParallel)
library(tidyverse)
library(SuperLearner)
library(data.table)
library(doRNG)

# Set up parallel backend
(cl <-makeCluster(detectCores()/2))
registerDoParallel(cl)


### SET HERE
model <- "glm"
reps <- 200
## END

###load data
dataname <-paste0("simdata_list_",as.character(reps),"_",model)
simdata_list <- readRDS(paste0("./tmp/",dataname,".RDS"))

# Set parameters: test truncation levels, SL libraries (later variance estimation)
trunc_levels <- rbind(c(0.01,1),c(0.001,1),c(0,1)) # Example truncation levels, adjust as needed
libs <- rbind(c("SL.mean","SL.glm","SL.speedglm"),c("SL.glm.interaction","SL.ridge","SL.glmnet"),c("SL.xgboost","SL.nnet","SL.ranger"))
# libs <- rbind(c("SL.mean","SL.glm","SL.speedglm"),c("SL.mean","SL.xgboost","SL.glmnet"),c("SL.mean","SL.earth","SL.ranger"))
(specs <- expand_grid(libs,trunc_levels))


#set psi0 for now, need to verify later
psi0 <- -0.13711#-0.0461723#-0.2325635
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
# Run analysis for each library/truncation level combination
est_matrix <- foreach(i = 1:length(simdata_list),.combine='rbind') %:% #
                foreach(j = 1:nrow(specs),.combine='cbind',r=rng[(i - 1) * row(specs) + 1:row(specs)]) %dopar% {
                  print(paste0("data iteration: ",i,"; specs iteration: ",j))
                  library(ltmle)
                  # Run ltmle analysis
                  try(result <- run_ltmle(data=simdata_list[[i]], SL.library=specs$libs[j,], trunc_level=specs$trunc_levels[j,]))
                  if(exists("result")){
                    result.summary <- summary(result)
                    return(result.summary$effect.measures$ATE$estimate)
                }else{ return(NA)}
                }

# Stop parallel backend
stopCluster(cl)

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
specs[,SL.library:= paste0(libs.V1,",",libs.V2,",",libs.V3),]
specs[,truncation:= paste0("[",trunc_levels.V1,",",trunc_levels.V2,"]"),]
(output <- data.table(cbind(specs[,c("SL.library","truncation")],bias,mse,as.vector(oraclecov))))
names(output) <- c("SL.library","truncation_levels","bias","mse","o_coverage")
rownames(output) <-NULL
kableExtra::kbl(output)
save(list=c("est_matrix","specs"),file = paste0("./tmp/est_matrix_",dataname,"_Psi0",as.character(psi0),".RData"))
write.csv(output,paste0("./output/simresults_",dataname,"_psi0_",as.character(psi0),"_",dataname,".csv"))
