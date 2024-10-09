library(lava)
library(data.table)
library(tidyverse)
coefs <-  data.table::fread("../../cvd_simulation/data/coefficients.txt")

source("../../cvd_simulation/functions/synthesizeDD.R")
source("../../cvd_simulation/functions/sim_sig_data.R")
data_specs <- synthesize(coefs)
N_time=4
N=50000
set.seed(255)
dt <- data.table(sim_sig_data(data_specs,
                              n=N,
                              N_time=N_time,
                              iter = 1)[[1]])
dt <- dt %>% select("age_base", "sex","ie_type","code5txt","quartile_income",
                  apply(expand.grid(c("censor_","glp1_","insulin_","mace_hf_"),1:N_time),1,function(x) paste0(x,sep="",collapse="")))

names(dt)
write_csv(dt,paste0("./data/fakedataset_t",N_time,"_n",N,".csv"))
