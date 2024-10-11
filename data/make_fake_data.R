library(lava)
library(data.table)
library(tidyverse)
coefs <-  data.table::fread("./data/coefficients.txt")

source("./functions/synthesize.R")
source("./functions/sim_sig_data.R")
data_specs <- synthesize(coefs)
N_time=4
N=50000
set.seed(255)
dt <- data.table(sim_sig_data(data_specs,
                              n=N,
                              N_time=N_time,
                              iter = 1)[[1]])

names(dt)
write_csv(dt,paste0("./data/fakedataset_t",N_time,"_n",N,".csv"))
