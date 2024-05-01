
coefs <-  data.table::fread("../../cvd_simulation/data/coefficients.txt")

source("../../cvd_simulation/functions/synthesizeDD.R")
source("../../cvd_simulation/functions/sim_sig_data.R")
data_specs <- synthesizeDD(coefs)
N_time=4
dt <- data.table(sim_sig_data(data_specs,
                              n=60000,
                              N_time=N_time,
                              iter = 1)[[1]])
