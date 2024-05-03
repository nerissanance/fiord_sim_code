coefs <-  data.table::fread("C:/Users/andre/Documents/jici/cvd_simulation/data/coefficients.txt")

# source("C:/Users/andre/Documents/jici/cvd_simulation/functions/synthesizeDD.R")
# source("C:/Users/andre/Documents/jici/cvd_simulation/functions/sim_sig_data.R")
# data_specs <- synthesizeDD(coefs)
# N_time=4
# dt <- data.table(sim_sig_data(data_specs,
#                               n=60000,
#                               N_time=N_time,
#                               iter = 1)[[1]])






synthesizeDD.always <- function(coefficients, A_name = "glp1"){
  requireNamespace("lava")
  coefficients <- data.table(coefficients)
  XNAMES <- names(coefficients)[-(1:3)]
  BETA <- coefficients[,-(1:3),with=0L]
  # collect At  nodes; intervene At=1 and Ct=0 later
  loc_A <- grep(paste0("^", A_name, "_"), XNAMES)
  beta_A <- BETA[, loc_A, with = F]


  INTERCEPT <- coefficients[["(Intercept)"]]
  # empty lava model for simulation
  m <- lvm()
  distribution(m,"age_base") <- normal.lvm(mean=70,sd=10)
  distribution(m,"sex") <- binomial.lvm(p=0.4)
  m <- addvar(m,"ie_type")
  m <- addvar(m,"code5txt")
  m <- addvar(m,"quartile_income")
  # loop across time and variables
  for (j in 1:NROW(coefficients)){
    # At constant 1 -> intercept becomes intercept + At coefficient
    # also remove At from fitted betas
    temp_intercept <- INTERCEPT[j]
    temp_sum_A_coef <- rowSums(beta_A[j], na.rm = T)  # intercept + At coefficients
    temp_intercept <- temp_intercept + temp_sum_A_coef

    V <- coefficients$var[j]
    beta <- unlist(BETA[j,])
    beta[loc_A] <- NA  # absorb A coefficient into intercept for always-on group; not depending on observed A values any more

    X <- XNAMES[!is.na(beta)]
    beta <- beta[!is.na(beta)]
    # add V ~ Intercept + beta X
    distribution(m,V) <- binomial.lvm()
    #intercept(m,V) <- ifelse(grepl("event_death",V), INTERCEPT[j], temp_intercept) #remove competing risk
    intercept(m,V) <-  temp_intercept #remove competing risk
    regression(m,from=X,to=V) <- beta
  }
  class(m) <- c("synthesizeDD",class(m))
  m
}



synthesizeDD.never <- function(coefficients, A_name = "glp1"){
  requireNamespace("lava")
  coefficients <- data.table(coefficients)
  XNAMES <- names(coefficients)[-(1:3)]
  BETA <- coefficients[,-(1:3),with=0L]
  # collect At and Ct nodes; intervene At=1 and Ct=0 later
  loc_A <- grep(paste0("^", A_name, "_"), XNAMES)
  beta_A <- BETA[, loc_A, with = F]


  INTERCEPT <- coefficients[["(Intercept)"]]
  # empty lava model for simulation
  m <- lvm()
  distribution(m,"age_base") <- normal.lvm(mean=70,sd=10)
  distribution(m,"sex") <- binomial.lvm(p=0.4)
  m <- addvar(m,"ie_type")
  m <- addvar(m,"code5txt")
  m <- addvar(m,"quartile_income")
  # loop across time and variables
  for (j in 1:NROW(coefficients)){
    V <- coefficients$var[j]
    beta <- unlist(BETA[j,])
    beta[loc_A] <- NA  # absorb A coefficient into intercept for always-on group; not depending on observed A values any more
    X <- XNAMES[!is.na(beta)]
    beta <- beta[!is.na(beta)]
    # add V ~ Intercept + beta X
    distribution(m,V) <- binomial.lvm()
    intercept(m,V) <- INTERCEPT[j] #keep only intercept for "never on"
    regression(m,from=X,to=V) <- beta
  }
  class(m) <- c("synthesizeDD",class(m))
  m
}




seed <- 3457347
nsamp=1000000
N_time=4

set.seed(seed)
u.always <- synthesizeDD.always(coefs)
u.never <- synthesizeDD.never(coefs)

d.always <- data.table(sim_sig_data(u.always,
                              n=1000000,
                              N_time=N_time,
                              iter = 1)[[1]])

d.never <- data.table(sim_sig_data(u.never,
                               n=1000000,
                               N_time=N_time,
                               iter = 1)[[1]])

mean(d.always$mace_hf_4,na.rm=T) / mean(d.never$mace_hf_4,na.rm=T)
mean(d.always$mace_hf_4,na.rm=T) - mean(d.never$mace_hf_4,na.rm=T)


