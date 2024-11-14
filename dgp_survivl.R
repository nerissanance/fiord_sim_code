## DGP example using the survivl and causl packages
## template code provided by rje42 (Robin Evans)

##to install packages (uncomment as needed):
#devtools::install_github("rje42/causl")
#devtools::install_github("rje42/survivl",ref = "devel")#install development version for now


#load in libraries
library(dplyr)
library(ltmle)
library(data.table)
library(survivl)#loads in causl as dependency
library(foreach)
invisible(sapply(list.files("./functions"),FUN=function(x){ source(paste0("./functions/",x))}))
nts <- 4 #time periods
dt <- data.table::data.table(read.csv(paste0("./data/fakedataset_t",nts,"_N5000.csv"), nrows = 5000))
#dt <- data.table::data.table(read.csv(paste0("./data/fakedataset.csv"), nrows = 5000))
#dt <- dt[,-"X"]
names(dt)[-(1:5)] <- paste0(survivl:::rmv_time(names(dt)[-(1:5)]), "_", rep(0:3,each=4))
#dt <- data.table:::as.data.frame.data.table(dt)


forms <- list(list(),
              list(insulin ~ ie_type + age_base + sex + code5txt + quartile_income +  insulin_l1 + glp1_l1),
              list(glp1 ~ ie_type + age_base + sex + code5txt + quartile_income + glp1_l1 + insulin_l0),
              list(mace_hf ~ glp1_l0),
              ~ 1)
#specify function families (for details, see causl:::family_vals)
fams <- list(integer(0), 5, 5, 3, 1) #(Z, L(t), A(t), Y(t))

#specify coefficients for the models: outcome, time-dep covariate, exposure, and copula for dependency between outcome Y and covariates
pars <- list(mace_hf = list(beta=c(0.2, -0.02), phi=1),
             insulin = list(beta=c(-2.5, -0.25, -0.015, -0.05, 0.05, -0.1,
                                   6, 0.5)),
             glp1 = list(beta=c(-1, -0.5, -0.02, -0.1, -0.1, 0.05, 5, 0.2)),
             cop = list(beta=0.5))

## simulate datasets
set.seed(123)
R=2#500 #number of dataset repetitions
simdata_list <-foreach(j=1:R,.combine='list')%do%{
  print(paste0("begin iteration: ",j))
  dat0 <- dt[sample(1:nrow(dt),nrow(dt),replace=T),.(age_base,sex,ie_type,code5txt,quartile_income)]
  #plasmode baseline covariate sampling
  dat <- msm_samp(dat = dat0, formulas=forms, family=fams, pars=pars, T=nts,
                  link = list(character(0), "logit", "logit", "inverse"))
  surv <- rep(TRUE, nrow(dat))
  ## now add back in plasmode censoring
  for (t in seq_len(nts)-1) {
    vnm <- paste0("censor_", t)
    onm <- paste0("mace_hf_", t)
    cnm <- paste0(c("insulin_", "glp1_"), t)
    dat[!surv,c(onm, cnm)] <- NA
    dat[[vnm]] <- NA
    dat[[vnm]][surv] <- dt[[vnm]][surv]
    dat[surv,"T"][dat[surv,vnm] == 1] <- t+1
    surv[surv] <- surv[surv] & (dt[[vnm]][surv] == 0) & (dat[[onm]][surv] == 0)
  }

  ##prep for ltmle
  dat_prep <- data.table(dat)
  dat_prep[ , (names(dat_prep)) := lapply(.SD, function(x){as.numeric(x)}), .SDcols = names(dat_prep)]

  setcolorder(dat_prep,c("age_base","sex","ie_type","code5txt","quartile_income",
                         "glp1_0","censor_0","insulin_0","mace_hf_0",
                         "glp1_1", "censor_1","insulin_1", "mace_hf_1",
                         "glp1_2","censor_2" ,"insulin_2","mace_hf_2",
                         "glp1_3","censor_3","insulin_3","mace_hf_3"))#,
  for(k in 0:(nts-2)){
    dat_prep[get(paste0("mace_hf_",k))==1 & is.na(get(paste0("mace_hf_",k+1))), (paste0("mace_hf_",(k+1))):=1]
    dat_prep[get(paste0("mace_hf_",k))==0 & is.na(get(paste0("mace_hf_",k+1))), (paste0("mace_hf_",(k+1))):=0]
    dat_prep[get(paste0("insulin_",k))==1 & is.na(get(paste0("insulin_",k+1))), (paste0("insulin_",(k+1))):=1]
    dat_prep[get(paste0("insulin_",k))==0 & is.na(get(paste0("insulin_",k+1))), (paste0("insulin_",(k+1))):=0]
    dat_prep[get(paste0("censor_",k))==1 & is.na(get(paste0("censor_",k+1))), (paste0("censor_",(k+1))):=1]
    dat_prep[get(paste0("censor_",k))==0 & is.na(get(paste0("censor_",k+1))), (paste0("censor_",(k+1))):=0]
    dat_prep[get(paste0("glp1_",k))==1 & is.na(get(paste0("glp1_",k+1))), (paste0("glp1_",(k+1))):=1]
    dat_prep[get(paste0("glp1_",k))==0 & is.na(get(paste0("glp1_",k+1))), (paste0("glp1_",(k+1))):=0]
    }
  dat_prep <- dat_prep[,!c("id","status","T")]

  return(dat_prep)
}

saveRDS(simdata_list,file=paste0("./tmp/simdata_list_",as.character(R),"_","t",as.character(nts),"_suvivl.RDS"))


