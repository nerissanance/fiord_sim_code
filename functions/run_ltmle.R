run_ltmle <- function(data, SL.library, trunc_level, varmethod="ic") {
  est_obj <- ltmle(data,
                   Anodes=names(data)[grepl("glp",names(data))],
                   Cnodes=names(data)[grepl("censor",names(data))],
                   Lnodes=names(data)[grepl("insulin",names(data))],
                   Ynodes=names(data)[grepl("mace",names(data))],
                   survivalOutcome = TRUE,
                   SL.library=SL.library,
                   gbounds=trunc_level,
                   variance.method = varmethod,
                   abar = list(rep(1,length(names(data)[grepl("glp",names(data))])),
                               rep(0,length(names(data)[grepl("glp",names(data))]))))
}
