
simdata_list_200_t4_glm <- readRDS("./tmp/simdata_list_200_t4_glm.RDS")
dt <-data.table(read.csv("./data/fakedataset.csv"))
dt <-dt[,-c("X")]
#par(mfrow=c(1,2))
png("./output/corrplot_realdata.png")
corrplot(cor(as.matrix(dt)), method = "color")
dev.off()
png("./output/corrplot_fakedata.png")
corrplot(cor(as.matrix(simdata_list_200_t4_glm[[1]])), method = "color")
dev.off()
#for(i in 1:length(simdata_list)){
  corrplot(cor(as.matrix(simdata_list_200_t4_glm[[1]])), method = "color")
#}

tab1 <- print(tableone::CreateCatTable(data=dt,vars = names(dt)[6:20]))
write.csv(tab1,file="./output/tab1.csv")
tab2 <- print(tableone::CreateCatTable(data=simdata_list_200_t4_glm[[1]],vars = names(simdata_list_200_t4_glm[[1]])[6:20]))
write.csv(tab2,file="./output/tab2.csv")
