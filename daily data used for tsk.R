#################################
#### Combine the Daily Data
load("F:/我的论文/第五篇/RData/da_all_1.RData")
load("F:/我的论文/第五篇/RData/da_all_2.RData")
load("F:/我的论文/第五篇/RData/da_all_3.RData")
load("F:/我的论文/第五篇/RData/da_all_4.RData")
load("F:/我的论文/第五篇/RData/da_all_5.RData")
load("F:/我的论文/第五篇/RData/da_all_6.RData")
load("F:/我的论文/第五篇/RData/da_all_7.RData")
load("F:/我的论文/第五篇/RData/da_all_8.RData")

da_tsk <- as.data.table(rbind(da_all_1,da_all_2,da_all_3,da_all_4,
                               da_all_5,da_all_6,da_all_7,da_all_8))
da_tsk <- da_tsk[(SecCode>=600000 & SecCode<604000) | SecCode<2000 | 
                     (SecCode>2000 & SecCode<3000) | (SecCode>300000 & SecCode<301000),]
# Main Board(Shanghai Stock Exchange)
# Main Board(Shenzhen Stock Exchange)
# Small and Medium Enterprise Board (SME Board)
# Second Board

da_tsk <- na.omit(da_tsk[,.(SecCode,TDate,adpr,rf)])

da_tsk[,TDate:=ymd(TDate)]
da_tsk[,ym:=ymd(paste0(lubridate::year(TDate),"-",
                        lubridate::month(TDate),"-01"))]
da_tsk <- da_tsk[order(SecCode,TDate),]
da_tsk[,adpr_ld:=c(NA,adpr[-(.N)]),by=SecCode]
da_tsk <- na.omit(da_tsk)
da_tsk[,ret_d:=log(adpr)-log(adpr_ld)] 
da_tsk <- da_tsk[order(SecCode,TDate),]

da_tsk[,`:=`(ret_e=ret_d-rf)] 
da_tsk <- na.omit(da_tsk)
nrow(da_tsk)

da_tsk <- da_tsk[,.(ym,SecCode,ret_e)]

save(da_tsk,file="C:/Users/Ding/Desktop/da_tsk.RData")
