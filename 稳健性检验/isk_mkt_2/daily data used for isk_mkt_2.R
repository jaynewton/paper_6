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
load("F:/我的论文/第五篇/RData/FF3F_A_d.RData")

da_isk <- as.data.table(rbind(da_all_1,da_all_2,da_all_3,da_all_4,
                               da_all_5,da_all_6,da_all_7,da_all_8))
da_isk <- da_isk[(SecCode>=600000 & SecCode<604000) | SecCode<2000 | 
                     (SecCode>2000 & SecCode<3000) | (SecCode>300000 & SecCode<301000),]
# Main Board(Shanghai Stock Exchange)
# Main Board(Shenzhen Stock Exchange)
# Small and Medium Enterprise Board (SME Board)
# Second Board

da_isk <- na.omit(da_isk[,.(SecCode,TDate,adpr,rf)])

da_isk[,TDate:=ymd(TDate)]
da_isk[,ym:=ymd(paste0(lubridate::year(TDate),"-",
                        lubridate::month(TDate),"-01"))]
da_isk <- da_isk[order(SecCode,TDate),]
da_isk[,adpr_ld:=c(NA,adpr[-(.N)]),by=SecCode]
da_isk <- na.omit(da_isk)
da_isk[,ret_d:=log(adpr)-log(adpr_ld)] 
da_isk <- da_isk[order(SecCode,TDate),]

da_isk[,`:=`(ret_e=ret_d-rf)] 
da_isk <- na.omit(da_isk)
nrow(da_isk)

da_isk <- merge(da_isk,FF3F_A_d,by="TDate")
da_isk[,`:=`(intercept=1,mkt_e_2=mkt_e^2),]
da_isk_mkt_2 <- da_isk[,.(ym,SecCode,ret_e,intercept,mkt_e,mkt_e_2)]

save(da_isk_mkt_2,file="C:/Users/Ding/Desktop/da_isk_mkt_2.RData")

