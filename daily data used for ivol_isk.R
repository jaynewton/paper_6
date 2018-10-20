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

da_ivol <- as.data.table(rbind(da_all_1,da_all_2,da_all_3,da_all_4,
                               da_all_5,da_all_6,da_all_7,da_all_8))
da_ivol <- da_ivol[(SecCode>=600000 & SecCode<604000) | SecCode<2000 | 
                     (SecCode>2000 & SecCode<3000) | (SecCode>300000 & SecCode<301000),]
# Main Board(Shanghai Stock Exchange)
# Main Board(Shenzhen Stock Exchange)
# Small and Medium Enterprise Board (SME Board)
# Second Board

da_ivol <- na.omit(da_ivol[,.(SecCode,TDate,adpr,rf)])

da_ivol[,TDate:=ymd(TDate)]
da_ivol[,ym:=ymd(paste0(lubridate::year(TDate),"-",
                        lubridate::month(TDate),"-01"))]
da_ivol <- da_ivol[order(SecCode,TDate),]
da_ivol[,adpr_ld:=c(NA,adpr[-(.N)]),by=SecCode]
da_ivol <- na.omit(da_ivol)
da_ivol[,ret_d:=log(adpr)-log(adpr_ld)] 
da_ivol <- da_ivol[order(SecCode,TDate),]

da_ivol[,`:=`(ret_e=ret_d-rf)] 
da_ivol <- na.omit(da_ivol)
nrow(da_ivol)

da_ivol <- merge(da_ivol,FF3F_A_d,by="TDate")
da_ivol[,intercept:=1,]
da_ivol <- da_ivol[,.(ym,SecCode,ret_e,intercept,mkt_e,smb,hml)]

save(da_ivol,file="C:/Users/Ding/Desktop/da_ivol.RData")
