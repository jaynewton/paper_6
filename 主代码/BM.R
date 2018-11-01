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

da_bm <- as.data.table(rbind(da_all_1,da_all_2,da_all_3,da_all_4,
                               da_all_5,da_all_6,da_all_7,da_all_8))
da_bm <- da_bm[(SecCode>=600000 & SecCode<604000) | SecCode<2000 | 
                     (SecCode>2000 & SecCode<3000) | (SecCode>300000 & SecCode<301000),]
# Main Board(Shanghai Stock Exchange)
# Main Board(Shenzhen Stock Exchange)
# Small and Medium Enterprise Board (SME Board)
# Second Board

da_bm <- na.omit(da_bm[,.(SecCode,TDate,PB)])

da_bm[,TDate:=ymd(TDate)]
da_bm[,ym:=ymd(paste0(lubridate::year(TDate),"-",
                        lubridate::month(TDate),"-01"))]
da_bm <- da_bm[order(SecCode,TDate),]

da_bm <- da_bm[ym>=ymd("2000-1-1"),.(ym,SecCode,PB)]

save(da_bm,file="C:/Users/Ding/Desktop/da_bm.RData")

#################################
#### Transform Daily Data into monthly Data 
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_bm.RData")

now()
da_bm_m <- da_bm[,.(BM=mean(1/PB)),keyby=.(ym,SecCode)]
now() 

save(da_bm_m,file="C:/Users/Ding/Desktop/da_bm_m.RData")

#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_bm_m.RData")

da_bm_m[,percentile_BM:=percent_rank(BM),by=ym]
# Note: percent_rank() is different from rank()/(.N-1) when there exists NA.

da_percentile_bm_m <- da_bm_m[,.(ym,SecCode,percentile_BM)]

save(da_percentile_bm_m,file="C:/Users/Ding/Desktop/da_percentile_bm_m.RData")

