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

da_size <- as.data.table(rbind(da_all_1,da_all_2,da_all_3,da_all_4,
                               da_all_5,da_all_6,da_all_7,da_all_8))
da_size <- da_size[(SecCode>=600000 & SecCode<604000) | SecCode<2000 | 
                     (SecCode>2000 & SecCode<3000) | (SecCode>300000 & SecCode<301000),]
# Main Board(Shanghai Stock Exchange)
# Main Board(Shenzhen Stock Exchange)
# Small and Medium Enterprise Board (SME Board)
# Second Board

da_size <- na.omit(da_size[,.(SecCode,TDate,clpr,trdshr)])

da_size[,TDate:=ymd(TDate)]
da_size[,ym:=ymd(paste0(lubridate::year(TDate),"-",
                        lubridate::month(TDate),"-01"))]
da_size <- da_size[order(SecCode,TDate),]

da_size <- da_size[ym>=ymd("2000-1-1"),.(ym,SecCode,clpr,trdshr)]

save(da_size,file="C:/Users/Ding/Desktop/da_size.RData")

#################################
#### Transform Daily Data into monthly Data 
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_size.RData")

now()
da_size_m <- da_size[,.(size=mean(clpr*trdshr)),keyby=.(ym,SecCode)]
now() 

save(da_size_m,file="C:/Users/Ding/Desktop/da_size_m.RData")

#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_size_m..RData")

da_size_m[,percentile_size:=1-percent_rank(size),by=ym]
# Note: percent_rank() is different from rank()/(.N-1) when there exists NA.

da_percentile_size_m <- da_size_m[,.(ym,SecCode,percentile_size)]

save(da_percentile_size_m,file="C:/Users/Ding/Desktop/da_percentile_size_m.RData")



