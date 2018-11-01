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

da_price <- as.data.table(rbind(da_all_1,da_all_2,da_all_3,da_all_4,
                              da_all_5,da_all_6,da_all_7,da_all_8))
da_price <- da_price[(SecCode>=600000 & SecCode<604000) | SecCode<2000 | 
                       (SecCode>2000 & SecCode<3000) | (SecCode>300000 & SecCode<301000),.(SecCode,TDate,clpr)]

da_price[,TDate:=ymd(TDate)]
da_price[,ym:=ymd(paste0(lubridate::year(TDate),"-",
                       lubridate::month(TDate),"-01"))]
da_price <- da_price[order(SecCode,TDate),]
da_price <- na.omit(da_price)

da_price_m <- da_price[ym>=ymd("2000-1-1"),.(price=mean(clpr)),by=.(ym,SecCode)]

save(da_price_m,file="C:/Users/Ding/Desktop/da_price_m.RData")

#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_price_m.RData")

da_price_m[,percentile_price:=1-percent_rank(price),by=ym]
# Note: percent_rank() is different from rank()/(.N-1) when there exists NA.

da_percentile_price_m <- da_price_m[,.(ym,SecCode,percentile_price)]

save(da_percentile_price_m,file="C:/Users/Ding/Desktop/da_percentile_price_m.RData")

