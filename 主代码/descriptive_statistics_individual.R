#################################
load("F:/我的论文/第五篇/RData/da_all_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_individual_m.RData")
load("F:/我的论文/第五篇/RData/da_inst_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_price_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_ivol_6m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_size_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_profit_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_bm_m.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")
#load("F:/我的论文/第五篇/主代码/beta anomaly/daily data in one year/RData/da_beta_y.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_turnover_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_dividend_m.RData")

da_all_m <- da_all_m[,.(ym,SecCode,max_ret)]
da_individual_m <- da_individual_m[,.(ym,SecCode,individual)]
da_ivol_6m <- da_ivol_6m[,.(ym,SecCode,ivol)]
da_profit_m <- na.omit(da_profit_m)

da_m <- merge(da_all_m,da_individual_m,by=c("ym","SecCode"))
da_m <- merge(da_m,da_inst_m,by=c("ym","SecCode"))
da_m <- merge(da_m,da_price_m,by=c("ym","SecCode"))
da_m <- merge(da_m,da_ivol_6m,by=c("ym","SecCode"))
da_m <- merge(da_m,da_size_m,by=c("ym","SecCode"))
da_m <- merge(da_m,da_profit_m,by=c("ym","SecCode"))
da_m <- merge(da_m,da_bm_m,by=c("ym","SecCode"))
da_m <- merge(da_m,da_beta_5y,by=c("ym","SecCode"))
da_m <- merge(da_m,da_turnover_m,by=c("ym","SecCode"))
da_m <- merge(da_m,da_dividend_m,by=c("ym","SecCode"))

which(is.na(da_m),arr.ind = T)
cor(da_m[,.(max_ret,individual,inst,price,ivol,size,eps,roe,
            opps,BM,be,turnover,dividend)],method="pearson")
#cor(da_m[,.(max_ret,individual,inst,price,ivol,size,eps,roe,
#            opps,BM,be,turnover,dividend)],method="spearman")

ds_2 <- describe(da_m[,.(rkt,max_ret,rvol,rsk,be,size,BM,mom,ret_tm,illiq)])[,c("mean","median","sd","skew","kurtosis","min","max")]
ds_2[,"kurtosis"] <- ds_2[,"kurtosis"]+3
#ds_2
format(ds_2,digits=3)