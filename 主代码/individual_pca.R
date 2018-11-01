#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_inst_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_price_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_ivol_6m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_size_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_profit_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_bm_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_turnover_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_dividend_m.RData")

da_percentile_ivol_6m <- da_percentile_ivol_6m[,.(ym,SecCode,percentile_ivol)]

da_individual_m <- merge(da_percentile_inst_m,da_percentile_price_m,by=c("ym","SecCode"))
da_individual_m <- merge(da_individual_m,da_percentile_ivol_6m,by=c("ym","SecCode"))
da_individual_m <- merge(da_individual_m,da_percentile_size_m,by=c("ym","SecCode"))
da_individual_m <- merge(da_individual_m,da_percentile_profit_m,by=c("ym","SecCode"))
da_individual_m <- merge(da_individual_m,da_percentile_bm_m,by=c("ym","SecCode"))
da_individual_m <- merge(da_individual_m,da_percentile_turnover_m,by=c("ym","SecCode"))
da_individual_m <- merge(da_individual_m,da_percentile_dividend_m,by=c("ym","SecCode"))

selected_ym <- da_individual_m[,.N,by=ym][N>=500,ym]
da_individual_m <- da_individual_m[ym %in% selected_ym,]

da_individual_m[,individual:=-predict(princomp(.SD))[,1],by=ym,
                .SDcols=names(da_individual_m)[!names(da_individual_m) %in% c("ym","SecCode")]]
#da_individual_m[,individual:=-predict(princomp(.SD,cor=T))[,1],by=ym,
#                .SDcols=names(da_individual_m)[!names(da_individual_m) %in% c("ym","SecCode")]]

da_individual_pca_m <- da_individual_m[,.(ym,SecCode,individual)]
  
save(da_individual_pca_m,file="C:/Users/Ding/Desktop/da_individual_pca_m.RData")
  
