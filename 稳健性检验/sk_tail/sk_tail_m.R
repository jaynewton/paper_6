#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk.RData")

da_sk_tail_all <- da_tsk

now()
da_intermediate <- NULL
ym_index <- da_sk_tail_all[,sort(unique(ym))]
for (i in 1:length(ym_index)) {
  da_sub <- da_sk_tail_all[ym==ym_index[i],]
  selected_code <- da_sub[,.N,by=SecCode][N>=15,SecCode]
  da_sub <- da_sub[SecCode %in% selected_code,]
  da_intermediate <- rbind(da_intermediate,da_sub)
}
now() 
da_sk_tail_all <- da_intermediate

now()
da_sk_tail_m <- da_sk_tail_all[,.(quantile_99=quantile(ret_e,0.99),
                                  quantile_95=quantile(ret_e,0.95),
                                  quantile_50=quantile(ret_e,0.50),
                                  quantile_5=quantile(ret_e,0.05),
                                  quantile_1=quantile(ret_e,0.01)),keyby=.(ym,SecCode)]
now()
da_sk_tail_m[,`:=`(sk_1=((quantile_99-quantile_50)-(quantile_50-quantile_1))/(quantile_99-quantile_1),
                   sk_5=((quantile_95-quantile_50)-(quantile_50-quantile_5))/(quantile_95-quantile_5))]
da_sk_tail_m <- da_sk_tail_m[,.(ym,SecCode,sk_1,sk_5)]

save(da_sk_tail_m,file="C:/Users/Ding/Desktop/da_sk_tail_m.RData")

