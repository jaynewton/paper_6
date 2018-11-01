#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk_all_1.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk_all_2.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk_all_3.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk_all_4.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk_all_5.RData")

# Since we are faced with storage space limitation, don't use copy() here.
da_sk_tail_all <- da_tsk_all_1
#da_sk_tail_all <- da_tsk_all_2
#da_sk_tail_all <- da_tsk_all_3
#da_sk_tail_all <- da_tsk_all_4
#da_sk_tail_all <- da_tsk_all_5

now()
da_intermediate <- NULL
ym_index <- da_sk_tail_all[,sort(unique(ym))]
for (i in 1:length(ym_index)) {
  da_sub <- da_sk_tail_all[ym==ym_index[i],]
  selected_code <- da_sub[,.N,by=SecCode][N>=120,SecCode]
  da_sub <- da_sub[SecCode %in% selected_code,]
  da_intermediate <- rbind(da_intermediate,da_sub)
}
now() 
da_sk_tail_all <- da_intermediate

now()
da_sk_tail_y <- da_sk_tail_all[,.(quantile_99=quantile(ret_e,0.99),
                                  quantile_95=quantile(ret_e,0.95),
                                  quantile_50=quantile(ret_e,0.50),
                                  quantile_5=quantile(ret_e,0.05),
                                  quantile_1=quantile(ret_e,0.01)),keyby=.(ym,SecCode)]
now()
da_sk_tail_y[,`:=`(sk_1=((quantile_99-quantile_50)-(quantile_50-quantile_1))/(quantile_99-quantile_1),
                   sk_5=((quantile_95-quantile_50)-(quantile_50-quantile_5))/(quantile_95-quantile_5))]
da_sk_tail_y <- da_sk_tail_y[,.(ym,SecCode,sk_1,sk_5)]

da_sk_tail_y_1 <- da_sk_tail_y
#da_sk_tail_y_2 <- da_sk_tail_y
#da_sk_tail_y_3 <- da_sk_tail_y
#da_sk_tail_y_4 <- da_sk_tail_y
#da_sk_tail_y_5 <- da_sk_tail_y

save(da_sk_tail_y_1,file="C:/Users/Ding/Desktop/da_sk_tail_y_1.RData")
#save(da_sk_tail_y_2,file="C:/Users/Ding/Desktop/da_sk_tail_y_2.RData")
#save(da_sk_tail_y_3,file="C:/Users/Ding/Desktop/da_sk_tail_y_3.RData")
#save(da_sk_tail_y_4,file="C:/Users/Ding/Desktop/da_sk_tail_y_4.RData")
#save(da_sk_tail_y_5,file="C:/Users/Ding/Desktop/da_sk_tail_y_5.RData")

rm(list=ls())

#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_sk_tail_y_1.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_sk_tail_y_2.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_sk_tail_y_3.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_sk_tail_y_4.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_sk_tail_y_5.RData")

da_sk_tail_y <- rbind(da_sk_tail_y_1,da_sk_tail_y_2,da_sk_tail_y_3,da_sk_tail_y_4,da_sk_tail_y_5)

save(da_sk_tail_y,file="C:/Users/Ding/Desktop/da_sk_tail_y.RData")

