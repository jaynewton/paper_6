#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk.RData")

da_sk_tail <- da_tsk[ym>=ymd("1999-8-1") & ym<=ymd("2009-12-1"),]
#da_sk_tail <- da_tsk[ym>=ymd("2009-8-1") & ym<=ymd("2014-12-1"),]
#da_sk_tail <- da_tsk[ym>=ymd("2014-8-1") & ym<=ymd("2017-12-1"),]

now()
for (i in 1:5) {
  da_sk_tail_intermediate <- copy(da_sk_tail)
  da_sk_tail_intermediate[,ym:=ym+months(i)]
  assign(paste0("da_sk_tail_",i),da_sk_tail_intermediate)
  rm(da_sk_tail_intermediate)
}
now()

da_sk_tail_all <- copy(da_sk_tail)
for (i in 1:5) {
  da_sk_tail_all <- rbind(da_sk_tail_all,get(paste0("da_sk_tail_",i)))
}

for (i in 1:5) {
  assign(paste0("da_sk_tail_",i),rm)
}

da_sk_tail_all <- da_sk_tail_all[ym>=ymd("2000-1-1") & ym<=ymd("2009-12-1"),]
#da_sk_tail_all <- da_sk_tail_all[ym>=ymd("2010-1-1") & ym<=ymd("2014-12-1"),]
#da_sk_tail_all <- da_sk_tail_all[ym>=ymd("2015-1-1") & ym<=ymd("2017-12-1"),]

now()
da_intermediate <- NULL
ym_index <- da_sk_tail_all[,sort(unique(ym))]
for (i in 1:length(ym_index)) {
  da_sub <- da_sk_tail_all[ym==ym_index[i],]
  selected_code <- da_sub[,.N,by=SecCode][N>=60,SecCode]
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

da_sk_tail_6m_1 <- da_sk_tail_m
#da_sk_tail_6m_2 <- da_sk_tail_m
#da_sk_tail_6m_3 <- da_sk_tail_m

save(da_sk_tail_6m_1,file="C:/Users/Ding/Desktop/da_sk_tail_6m_1.RData")
#save(da_sk_tail_6m_2,file="C:/Users/Ding/Desktop/da_sk_tail_6m_2.RData")
#save(da_sk_tail_6m_3,file="C:/Users/Ding/Desktop/da_sk_tail_6m_3.RData")

rm(list=ls())

#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_sk_tail_6m_1.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_sk_tail_6m_2.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_sk_tail_6m_3.RData")

da_sk_tail_6m <- rbind(da_sk_tail_6m_1,da_sk_tail_6m_2,da_sk_tail_6m_3)

save(da_sk_tail_6m,file="C:/Users/Ding/Desktop/da_sk_tail_6m.RData")

