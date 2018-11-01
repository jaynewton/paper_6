#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk.RData")

da_tsk_all <- da_tsk

now()
da_intermediate <- NULL
ym_index <- da_tsk_all[,sort(unique(ym))]
for (i in 1:length(ym_index)) {
  da_sub <- da_tsk_all[ym==ym_index[i],]
  selected_code <- da_sub[,.N,by=SecCode][N>=15,SecCode]
  da_sub <- da_sub[SecCode %in% selected_code,]
  da_intermediate <- rbind(da_intermediate,da_sub)
}
now() 
da_tsk_all <- da_intermediate

da_tsk_m <- da_tsk_all[,.(tsk=skew(ret_e)),keyby=.(ym,SecCode)]

save(da_tsk_m,file="C:/Users/Ding/Desktop/da_tsk_m.RData")



