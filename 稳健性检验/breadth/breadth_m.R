#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk.RData")

da_breadth_all <- da_tsk

now()
da_intermediate <- NULL
ym_index <- da_breadth_all[,sort(unique(ym))]
for (i in 1:length(ym_index)) {
  da_sub <- da_breadth_all[ym==ym_index[i],]
  selected_code <- da_sub[,.N,by=SecCode][N>=15,SecCode]
  da_sub <- da_sub[SecCode %in% selected_code,]
  da_intermediate <- rbind(da_intermediate,da_sub)
}
now() 
da_breadth_all <- da_intermediate

da_breadth_m <- da_breadth_all[,.(breadth=mean(ret_e)-median(ret_e)),keyby=.(ym,SecCode)]

save(da_breadth_m,file="C:/Users/Ding/Desktop/da_breadth_m.RData")



