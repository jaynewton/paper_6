#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_isk_mkt_2.RData")

da_isk_all <- da_isk_mkt_2[,.(ym,SecCode,ret_e,intercept,mkt_e)]

now()
da_intermediate <- NULL
ym_index <- da_isk_all[,sort(unique(ym))]
for (i in 1:length(ym_index)) {
  da_sub <- da_isk_all[ym==ym_index[i],]
  selected_code <- da_sub[,.N,by=SecCode][N>=15,SecCode]
  da_sub <- da_sub[SecCode %in% selected_code,]
  da_intermediate <- rbind(da_intermediate,da_sub)
}
now() 
da_isk_all <- da_intermediate

FUN_ISK <- function(da) {
  da <- na.omit(da)
  coef_isk <- (solve(t(da[,2:3]) %*% da[,2:3]) %*% 
                 t(da[,2:3]) %*% da[,1])[,1]
  isk <- skew(da[,1]-da[,2:3] %*% coef_isk)
  return(isk)
}

now()
da_isk_m <- da_isk_all[,.(isk=FUN_ISK(as.matrix(.SD))),
                       keyby=.(ym,SecCode)]
# ret_e,intercept,mkt_e in .SD corresponds to 1,2,3
now()

da_isk_mkt_m <- da_isk_m

save(da_isk_mkt_m,file="C:/Users/Ding/Desktop/da_isk_mkt_m.RData")

