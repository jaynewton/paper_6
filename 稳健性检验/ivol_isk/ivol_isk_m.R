#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_ivol.RData")

da_ivol_all <- da_ivol

now()
da_intermediate <- NULL
ym_index <- da_ivol_all[,sort(unique(ym))]
for (i in 1:length(ym_index)) {
  da_sub <- da_ivol_all[ym==ym_index[i],]
  selected_code <- da_sub[,.N,by=SecCode][N>=15,SecCode]
  da_sub <- da_sub[SecCode %in% selected_code,]
  da_intermediate <- rbind(da_intermediate,da_sub)
}
now() 
da_ivol_all <- da_intermediate

FUN_IVOL <- function(da) {
  da <- na.omit(da)
  coef_ivol <- (solve(t(da[,2:5]) %*% da[,2:5]) %*% 
                  t(da[,2:5]) %*% da[,1])[,1]
  ivol <- sd(da[,1]-da[,2:5] %*% coef_ivol)
  return(ivol)
}

FUN_ISK <- function(da) {
  coef_isk <- (solve(t(da[,2:5]) %*% da[,2:5]) %*% 
                 t(da[,2:5]) %*% da[,1])[,1]
  isk <- skew(da[,1]-da[,2:5] %*% coef_isk)
  return(isk)
}

now()
da_ivol_m <- da_ivol_all[,.(ivol=FUN_IVOL(as.matrix(.SD)),
                            isk=FUN_ISK(as.matrix(.SD))),
                         keyby=.(ym,SecCode)]
# ret_e,intercept,mkt_e,smb,hml in .SD corresponds to 1,2,3,4,5
now()

save(da_ivol_m,file="C:/Users/Ding/Desktop/da_ivol_m.RData")

