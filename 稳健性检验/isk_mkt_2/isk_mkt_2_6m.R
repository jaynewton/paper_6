#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_isk_mkt_2.RData")

da_isk <- da_isk_mkt_2[ym>=ymd("1999-8-1") & ym<=ymd("2009-12-1"),]
#da_isk <- da_isk_mkt_2[ym>=ymd("2009-8-1") & ym<=ymd("2014-12-1"),]
#da_isk <- da_isk_mkt_2[ym>=ymd("2014-8-1") & ym<=ymd("2017-12-1"),]

now()
for (i in 1:5) {
  da_isk_intermediate <- copy(da_isk)
  da_isk_intermediate[,ym:=ym+months(i)]
  assign(paste0("da_isk_",i),da_isk_intermediate)
  rm(da_isk_intermediate)
}
now()

da_isk_all <- copy(da_isk)
for (i in 1:5) {
  da_isk_all <- rbind(da_isk_all,get(paste0("da_isk_",i)))
}

for (i in 1:5) {
  assign(paste0("da_isk_",i),rm)
}


da_isk_all <- da_isk_all[ym>=ymd("2000-1-1") & ym<=ymd("2009-12-1"),]
#da_isk_all <- da_isk_all[ym>=ymd("2010-1-1") & ym<=ymd("2014-12-1"),]
#da_isk_all <- da_isk_all[ym>=ymd("2015-1-1") & ym<=ymd("2017-12-1"),]

now()
da_intermediate <- NULL
ym_index <- da_isk_all[,sort(unique(ym))]
for (i in 1:length(ym_index)) {
  da_sub <- da_isk_all[ym==ym_index[i],]
  selected_code <- da_sub[,.N,by=SecCode][N>=60,SecCode]
  da_sub <- da_sub[SecCode %in% selected_code,]
  da_intermediate <- rbind(da_intermediate,da_sub)
}
now() 
da_isk_all <- da_intermediate

FUN_ISK <- function(da) {
  da <- na.omit(da)
  coef_isk <- (solve(t(da[,2:4]) %*% da[,2:4]) %*% 
                 t(da[,2:4]) %*% da[,1])[,1]
  isk <- skew(da[,1]-da[,2:4] %*% coef_isk)
  return(isk)
}

now()
da_isk_m <- da_isk_all[,.(isk=FUN_ISK(as.matrix(.SD))),
                         keyby=.(ym,SecCode)]
# ret_e,intercept,mkt_e,mkt_e_2 in .SD corresponds to 1,2,3,4
now()

da_isk_mkt_2_6m_1 <- da_isk_m
#da_isk_mkt_2_6m_2 <- da_isk_m
#da_isk_mkt_2_6m_3 <- da_isk_m

save(da_isk_mkt_2_6m_1,file="C:/Users/Ding/Desktop/da_isk_mkt_2_6m_1.RData")
#save(da_isk_mkt_2_6m_2,file="C:/Users/Ding/Desktop/da_isk_mkt_2_6m_2.RData")
#save(da_isk_mkt_2_6m_3,file="C:/Users/Ding/Desktop/da_isk_mkt_2_6m_3.RData")

rm(list=ls())

#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_isk_mkt_2_6m_1.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_isk_mkt_2_6m_2.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_isk_mkt_2_6m_3.RData")

da_isk_mkt_2_6m <- rbind(da_isk_mkt_2_6m_1,da_isk_mkt_2_6m_2,da_isk_mkt_2_6m_3)

save(da_isk_mkt_2_6m,file="C:/Users/Ding/Desktop/da_isk_mkt_2_6m.RData")

