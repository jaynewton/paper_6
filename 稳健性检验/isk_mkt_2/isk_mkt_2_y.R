#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_isk_mkt_2.RData")

da_isk <- da_isk_mkt_2[ym>=ymd("2000-1-1") & ym<=ymd("2004-12-31"),]
#da_isk <- da_isk_mkt_2[ym>=ymd("2004-2-1") & ym<=ymd("2009-12-31"),]
#da_isk <- da_isk_mkt_2[ym>=ymd("2009-2-1") & ym<=ymd("2013-12-31"),]
#da_isk <- da_isk_mkt_2[ym>=ymd("2013-2-1") & ym<=ymd("2015-12-31"),]
#da_isk <- da_isk_mkt_2[ym>=ymd("2015-2-1") & ym<=ymd("2017-12-31"),]

now()
for (i in 1:11) {
  da_isk_intermediate <- copy(da_isk)
  da_isk_intermediate[,ym:=ym+months(i)]
  assign(paste0("da_isk_",i),da_isk_intermediate)
  rm(da_isk_intermediate)
}
now()

da_isk_all <- copy(da_isk)
for (i in 1:11) {
  da_isk_all <- rbind(da_isk_all,get(paste0("da_isk_",i)))
}

for (i in 1:11) {
  assign(paste0("da_isk_",i),rm)
}

da_isk_all <- da_isk_all[order(ym,SecCode),]

da_isk_mkt_2_all_1 <- da_isk_all[ym>=ymd("2000-12-1") & ym<=ymd("2004-12-31"),]
#da_isk_mkt_2_all_2 <- da_isk_all[ym>=ymd("2005-1-1") & ym<=ymd("2009-12-31"),]
#da_isk_mkt_2_all_3 <- da_isk_all[ym>=ymd("2010-1-1") & ym<=ymd("2013-12-31"),]
#da_isk_mkt_2_all_4 <- da_isk_all[ym>=ymd("2014-1-1") & ym<=ymd("2015-12-31"),]
#da_isk_mkt_2_all_5 <- da_isk_all[ym>=ymd("2016-1-1") & ym<=ymd("2017-12-31"),]

save(da_isk_mkt_2_all_1,file="C:/Users/Ding/Desktop/da_isk_mkt_2_all_1.RData")
#save(da_isk_mkt_2_all_2,file="C:/Users/Ding/Desktop/da_isk_mkt_2_all_2.RData")
#save(da_isk_mkt_2_all_3,file="C:/Users/Ding/Desktop/da_isk_mkt_2_all_3.RData")
#save(da_isk_mkt_2_all_4,file="C:/Users/Ding/Desktop/da_isk_mkt_2_all_4.RData")
#save(da_isk_mkt_2_all_5,file="C:/Users/Ding/Desktop/da_isk_mkt_2_all_5.RData")

rm(list=ls())

#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_isk_mkt_2_all_1.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_isk_mkt_2_all_2.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_isk_mkt_2_all_3.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_isk_mkt_2_all_4.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_isk_mkt_2_all_5.RData")

# Since we are faced with storage space limitation, don't use copy() here.
da_isk_all <- da_isk_mkt_2_all_1
#da_isk_all <- da_isk_mkt_2_all_2
#da_isk_all <- da_isk_mkt_2_all_3
#da_isk_all <- da_isk_mkt_2_all_4
#da_isk_all <- da_isk_mkt_2_all_5

now()
da_intermediate <- NULL
ym_index <- da_isk_all[,sort(unique(ym))]
for (i in 1:length(ym_index)) {
  da_sub <- da_isk_all[ym==ym_index[i],]
  selected_code <- da_sub[,.N,by=SecCode][N>=120,SecCode]
  da_sub <- da_sub[SecCode %in% selected_code,]
  da_intermediate <- rbind(da_intermediate,da_sub)
}
now() 
da_isk_all <- da_intermediate

FUN_ISK <- function(da) {
  coef_isk <- (solve(t(da[,2:4]) %*% da[,2:4]) %*% 
                 t(da[,2:4]) %*% da[,1])[,1]
  isk <- skew(da[,1]-da[,2:4] %*% coef_isk)
  return(isk)
}

now()
da_isk_y <- da_isk_all[,.(isk=FUN_ISK(as.matrix(.SD))),keyby=.(ym,SecCode)]
# ret_e,intercept,mkt_e,mkt_e_2 in .SD corresponds to 1,2,3,4
now()

da_isk_mkt_2_y_1 <- da_isk_y
#da_isk_mkt_2_y_2 <- da_isk_y
#da_isk_mkt_2_y_3 <- da_isk_y
#da_isk_mkt_2_y_4 <- da_isk_y
#da_isk_mkt_2_y_5 <- da_isk_y

save(da_isk_mkt_2_y_1,file="C:/Users/Ding/Desktop/da_isk_mkt_2_y_1.RData")
#save(da_isk_mkt_2_y_2,file="C:/Users/Ding/Desktop/da_isk_mkt_2_y_2.RData")
#save(da_isk_mkt_2_y_3,file="C:/Users/Ding/Desktop/da_isk_mkt_2_y_3.RData")
#save(da_isk_mkt_2_y_4,file="C:/Users/Ding/Desktop/da_isk_mkt_2_y_4.RData")
#save(da_isk_mkt_2_y_5,file="C:/Users/Ding/Desktop/da_isk_mkt_2_y_5.RData")

rm(list=ls())

#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_isk_mkt_2_y_1.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_isk_mkt_2_y_2.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_isk_mkt_2_y_3.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_isk_mkt_2_y_4.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_isk_mkt_2_y_5.RData")

da_isk_mkt_2_y <- rbind(da_isk_mkt_2_y_1,da_isk_mkt_2_y_2,da_isk_mkt_2_y_3,
                        da_isk_mkt_2_y_4,da_isk_mkt_2_y_5)

save(da_isk_mkt_2_y,file="C:/Users/Ding/Desktop/da_isk_mkt_2_y.RData")

