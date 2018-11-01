#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk.RData")

da_tsk <- da_tsk[ym>=ymd("2000-1-1") & ym<=ymd("2004-12-31"),]
#da_tsk <- da_tsk[ym>=ymd("2004-2-1") & ym<=ymd("2009-12-31"),]
#da_tsk <- da_tsk[ym>=ymd("2009-2-1") & ym<=ymd("2013-12-31"),]
#da_tsk <- da_tsk[ym>=ymd("2013-2-1") & ym<=ymd("2015-12-31"),]
#da_tsk <- da_tsk[ym>=ymd("2015-2-1") & ym<=ymd("2017-12-31"),]

now()
for (i in 1:11) {
  da_tsk_intermediate <- copy(da_tsk)
  da_tsk_intermediate[,ym:=ym+months(i)]
  assign(paste0("da_tsk_",i),da_tsk_intermediate)
  rm(da_tsk_intermediate)
}
now()

da_tsk_all <- copy(da_tsk)
for (i in 1:11) {
  da_tsk_all <- rbind(da_tsk_all,get(paste0("da_tsk_",i)))
}

for (i in 1:11) {
  assign(paste0("da_tsk_",i),rm)
}

da_tsk_all <- da_tsk_all[order(ym,SecCode),]

da_tsk_all_1 <- da_tsk_all[ym>=ymd("2000-12-1") & ym<=ymd("2004-12-31"),]
#da_tsk_all_2 <- da_tsk_all[ym>=ymd("2005-1-1") & ym<=ymd("2009-12-31"),]
#da_tsk_all_3 <- da_tsk_all[ym>=ymd("2010-1-1") & ym<=ymd("2013-12-31"),]
#da_tsk_all_4 <- da_tsk_all[ym>=ymd("2014-1-1") & ym<=ymd("2015-12-31"),]
#da_tsk_all_5 <- da_tsk_all[ym>=ymd("2016-1-1") & ym<=ymd("2017-12-31"),]

save(da_tsk_all_1,file="C:/Users/Ding/Desktop/da_tsk_all_1.RData")
#save(da_tsk_all_2,file="C:/Users/Ding/Desktop/da_tsk_all_2.RData")
#save(da_tsk_all_3,file="C:/Users/Ding/Desktop/da_tsk_all_3.RData")
#save(da_tsk_all_4,file="C:/Users/Ding/Desktop/da_tsk_all_4.RData")
#save(da_tsk_all_5,file="C:/Users/Ding/Desktop/da_tsk_all_5.RData")

rm(list=ls())

#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk_all_1.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk_all_2.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk_all_3.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk_all_4.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk_all_5.RData")

# Since we are faced with storage space limitation, don't use copy() here.
da_tsk_all <- da_tsk_all_1
#da_tsk_all <- da_tsk_all_2
#da_tsk_all <- da_tsk_all_3
#da_tsk_all <- da_tsk_all_4
#da_tsk_all <- da_tsk_all_5

now()
da_intermediate <- NULL
ym_index <- da_tsk_all[,sort(unique(ym))]
for (i in 1:length(ym_index)) {
  da_sub <- da_tsk_all[ym==ym_index[i],]
  selected_code <- da_sub[,.N,by=SecCode][N>=120,SecCode]
  da_sub <- da_sub[SecCode %in% selected_code,]
  da_intermediate <- rbind(da_intermediate,da_sub)
}
now() 
da_tsk_all <- da_intermediate

da_tsk_y <- da_tsk_all[,.(tsk=skew(ret_e)),keyby=.(ym,SecCode)]

da_tsk_y_1 <- da_tsk_y
#da_tsk_y_2 <- da_tsk_y
#da_tsk_y_3 <- da_tsk_y
#da_tsk_y_4 <- da_tsk_y
#da_tsk_y_5 <- da_tsk_y

save(da_tsk_y_1,file="C:/Users/Ding/Desktop/da_tsk_y_1.RData")
#save(da_tsk_y_2,file="C:/Users/Ding/Desktop/da_tsk_y_2.RData")
#save(da_tsk_y_3,file="C:/Users/Ding/Desktop/da_tsk_y_3.RData")
#save(da_tsk_y_4,file="C:/Users/Ding/Desktop/da_tsk_y_4.RData")
#save(da_tsk_y_5,file="C:/Users/Ding/Desktop/da_tsk_y_5.RData")

rm(list=ls())

#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk_y_1.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk_y_2.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk_y_3.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk_y_4.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk_y_5.RData")

da_tsk_y <- rbind(da_tsk_y_1,da_tsk_y_2,da_tsk_y_3,da_tsk_y_4,da_tsk_y_5)

save(da_tsk_y,file="C:/Users/Ding/Desktop/da_tsk_y.RData")

