#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk.RData")

da_tsk <- da_tsk[ym>=ymd("1999-8-1") & ym<=ymd("2009-12-1"),]
#da_tsk <- da_tsk[ym>=ymd("2009-8-1") & ym<=ymd("2014-12-1"),]
#da_tsk <- da_tsk[ym>=ymd("2014-8-1") & ym<=ymd("2017-12-1"),]

now()
for (i in 1:5) {
  da_tsk_intermediate <- copy(da_tsk)
  da_tsk_intermediate[,ym:=ym+months(i)]
  assign(paste0("da_tsk_",i),da_tsk_intermediate)
  rm(da_tsk_intermediate)
}
now()

da_tsk_all <- copy(da_tsk)
for (i in 1:5) {
  da_tsk_all <- rbind(da_tsk_all,get(paste0("da_tsk_",i)))
}

for (i in 1:5) {
  assign(paste0("da_tsk_",i),rm)
}

da_tsk_all <- da_tsk_all[ym>=ymd("2000-1-1") & ym<=ymd("2009-12-1"),]
#da_tsk_all <- da_tsk_all[ym>=ymd("2010-1-1") & ym<=ymd("2014-12-1"),]
#da_tsk_all <- da_tsk_all[ym>=ymd("2015-1-1") & ym<=ymd("2017-12-1"),]

now()
da_intermediate <- NULL
ym_index <- da_tsk_all[,sort(unique(ym))]
for (i in 1:length(ym_index)) {
  da_sub <- da_tsk_all[ym==ym_index[i],]
  selected_code <- da_sub[,.N,by=SecCode][N>=60,SecCode]
  da_sub <- da_sub[SecCode %in% selected_code,]
  da_intermediate <- rbind(da_intermediate,da_sub)
}
now() 
da_tsk_all <- da_intermediate

da_tsk_m <- da_tsk_all[,.(tsk=skew(ret_e)),keyby=.(ym,SecCode)]

da_tsk_6m_1 <- da_tsk_m
#da_tsk_6m_2 <- da_tsk_m
#da_tsk_6m_3 <- da_tsk_m

save(da_tsk_6m_1,file="C:/Users/Ding/Desktop/da_tsk_6m_1.RData")
#save(da_tsk_6m_2,file="C:/Users/Ding/Desktop/da_tsk_6m_2.RData")
#save(da_tsk_6m_3,file="C:/Users/Ding/Desktop/da_tsk_6m_3.RData")

rm(list=ls())

#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk_6m_1.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk_6m_2.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk_6m_3.RData")

da_tsk_6m <- rbind(da_tsk_6m_1,da_tsk_6m_2,da_tsk_6m_3)

save(da_tsk_6m,file="C:/Users/Ding/Desktop/da_tsk_6m.RData")

