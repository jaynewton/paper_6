#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk.RData")

da_breadth <- da_tsk[ym>=ymd("1999-11-1") & ym<=ymd("2009-12-1"),]
#da_breadth <- da_tsk[ym>=ymd("2009-11-1") & ym<=ymd("2014-12-1"),]
#da_breadth <- da_tsk[ym>=ymd("2014-11-1") & ym<=ymd("2017-12-1"),]

now()
for (i in 1:2) {
  da_breadth_intermediate <- copy(da_breadth)
  da_breadth_intermediate[,ym:=ym+months(i)]
  assign(paste0("da_breadth_",i),da_breadth_intermediate)
  rm(da_breadth_intermediate)
}
now()

da_breadth_all <- copy(da_breadth)
for (i in 1:2) {
  da_breadth_all <- rbind(da_breadth_all,get(paste0("da_breadth_",i)))
}

for (i in 1:2) {
  assign(paste0("da_breadth_",i),rm)
}

da_breadth_all <- da_breadth_all[ym>=ymd("2000-1-1") & ym<=ymd("2009-12-1"),]
#da_breadth_all <- da_breadth_all[ym>=ymd("2010-1-1") & ym<=ymd("2014-12-1"),]
#da_breadth_all <- da_breadth_all[ym>=ymd("2015-1-1") & ym<=ymd("2017-12-1"),]

now()
da_intermediate <- NULL
ym_index <- da_breadth_all[,sort(unique(ym))]
for (i in 1:length(ym_index)) {
  da_sub <- da_breadth_all[ym==ym_index[i],]
  selected_code <- da_sub[,.N,by=SecCode][N>=30,SecCode]
  da_sub <- da_sub[SecCode %in% selected_code,]
  da_intermediate <- rbind(da_intermediate,da_sub)
}
now() 
da_breadth_all <- da_intermediate

da_breadth_m <- da_breadth_all[,.(breadth=mean(ret_e)-median(ret_e)),keyby=.(ym,SecCode)]

da_breadth_3m_1 <- da_breadth_m
#da_breadth_3m_2 <- da_breadth_m
#da_breadth_3m_3 <- da_breadth_m

save(da_breadth_3m_1,file="C:/Users/Ding/Desktop/da_breadth_3m_1.RData")
#save(da_breadth_3m_2,file="C:/Users/Ding/Desktop/da_breadth_3m_2.RData")
#save(da_breadth_3m_3,file="C:/Users/Ding/Desktop/da_breadth_3m_3.RData")

rm(list=ls())

#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_breadth_3m_1.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_breadth_3m_2.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_breadth_3m_3.RData")

da_breadth_3m <- rbind(da_breadth_3m_1,da_breadth_3m_2,da_breadth_3m_3)

save(da_breadth_3m,file="C:/Users/Ding/Desktop/da_breadth_3m.RData")

