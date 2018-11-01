#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk_all_1.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk_all_2.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk_all_3.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk_all_4.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_tsk_all_5.RData")

# Since we are faced with storage space limitation, don't use copy() here.
da_breadth_all <- da_tsk_all_1
#da_breadth_all <- da_tsk_all_2
#da_breadth_all <- da_tsk_all_3
#da_breadth_all <- da_tsk_all_4
#da_breadth_all <- da_tsk_all_5

now()
da_intermediate <- NULL
ym_index <- da_breadth_all[,sort(unique(ym))]
for (i in 1:length(ym_index)) {
  da_sub <- da_breadth_all[ym==ym_index[i],]
  selected_code <- da_sub[,.N,by=SecCode][N>=120,SecCode]
  da_sub <- da_sub[SecCode %in% selected_code,]
  da_intermediate <- rbind(da_intermediate,da_sub)
}
now() 
da_breadth_all <- da_intermediate

da_breadth_y <- da_breadth_all[,.(breadth=mean(ret_e)-median(ret_e)),keyby=.(ym,SecCode)]

da_breadth_y_1 <- da_breadth_y
#da_breadth_y_2 <- da_breadth_y
#da_breadth_y_3 <- da_breadth_y
#da_breadth_y_4 <- da_breadth_y
#da_breadth_y_5 <- da_breadth_y

save(da_breadth_y_1,file="C:/Users/Ding/Desktop/da_breadth_y_1.RData")
#save(da_breadth_y_2,file="C:/Users/Ding/Desktop/da_breadth_y_2.RData")
#save(da_breadth_y_3,file="C:/Users/Ding/Desktop/da_breadth_y_3.RData")
#save(da_breadth_y_4,file="C:/Users/Ding/Desktop/da_breadth_y_4.RData")
#save(da_breadth_y_5,file="C:/Users/Ding/Desktop/da_breadth_y_5.RData")

rm(list=ls())

#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_breadth_y_1.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_breadth_y_2.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_breadth_y_3.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_breadth_y_4.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_breadth_y_5.RData")

da_breadth_y <- rbind(da_breadth_y_1,da_breadth_y_2,da_breadth_y_3,da_breadth_y_4,da_breadth_y_5)

save(da_breadth_y,file="C:/Users/Ding/Desktop/da_breadth_y.RData")

