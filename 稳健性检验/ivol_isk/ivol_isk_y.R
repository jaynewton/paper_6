#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_ivol.RData")

da_ivol <- da_ivol[ym>=ymd("2000-1-1") & ym<=ymd("2004-12-31"),]
#da_ivol <- da_ivol[ym>=ymd("2004-2-1") & ym<=ymd("2009-12-31"),]
#da_ivol <- da_ivol[ym>=ymd("2009-2-1") & ym<=ymd("2013-12-31"),]
#da_ivol <- da_ivol[ym>=ymd("2013-2-1") & ym<=ymd("2015-12-31"),]
#da_ivol <- da_ivol[ym>=ymd("2015-2-1") & ym<=ymd("2017-12-31"),]

now()
for (i in 1:11) {
  da_ivol_intermediate <- copy(da_ivol)
  da_ivol_intermediate[,ym:=ym+months(i)]
  assign(paste0("da_ivol_",i),da_ivol_intermediate)
  rm(da_ivol_intermediate)
}
now()

da_ivol_all <- copy(da_ivol)
for (i in 1:11) {
  da_ivol_all <- rbind(da_ivol_all,get(paste0("da_ivol_",i)))
}

for (i in 1:11) {
  assign(paste0("da_ivol_",i),rm)
}

da_ivol_all <- da_ivol_all[order(ym,SecCode),]

da_ivol_all_1 <- da_ivol_all[ym>=ymd("2000-12-1") & ym<=ymd("2004-12-31"),]
#da_ivol_all_2 <- da_ivol_all[ym>=ymd("2005-1-1") & ym<=ymd("2009-12-31"),]
#da_ivol_all_3 <- da_ivol_all[ym>=ymd("2010-1-1") & ym<=ymd("2013-12-31"),]
#da_ivol_all_4 <- da_ivol_all[ym>=ymd("2014-1-1") & ym<=ymd("2015-12-31"),]
#da_ivol_all_5 <- da_ivol_all[ym>=ymd("2016-1-1") & ym<=ymd("2017-12-31"),]

save(da_ivol_all_1,file="C:/Users/Ding/Desktop/da_ivol_all_1.RData")
#save(da_ivol_all_2,file="C:/Users/Ding/Desktop/da_ivol_all_2.RData")
#save(da_ivol_all_3,file="C:/Users/Ding/Desktop/da_ivol_all_3.RData")
#save(da_ivol_all_4,file="C:/Users/Ding/Desktop/da_ivol_all_4.RData")
#save(da_ivol_all_5,file="C:/Users/Ding/Desktop/da_ivol_all_5.RData")

rm(list=ls())

#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_ivol_all_1.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_ivol_all_2.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_ivol_all_3.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_ivol_all_4.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_ivol_all_5.RData")

# Since we are faced with storage space limitation, don't use copy() here.
da_ivol_all <- da_ivol_all_1
#da_ivol_all <- da_ivol_all_2
#da_ivol_all <- da_ivol_all_3
#da_ivol_all <- da_ivol_all_4
#da_ivol_all <- da_ivol_all_5

now()
da_intermediate <- NULL
ym_index <- da_ivol_all[,sort(unique(ym))]
for (i in 1:length(ym_index)) {
  da_sub <- da_ivol_all[ym==ym_index[i],]
  selected_code <- da_sub[,.N,by=SecCode][N>=120,SecCode]
  da_sub <- da_sub[SecCode %in% selected_code,]
  da_intermediate <- rbind(da_intermediate,da_sub)
}
now() 
da_ivol_all <- da_intermediate
# Note: The codes above serve two purposes.
# First, it ensure enough observations that linear regression could be applied.
# Second, we set more strict request of minimum number of obversations for each regression 
# to ganrantee the accuracy of estimation.

FUN_IVOL <- function(da) {
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
da_ivol_y <- da_ivol_all[,.(ivol=FUN_IVOL(as.matrix(.SD)),
                            isk=FUN_ISK(as.matrix(.SD))),by=.(ym,SecCode),
                         .SDcols=c("ret_e","intercept","mkt_e","smb","hml")]
now()

da_ivol_y_1 <- da_ivol_y
#da_ivol_y_2 <- da_ivol_y
#da_ivol_y_3 <- da_ivol_y
#da_ivol_y_4 <- da_ivol_y
#da_ivol_y_5 <- da_ivol_y

save(da_ivol_y_1,file="C:/Users/Ding/Desktop/da_ivol_y_1.RData")
#save(da_ivol_y_2,file="C:/Users/Ding/Desktop/da_ivol_y_2.RData")
#save(da_ivol_y_3,file="C:/Users/Ding/Desktop/da_ivol_y_3.RData")
#save(da_ivol_y_4,file="C:/Users/Ding/Desktop/da_ivol_y_4.RData")
#save(da_ivol_y_5,file="C:/Users/Ding/Desktop/da_ivol_y_5.RData")

rm(list=ls())

#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_ivol_y_1.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_ivol_y_2.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_ivol_y_3.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_ivol_y_4.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_ivol_y_5.RData")

da_ivol_y <- rbind(da_ivol_y_1,da_ivol_y_2,da_ivol_y_3,da_ivol_y_4,da_ivol_y_5)

save(da_ivol_y,file="C:/Users/Ding/Desktop/da_ivol_y.RData")

