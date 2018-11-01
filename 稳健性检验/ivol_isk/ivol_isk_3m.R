#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_ivol.RData")

da_ivol <- da_ivol[ym>=ymd("1999-11-1") & ym<=ymd("2009-12-1"),]
#da_ivol <- da_ivol[ym>=ymd("2009-11-1") & ym<=ymd("2014-12-1"),]
#da_ivol <- da_ivol[ym>=ymd("2014-11-1") & ym<=ymd("2017-12-1"),]

now()
for (i in 1:2) {
  da_ivol_intermediate <- copy(da_ivol)
  da_ivol_intermediate[,ym:=ym+months(i)]
  assign(paste0("da_ivol_",i),da_ivol_intermediate)
  rm(da_ivol_intermediate)
}
now()

da_ivol_all <- copy(da_ivol)
for (i in 1:2) {
  da_ivol_all <- rbind(da_ivol_all,get(paste0("da_ivol_",i)))
}

for (i in 1:2) {
  assign(paste0("da_ivol_",i),rm)
}

da_ivol_all <- da_ivol_all[ym>=ymd("2000-1-1") & ym<=ymd("2009-12-1"),]
#da_ivol_all <- da_ivol_all[ym>=ymd("2010-1-1") & ym<=ymd("2014-12-1"),]
#da_ivol_all <- da_ivol_all[ym>=ymd("2015-1-1") & ym<=ymd("2017-12-1"),]

now()
da_intermediate <- NULL
ym_index <- da_ivol_all[,sort(unique(ym))]
for (i in 1:length(ym_index)) {
  da_sub <- da_ivol_all[ym==ym_index[i],]
  selected_code <- da_sub[,.N,by=SecCode][N>=30,SecCode]
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
da_ivol_m <- da_ivol_all[,.(ivol=FUN_IVOL(as.matrix(.SD)),
                            isk=FUN_ISK(as.matrix(.SD))),
                         keyby=.(ym,SecCode)]
# ret_e,intercept,mkt_e,smb,hml in .SD corresponds to 1,2,3,4,5
now()

da_ivol_3m_1 <- da_ivol_m
#da_ivol_3m_2 <- da_ivol_m
#da_ivol_3m_3 <- da_ivol_m

save(da_ivol_3m_1,file="C:/Users/Ding/Desktop/da_ivol_3m_1.RData")
#save(da_ivol_3m_2,file="C:/Users/Ding/Desktop/da_ivol_3m_2.RData")
#save(da_ivol_3m_3,file="C:/Users/Ding/Desktop/da_ivol_3m_3.RData")

rm(list=ls())

#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_ivol_3m_1.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_ivol_3m_2.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_ivol_3m_3.RData")

da_ivol_3m <- rbind(da_ivol_3m_1,da_ivol_3m_2,da_ivol_3m_3)

save(da_ivol_3m,file="C:/Users/Ding/Desktop/da_ivol_3m.RData")

#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_ivol_3m.RData")

da_ivol_3m[,`:=`(percentile_ivol=percent_rank(ivol),percentile_isk=percent_rank(isk)),by=ym]
# Note: percent_rank() is different from rank()/(.N-1) when there exists NA.

da_percentile_ivol_3m <- da_ivol_3m[,.(ym,SecCode,percentile_ivol,percentile_isk)]

save(da_percentile_ivol_3m,file="C:/Users/Ding/Desktop/da_percentile_ivol_3m.RData")

