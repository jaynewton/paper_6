################################# 
load("F:/我的论文/第五篇/RData/da_all_m_1995.RData")
load("F:/我的论文/第五篇/RData/FF3F_A_m_1995.RData")

da_isk_m <- da_all_m_1995[,.(ym,SecCode,ret_e_tm)]
da_isk_m <- merge(da_isk_m,FF3F_A_m_1995,by="ym")

da_ym <- da_isk_m[,.(ym=seq.Date(ym[1],ym[.N],by="month")),by=SecCode]
da_isk_m <- merge(da_isk_m,da_ym,by=c("SecCode","ym"),all.y=T)

selectedcode <- da_isk_m[,.N,by=SecCode][N>=60,SecCode]
da_isk_m <- da_isk_m[SecCode %in% selectedcode,]

da_isk_m[,intercept:=1]
#da_isk_m <- da_isk_m[,.(SecCode,ym,ret_e_tm,intercept,mkt_e)]

now()
FUN_ISK <- function(da) {
  da <- na.omit(da)
  if (nrow(da)<50) {
    isk <- as.numeric(NA)
  } else {
    coef_isk <- (solve(t(da[,2:3]) %*% da[,2:3]) %*% 
                   t(da[,2:3]) %*% da[,1])[,1]
    isk <- skew(da[,1]-da[,2:3] %*% coef_isk)
  }
  return(isk)
}

da_isk_m[,isk:=rollapply(as.matrix(.SD),60,FUN = FUN_ISK,by=1,
                         by.column = FALSE,align = "right", fill = NA),
         by=SecCode,.SDcols=c("ret_e_tm","intercept","mkt_e")]
# We require a stock must have at least 50-month trading record in past 60 months.
now()

da_isk_mkt_5y <- na.omit(da_isk_m[ym>=ymd("2000-1-1"),.(ym,SecCode,isk)])

save(da_isk_mkt_5y,file="C:/Users/Ding/Desktop/da_isk_mkt_5y.RData")

