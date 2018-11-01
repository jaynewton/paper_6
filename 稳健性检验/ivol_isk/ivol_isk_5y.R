################################# 
load("F:/我的论文/第五篇/RData/da_all_m_1995.RData")
load("F:/我的论文/第五篇/RData/FF3F_A_m_1995.RData")

da_ivol_m <- da_all_m_1995[,.(ym,SecCode,ret_e_tm)]
da_ivol_m <- merge(da_ivol_m,FF3F_A_m_1995,by="ym")

da_ym <- da_ivol_m[,.(ym=seq.Date(ym[1],ym[.N],by="month")),by=SecCode]
da_ivol_m <- merge(da_ivol_m,da_ym,by=c("SecCode","ym"),all.y=T)

selectedcode <- da_ivol_m[,.N,by=SecCode][N>=60,SecCode]
da_ivol_m <- da_ivol_m[SecCode %in% selectedcode,]

da_ivol_m[,intercept:=1]
#da_ivol_m <- da_ivol_m[,.(SecCode,ym,ret_e_tm,intercept,mkt_e,smb,hml)]

now()
FUN_IVOL <- function(da) {
  da <- na.omit(da)
  if (nrow(da)<50) {
    ivol <- as.numeric(NA)
  } else {
    coef_ivol <- (solve(t(da[,2:5]) %*% da[,2:5]) %*% 
                   t(da[,2:5]) %*% da[,1])[,1]
    ivol <- sd(da[,1]-da[,2:5] %*% coef_ivol)
  }
  return(ivol)
}

FUN_ISK <- function(da) {
  da <- na.omit(da)
  if (nrow(da)<50) {
    isk <- as.numeric(NA)
  } else {
    coef_isk <- (solve(t(da[,2:5]) %*% da[,2:5]) %*% 
                   t(da[,2:5]) %*% da[,1])[,1]
    isk <- skew(da[,1]-da[,2:5] %*% coef_isk)
  }
  return(isk)
}

da_ivol_m[,ivol:=rollapply(as.matrix(.SD),60,FUN = FUN_IVOL,by=1,
                           by.column = FALSE,align = "right", fill = NA),
          by=SecCode,.SDcols=c("ret_e_tm","intercept","mkt_e","smb","hml")]
da_ivol_m[,isk:=rollapply(as.matrix(.SD),60,FUN = FUN_ISK,by=1,
                          by.column = FALSE,align = "right", fill = NA),
          by=SecCode,.SDcols=c("ret_e_tm","intercept","mkt_e","smb","hml")]
# We require a stock must have at least 50-month trading record in past 60 months.
now()

da_ivol_5y <- na.omit(da_ivol_m[ym>=ymd("2000-1-1"),.(ym,SecCode,ivol,isk)])

save(da_ivol_5y,file="C:/Users/Ding/Desktop/da_ivol_5y.RData")

