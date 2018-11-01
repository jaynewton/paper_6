################################# 
load("F:/我的论文/第五篇/RData/da_all_m_1995.RData")
load("F:/我的论文/第五篇/RData/da_mkt_e_m_1995.RData")

da_sk_tail_m <- da_all_m_1995[,.(ym,SecCode,ret_e_tm)]
da_sk_tail_m <- merge(da_sk_tail_m,da_mkt_e_m_1995,by="ym")

da_ym <- da_sk_tail_m[,.(ym=seq.Date(ym[1],ym[.N],by="month")),by=SecCode]
da_sk_tail_m <- merge(da_sk_tail_m,da_ym,by=c("SecCode","ym"),all.y=T)

selectedcode <- da_sk_tail_m[,.N,by=SecCode][N>=60,SecCode]
da_sk_tail_m <- da_sk_tail_m[SecCode %in% selectedcode,]

FUN_SK_TAIL_1 <- function(vari) { # vari denotes variable
  quantile_99=quantile(vari,0.99,na.rm=T)
  quantile_50=quantile(vari,0.50,na.rm=T)
  quantile_1=quantile(vari,0.01,na.rm=T)
  return(ifelse((60-length(which(is.na(vari))))<50,as.numeric(NA),
                ((quantile_99-quantile_50)-(quantile_50-quantile_1))/(quantile_99-quantile_1)))
}

FUN_SK_TAIL_5 <- function(vari) { # vari denotes variable
  quantile_95=quantile(vari,0.95,na.rm=T)
  quantile_50=quantile(vari,0.50,na.rm=T)
  quantile_5=quantile(vari,0.05,na.rm=T)
  return(ifelse((60-length(which(is.na(vari))))<50,as.numeric(NA),
                ((quantile_95-quantile_50)-(quantile_50-quantile_5))/(quantile_95-quantile_5)))
}

# We require a stock must have at least 40-month trading record in past 60 months.
now()
da_sk_tail_m[,sk_1:=rollapply(ret_e_tm,60,FUN_SK_TAIL_1,by=1,align="right",fill=NA),by=SecCode]
da_sk_tail_m[,sk_5:=rollapply(ret_e_tm,60,FUN_SK_TAIL_5,by=1,align="right",fill=NA),by=SecCode]
now()

da_sk_tail_5y <- na.omit(da_sk_tail_m[ym>=ymd("2000-1-1"),.(ym,SecCode,sk_1,sk_5)])

save(da_sk_tail_5y,file="C:/Users/Ding/Desktop/da_sk_tail_5y.RData")

