################################# 
load("F:/我的论文/第五篇/RData/da_all_m_1995.RData")
load("F:/我的论文/第五篇/RData/da_mkt_e_m_1995.RData")

da_breadth_m <- da_all_m_1995[,.(ym,SecCode,ret_e_tm)]
da_breadth_m <- merge(da_breadth_m,da_mkt_e_m_1995,by="ym")

da_ym <- da_breadth_m[,.(ym=seq.Date(ym[1],ym[.N],by="month")),by=SecCode]
da_breadth_m <- merge(da_breadth_m,da_ym,by=c("SecCode","ym"),all.y=T)

selectedcode <- da_breadth_m[,.N,by=SecCode][N>=60,SecCode]
da_breadth_m <- da_breadth_m[SecCode %in% selectedcode,]

FUN_BREADTH <- function(vari) { # vari denotes variable
  return(ifelse((60-length(which(is.na(vari))))<50,as.numeric(NA),
                mean(vari,na.rm=T)-median(vari,na.rm=T)))
}

# We require a stock must have at least 40-month trading record in past 60 months.
now()
da_breadth_m[,breadth:=rollapply(ret_e_tm,60,FUN_BREADTH,by=1,align="right",fill=NA),by=SecCode]
now()

da_breadth_5y <- na.omit(da_breadth_m[ym>=ymd("2000-1-1"),.(ym,SecCode,breadth)])

save(da_breadth_5y,file="C:/Users/Ding/Desktop/da_breadth_5y.RData")

