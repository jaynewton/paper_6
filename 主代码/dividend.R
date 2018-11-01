#################################
#### Months with Dividend in Past 12 months
da_dividend_date <- read.csv("F:/我的论文/第五篇/主代码/individual investor preference/RData/distribution.csv",
                     header=T,stringsAsFactors=F)[,c(3,9)]
da_dividend_date <- data.table(da_dividend_date)
names(da_dividend_date) <- c("SecCode","dividend_date")

da_dividend_date[,dividend_date:=ymd(dividend_date)]
da_dividend_date[,dividend_ym:=ymd(paste0(lubridate::year(dividend_date),"-",
                                          lubridate::month(dividend_date),"-01"))]
now()
da_dividend_date <- da_dividend_date[,.(ym=seq.Date(dividend_ym,dividend_ym+months(11),by="months")),
                                     by=.(SecCode,dividend_ym)]
now()

da_dividend_date <- da_dividend_date[order(ym,SecCode),]
da_dividend_date <- da_dividend_date[ym>=ymd("2000-1-1"),.(ym,SecCode)]
da_dividend_date <- unique(da_dividend_date) 
# Note: Consider the case of two dividend distribution in one year.

#### Months after IPO
da_ipo_date <- read.csv("F:/我的论文/第九篇/RData/IPO.csv",header=T,stringsAsFactors=F)[,c(3,12)]
da_ipo_date <- as.data.table(da_ipo_date)
names(da_ipo_date) <- c("SecCode","ipo_date")
da_ipo_date[,ipo_ym:=ymd(paste0(lubridate::year(ipo_date),"-",
                                lubridate::month(ipo_date),"-01"))]
da_ipo_date <- da_ipo_date[ipo_ym<=ymd("2017-12-1"),]

now()
da_ipo_date <- da_ipo_date[,.(ym=seq.Date(ipo_ym,ymd("2017-12-1"),by="months")),
                           by=.(SecCode,ipo_ym)]
now()
da_ipo_date <- da_ipo_date[ym>=ymd("2000-1-1"),.(ym,SecCode)]

#### Merge
da_dividend_m_1 <- merge(da_ipo_date,da_dividend_date,by=c("ym","SecCode"))
da_dividend_m_1[,dividend:=1]
da_dividend_m_0 <- fsetdiff(da_ipo_date,da_dividend_date)
da_dividend_m_0[,dividend:=0]

da_dividend_m <- rbind(da_dividend_m_1,da_dividend_m_0)
da_dividend_m <- da_dividend_m[order(ym,SecCode),]

da_dividend_m <- da_dividend_m[(SecCode>=600000 & SecCode<604000) | SecCode<2000 | 
                                 (SecCode>2000 & SecCode<3000) | 
                                 (SecCode>300000 & SecCode<301000),]

save(da_dividend_m,file="C:/Users/Ding/Desktop/da_dividend_m.RData")

#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_dividend_m.RData")

da_dividend_m[,percentile_dividend:=1-dividend]
da_percentile_dividend_m <- da_dividend_m[,.(ym,SecCode,percentile_dividend)]
# Note: There are only two realizations of dividend variable, namely, 0 and 1.
# These two figures also represents the correspongding percentile.

save(da_percentile_dividend_m,file="C:/Users/Ding/Desktop/da_percentile_dividend_m.RData")

