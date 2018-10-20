#################################
#### Input the Daily Stock Data
now()
da_turnover <- NULL # daily price for all securities
for (i in 1999:2002) {   
  for (j in 1:12) {
    path <- paste0("D:/daily_all/",i,"/",i,"_",j,".csv")
    da_turnover <- rbind(da_turnover,read.csv(path,header=T))
  }
}
da_turnover_1 <- da_turnover[,c(3,9,21)]
colnames(da_turnover_1) <- c("SecCode","TDate","turnover")
save(da_turnover_1,file="C:/Users/Ding/Desktop/da_turnover_1.RData")
rm(list=ls())

now()
da_turnover <- NULL 
for (i in 2003:2006) {   
  for (j in 1:12) {
    path <- paste0("D:/daily_all/",i,"/",i,"_",j,".csv")
    da_turnover <- rbind(da_turnover,read.csv(path,header=T))
  }
}
da_turnover_2 <- da_turnover[,c(3,9,21)]
colnames(da_turnover_2) <- c("SecCode","TDate","turnover")
save(da_turnover_2,file="C:/Users/Ding/Desktop/da_turnover_2.RData")
rm(list=ls())

now()
da_turnover <- NULL 
for (i in 2007:2010) {   
  for (j in 1:12) {
    path <- paste0("D:/daily_all/",i,"/",i,"_",j,".csv")
    da_turnover <- rbind(da_turnover,read.csv(path,header=T))
  }
}
da_turnover_3 <- da_turnover[,c(3,9,21)]
colnames(da_turnover_3) <- c("SecCode","TDate","turnover")
save(da_turnover_3,file="C:/Users/Ding/Desktop/da_turnover_3.RData")
rm(list=ls())

# Note: the colnames of dataframe are different before and after 2011
now()
da_turnover <- NULL
for (j in 1:12) {
  path <- paste0("D:/daily_all/2011/2011_",j,".csv")
  da_turnover <- rbind(da_turnover,read.csv(path,header=T))
}
da_turnover <- rbind(da_turnover,read.csv("D:/daily_all/2011/2011_8_2.csv",header=T))
da_turnover <- rbind(da_turnover,read.csv("D:/daily_all/2011/2011_12_2.csv",header=T))
da_turnover_4 <- da_turnover[,c(3,9,21)]
colnames(da_turnover_4) <- c("SecCode","TDate","turnover")
save(da_turnover_4,file="C:/Users/Ding/Desktop/da_turnover_4.RData")
rm(list=ls())

now()
da_turnover <- NULL
for (i in 2012:2013) {   
  for (j in 1:12) {
    for (k in 1:2) {
      path <- paste0("D:/daily_all/",i,"/",i,"_",j,"_",k,".csv")
      da_turnover <- rbind(da_turnover,read.csv(path,header=T))
    }
  }
}
da_turnover_5 <- da_turnover[,c(3,9,21)]
colnames(da_turnover_5) <- c("SecCode","TDate","turnover")
save(da_turnover_5,file="C:/Users/Ding/Desktop/da_turnover_5.RData")
rm(list=ls())

now()
da_turnover <- NULL
for (i in 2014:2015) {   
  for (j in 1:12) {
    for (k in 1:2) {
      path <- paste0("D:/daily_all/",i,"/",i,"_",j,"_",k,".csv")
      da_turnover <- rbind(da_turnover,read.csv(path,header=T))
    }
  }
}
da_turnover_6 <- da_turnover[,c(3,9,21)]
colnames(da_turnover_6) <- c("SecCode","TDate","turnover")
save(da_turnover_6,file="C:/Users/Ding/Desktop/da_turnover_6.RData")
rm(list=ls())

now()
da_turnover <- NULL
for (i in 2016:2017) {   
  for (j in 1:12) {
    for (k in 1:2) {
      path <- paste0("D:/daily_all/",i,"/",i,"_",j,"_",k,".csv")
      da_turnover <- rbind(da_turnover,read.csv(path,header=T))
    }
  }
}
da_turnover_7 <- da_turnover[,c(3,9,21)]
colnames(da_turnover_7) <- c("SecCode","TDate","turnover")
save(da_turnover_7,file="C:/Users/Ding/Desktop/da_turnover_7.RData")
rm(list=ls())
now()

#################################
#### Combine the Daily Data
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_turnover_1.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_turnover_2.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_turnover_3.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_turnover_4.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_turnover_5.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_turnover_6.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_turnover_7.RData")

da_turnover <- as.data.table(rbind(da_turnover_1,da_turnover_2,da_turnover_3,da_turnover_4,
                              da_turnover_5,da_turnover_6,da_turnover_7))
da_turnover <- da_turnover[(SecCode>=600000 & SecCode<604000) | SecCode<2000 | 
                   (SecCode>2000 & SecCode<3000) | (SecCode>300000 & SecCode<301000),]

da_turnover[,TDate:=ymd(TDate)]
da_turnover[,ym:=ymd(paste0(lubridate::year(TDate),"-",
                            lubridate::month(TDate),"-01"))]
da_turnover <- na.omit(da_turnover[,.(ym,SecCode,turnover)])

save(da_turnover,file="C:/Users/Ding/Desktop/da_turnover.RData")

#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_turnover.RData")

da_turnover <- da_turnover[ym>=ymd("1999-2-1") & ym<=ymd("2009-12-1"),]
#da_turnover <- da_turnover[ym>=ymd("2009-2-1") & ym<=ymd("2014-12-1"),]
#da_turnover <- da_turnover[ym>=ymd("2014-2-1") & ym<=ymd("2017-12-1"),]

da_turnover_1 <- copy(da_turnover)
da_turnover_1[,ym:=ym+months(1)]
da_turnover_2 <- copy(da_turnover)
da_turnover_2[,ym:=ym+months(2)]
da_turnover_3 <- copy(da_turnover)
da_turnover_3[,ym:=ym+months(3)]
da_turnover_4 <- copy(da_turnover)
da_turnover_4[,ym:=ym+months(4)]
da_turnover_5 <- copy(da_turnover)
da_turnover_5[,ym:=ym+months(5)]
da_turnover_6 <- copy(da_turnover)
da_turnover_6[,ym:=ym+months(6)]
da_turnover_7 <- copy(da_turnover)
da_turnover_7[,ym:=ym+months(7)]
da_turnover_8 <- copy(da_turnover)
da_turnover_8[,ym:=ym+months(8)]
da_turnover_9 <- copy(da_turnover)
da_turnover_9[,ym:=ym+months(9)]
da_turnover_10 <- copy(da_turnover)
da_turnover_10[,ym:=ym+months(10)]
da_turnover_11 <- copy(da_turnover)
da_turnover_11[,ym:=ym+months(11)]

da_turnover_past <- rbind(da_turnover_1,da_turnover_2,da_turnover_3,da_turnover_4,
                          da_turnover_5,da_turnover_6,da_turnover_7,da_turnover_8,
                          da_turnover_9,da_turnover_10,da_turnover_11)

da_turnover_1 <- NULL
da_turnover_2 <- NULL
da_turnover_3 <- NULL
da_turnover_4 <- NULL
da_turnover_5 <- NULL
da_turnover_6 <- NULL
da_turnover_7 <- NULL
da_turnover_8 <- NULL
da_turnover_9 <- NULL
da_turnover_10 <- NULL
da_turnover_11 <- NULL
da_turnover_12 <- NULL

da_turnover_past_1 <- da_turnover_past[ym>=ymd("2000-1-1") & ym<=ymd("2009-12-1"),]
#da_turnover_past_2 <- da_turnover_past[ym>=ymd("2010-1-1") & ym<=ymd("2014-12-1"),]
#da_turnover_past_3 <- da_turnover_past[ym>=ymd("2015-1-1") & ym<=ymd("2017-12-1"),]

da_turnover_past_m_1 <- da_turnover_past_1[,.(mean_turnover=mean(turnover)),by=.(ym,SecCode)]
#da_turnover_past_m_2 <- da_turnover_past_2[,.(mean_turnover=mean(turnover)),by=.(ym,SecCode)]
#da_turnover_past_m_3 <- da_turnover_past_3[,.(mean_turnover=mean(turnover)),by=.(ym,SecCode)]

save(da_turnover_past_m_1,file="C:/Users/Ding/Desktop/da_turnover_past_m_1.RData")
#save(da_turnover_past_m_2,file="C:/Users/Ding/Desktop/da_turnover_past_m_2.RData")
#save(da_turnover_past_m_3,file="C:/Users/Ding/Desktop/da_turnover_past_m_3.RData")

#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_turnover_past_m_1.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_turnover_past_m_2.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_turnover_past_m_3.RData")

da_turnover_past_m <- rbind(da_turnover_past_m_1,da_turnover_past_m_2,da_turnover_past_m_3)

save(da_turnover_past_m,file="C:/Users/Ding/Desktop/da_turnover_past_m.RData")

#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_turnover.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_turnover_past_m.RData")

da_turnover_m <- da_turnover[,.(max_turnover=max(turnover)),by=.(ym,SecCode)]
da_turnover_m <- merge(da_turnover_m,da_turnover_past_m,by=c("ym","SecCode"))
da_turnover_m[,turnover:=max_turnover/mean_turnover] 
# turnover denotes abnormal turnover.
da_turnover_m <- da_turnover_m[,.(ym,SecCode,turnover)]

save(da_turnover_m,file="C:/Users/Ding/Desktop/da_turnover_m.RData")

#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_turnover_m.RData")

da_turnover_m[,percentile_turnover:=percent_rank(turnover),by=ym]
# Note: percent_rank() is different from rank()/(.N-1) when there exists NA.

da_percentile_turnover_m <- da_turnover_m[,.(ym,SecCode,percentile_turnover)]

save(da_percentile_turnover_m,file="C:/Users/Ding/Desktop/da_percentile_turnover_m.RData")

