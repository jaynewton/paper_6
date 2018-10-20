#################################
#### Input the Daily Stock Data
now()
da_profit <- NULL # daily price for all securities
for (i in 2000:2002) {   
  for (j in 1:12) {
    path <- paste0("D:/daily_all/",i,"/",i,"_",j,".csv")
    da_profit <- rbind(da_profit,read.csv(path,header=T))
  }
}
da_profit_1 <- da_profit[,c(3,9,67,68,70)]
colnames(da_profit_1) <- c("SecCode","TDate","eps","roe","opps")
save(da_profit_1,file="C:/Users/Ding/Desktop/da_profit_1.RData")
rm(list=ls())

# eps: Earning per Stock (每股收益)
# roe: Return of Equity (净资产收益率)
# opps: Operating Profit per Stock (每股营业利润)

now()
da_profit <- NULL 
for (i in 2003:2006) {   
  for (j in 1:12) {
    path <- paste0("D:/daily_all/",i,"/",i,"_",j,".csv")
    da_profit <- rbind(da_profit,read.csv(path,header=T))
  }
}
da_profit_2 <- da_profit[,c(3,9,67,68,70)]
colnames(da_profit_2) <- c("SecCode","TDate","eps","roe","opps")
save(da_profit_2,file="C:/Users/Ding/Desktop/da_profit_2.RData")
rm(list=ls())

now()
da_profit <- NULL 
for (i in 2007:2010) {   
  for (j in 1:12) {
    path <- paste0("D:/daily_all/",i,"/",i,"_",j,".csv")
    da_profit <- rbind(da_profit,read.csv(path,header=T))
  }
}
da_profit_3 <- da_profit[,c(3,9,67,68,70)]
colnames(da_profit_3) <- c("SecCode","TDate","eps","roe","opps")
save(da_profit_3,file="C:/Users/Ding/Desktop/da_profit_3.RData")
rm(list=ls())

# Note: the colnames of dataframe are different before and after 2011
now()
da_profit <- NULL
for (j in 1:12) {
  path <- paste0("D:/daily_all/2011/2011_",j,".csv")
  da_profit <- rbind(da_profit,read.csv(path,header=T))
}
da_profit <- rbind(da_profit,read.csv("D:/daily_all/2011/2011_8_2.csv",header=T))
da_profit <- rbind(da_profit,read.csv("D:/daily_all/2011/2011_12_2.csv",header=T))
da_profit_4 <- da_profit[,c(3,9,67,68,70)]
colnames(da_profit_4) <- c("SecCode","TDate","eps","roe","opps")
save(da_profit_4,file="C:/Users/Ding/Desktop/da_profit_4.RData")
rm(list=ls())

now()
da_profit <- NULL
for (i in 2012:2013) {   
  for (j in 1:12) {
    for (k in 1:2) {
      path <- paste0("D:/daily_all/",i,"/",i,"_",j,"_",k,".csv")
      da_profit <- rbind(da_profit,read.csv(path,header=T))
    }
  }
}
da_profit_5 <- da_profit[,c(3,9,67,68,70)]
colnames(da_profit_5) <- c("SecCode","TDate","eps","roe","opps")
save(da_profit_5,file="C:/Users/Ding/Desktop/da_profit_5.RData")
rm(list=ls())

now()
da_profit <- NULL
for (i in 2014:2015) {   
  for (j in 1:12) {
    for (k in 1:2) {
      path <- paste0("D:/daily_all/",i,"/",i,"_",j,"_",k,".csv")
      da_profit <- rbind(da_profit,read.csv(path,header=T))
    }
  }
}
da_profit_6 <- da_profit[,c(3,9,67,68,70)]
colnames(da_profit_6) <- c("SecCode","TDate","eps","roe","opps")
save(da_profit_6,file="C:/Users/Ding/Desktop/da_profit_6.RData")
rm(list=ls())

now()
da_profit <- NULL
for (i in 2016:2017) {   
  for (j in 1:12) {
    for (k in 1:2) {
      path <- paste0("D:/daily_all/",i,"/",i,"_",j,"_",k,".csv")
      da_profit <- rbind(da_profit,read.csv(path,header=T))
    }
  }
}
da_profit_7 <- da_profit[,c(3,9,67,68,70)]
colnames(da_profit_7) <- c("SecCode","TDate","eps","roe","opps")
save(da_profit_7,file="C:/Users/Ding/Desktop/da_profit_7.RData")
rm(list=ls())
now()

#################################
#### Combine the Daily Data
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_profit_1.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_profit_2.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_profit_3.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_profit_4.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_profit_5.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_profit_6.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_profit_7.RData")

da_profit <- as.data.table(rbind(da_profit_1,da_profit_2,da_profit_3,da_profit_4,
                                 da_profit_5,da_profit_6,da_profit_7))
da_profit <- da_profit[(SecCode>=600000 & SecCode<604000) | SecCode<2000 | 
                         (SecCode>2000 & SecCode<3000) | (SecCode>300000 & SecCode<301000),]

da_profit[,TDate:=ymd(TDate)]
da_profit[,ym:=ymd(paste0(lubridate::year(TDate),"-",
                          lubridate::month(TDate),"-01"))]

da_profit <- da_profit[,.(ym,SecCode,eps,roe,opps)]
da_profit_m <- da_profit[,.(eps=mean(eps,na.rm=T),roe=mean(roe,na.rm=T),
                            opps=mean(opps,na.rm=T)),by=.(ym,SecCode)]

save(da_profit_m,file="C:/Users/Ding/Desktop/da_profit_m.RData")

#################################
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_profit_m.RData")

da_profit_m[,`:=`(percentile_eps=1-percent_rank(eps),percentile_roe=1-percent_rank(roe),
                  percentile_opps=1-percent_rank(opps)),by=ym]

# Note: percent_rank() is different from rank()/(.N-1) when there exists NA.

da_percentile_profit_m <- da_profit_m[,.(ym,SecCode,percentile_eps,
                                         percentile_roe,percentile_opps)]

da_percentile_profit_m[,percentile_profit:=mean(c(percentile_eps,percentile_roe,
                                                  percentile_opps),na.rm=T),
                       by=.(ym,SecCode)]
da_percentile_profit_m <- na.omit(da_percentile_profit_m[,.(ym,SecCode,percentile_profit)])

save(da_percentile_profit_m,file="C:/Users/Ding/Desktop/da_percentile_profit_m.RData")

