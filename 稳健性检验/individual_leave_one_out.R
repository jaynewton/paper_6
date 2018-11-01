#################################
#### Exclude Institution
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_price_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_ivol_6m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_size_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_profit_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_bm_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_beta_5y.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_beta_y.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_turnover_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_dividend_m.RData")

da_percentile_ivol_6m <- da_percentile_ivol_6m[,.(ym,SecCode,percentile_ivol)]

da_individual_m <- merge(da_percentile_price_m,da_percentile_ivol_6m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_size_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_profit_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_bm_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_beta_5y,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_turnover_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_dividend_m,by=c("ym","SecCode"),all=T)

da_na <- as.data.table(which(is.na(da_individual_m),arr.ind=T))
row_na <- da_na[,.N,by=row][(8-N)<5,row] # rows with more than 8-5 NAs

da_individual_m <- da_individual_m[!row_na,]
da_individual_m[,individual:=
                  mean(c(percentile_price,percentile_ivol,
                         percentile_size,percentile_profit,percentile_BM,
                         percentile_be,percentile_turnover,percentile_dividend),na.rm=T),
                by=.(ym,SecCode)]

da_individual_m <- da_individual_m[,.(ym,SecCode,individual)]
da_individual_m[,group_individual:=ifelse(individual<=quantile(individual,0.2),1,
                                          ifelse(individual<=quantile(individual,0.4),2,
                                                 ifelse(individual<=quantile(individual,0.6),3,
                                                        ifelse(individual<=quantile(individual,0.8),4,
                                                               5)))),by=ym]

da_individual_m[,port_1:=ifelse(group_individual==1,1,0),by=ym] # port denotes portfolio
da_individual_m[,port_2:=ifelse(group_individual==2,1,0),by=ym] 
da_individual_m[,port_3:=ifelse(group_individual==3,1,0),by=ym] 
da_individual_m[,port_4:=ifelse(group_individual==4,1,0),by=ym] 
da_individual_m[,port_5:=ifelse(group_individual==5,1,0),by=ym] 

da_individual_m_1 <- da_individual_m

save(da_individual_m_1,file="C:/Users/Ding/Desktop/da_individual_m_1.RData")

#################################
#### Exclude Price
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_inst_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_ivol_6m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_size_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_profit_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_bm_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_beta_5y.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_beta_y.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_turnover_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_dividend_m.RData")

da_percentile_ivol_6m <- da_percentile_ivol_6m[,.(ym,SecCode,percentile_ivol)]

da_individual_m <- merge(da_percentile_inst_m,da_percentile_ivol_6m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_size_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_profit_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_bm_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_beta_5y,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_turnover_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_dividend_m,by=c("ym","SecCode"),all=T)

da_na <- as.data.table(which(is.na(da_individual_m),arr.ind=T))
row_na <- da_na[,.N,by=row][(8-N)<5,row] # rows with more than 8-5 NAs

da_individual_m <- da_individual_m[!row_na,]
da_individual_m[,individual:=
                  mean(c(percentile_inst,percentile_ivol,
                         percentile_size,percentile_profit,percentile_BM,
                         percentile_be,percentile_turnover,percentile_dividend),na.rm=T),
                by=.(ym,SecCode)]

da_individual_m <- da_individual_m[,.(ym,SecCode,individual)]
da_individual_m[,group_individual:=ifelse(individual<=quantile(individual,0.2),1,
                                          ifelse(individual<=quantile(individual,0.4),2,
                                                 ifelse(individual<=quantile(individual,0.6),3,
                                                        ifelse(individual<=quantile(individual,0.8),4,
                                                               5)))),by=ym]

da_individual_m[,port_1:=ifelse(group_individual==1,1,0),by=ym] # port denotes portfolio
da_individual_m[,port_2:=ifelse(group_individual==2,1,0),by=ym] 
da_individual_m[,port_3:=ifelse(group_individual==3,1,0),by=ym] 
da_individual_m[,port_4:=ifelse(group_individual==4,1,0),by=ym] 
da_individual_m[,port_5:=ifelse(group_individual==5,1,0),by=ym] 

da_individual_m_2 <- da_individual_m

save(da_individual_m_2,file="C:/Users/Ding/Desktop/da_individual_m_2.RData")

#################################
#### Exclude Idiosyncratic Volatility
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_inst_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_price_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_size_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_profit_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_bm_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_beta_5y.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_beta_y.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_turnover_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_dividend_m.RData")

da_individual_m <- merge(da_percentile_inst_m,da_percentile_price_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_size_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_profit_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_bm_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_beta_5y,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_turnover_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_dividend_m,by=c("ym","SecCode"),all=T)

da_na <- as.data.table(which(is.na(da_individual_m),arr.ind=T))
row_na <- da_na[,.N,by=row][(8-N)<5,row] # rows with more than 8-5 NAs

da_individual_m <- da_individual_m[!row_na,]
da_individual_m[,individual:=
                  mean(c(percentile_inst,percentile_price,
                         percentile_size,percentile_profit,percentile_BM,
                         percentile_be,percentile_turnover,percentile_dividend),na.rm=T),
                by=.(ym,SecCode)]

da_individual_m <- da_individual_m[,.(ym,SecCode,individual)]
da_individual_m[,group_individual:=ifelse(individual<=quantile(individual,0.2),1,
                                          ifelse(individual<=quantile(individual,0.4),2,
                                                 ifelse(individual<=quantile(individual,0.6),3,
                                                        ifelse(individual<=quantile(individual,0.8),4,
                                                               5)))),by=ym]

da_individual_m[,port_1:=ifelse(group_individual==1,1,0),by=ym] # port denotes portfolio
da_individual_m[,port_2:=ifelse(group_individual==2,1,0),by=ym] 
da_individual_m[,port_3:=ifelse(group_individual==3,1,0),by=ym] 
da_individual_m[,port_4:=ifelse(group_individual==4,1,0),by=ym] 
da_individual_m[,port_5:=ifelse(group_individual==5,1,0),by=ym] 

da_individual_m_3 <- da_individual_m

save(da_individual_m_3,file="C:/Users/Ding/Desktop/da_individual_m_3.RData")

#################################
#### Exclude Size
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_inst_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_price_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_ivol_6m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_profit_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_bm_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_beta_5y.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_beta_y.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_turnover_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_dividend_m.RData")

da_percentile_ivol_6m <- da_percentile_ivol_6m[,.(ym,SecCode,percentile_ivol)]

da_individual_m <- merge(da_percentile_inst_m,da_percentile_price_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_ivol_6m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_profit_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_bm_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_beta_5y,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_turnover_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_dividend_m,by=c("ym","SecCode"),all=T)

da_na <- as.data.table(which(is.na(da_individual_m),arr.ind=T))
row_na <- da_na[,.N,by=row][(8-N)<5,row] # rows with more than 8-5 NAs

da_individual_m <- da_individual_m[!row_na,]
da_individual_m[,individual:=
                  mean(c(percentile_inst,percentile_price,percentile_ivol,
                         percentile_profit,percentile_BM,
                         percentile_be,percentile_turnover,percentile_dividend),na.rm=T),
                by=.(ym,SecCode)]

da_individual_m <- da_individual_m[,.(ym,SecCode,individual)]
da_individual_m[,group_individual:=ifelse(individual<=quantile(individual,0.2),1,
                                          ifelse(individual<=quantile(individual,0.4),2,
                                                 ifelse(individual<=quantile(individual,0.6),3,
                                                        ifelse(individual<=quantile(individual,0.8),4,
                                                               5)))),by=ym]

da_individual_m[,port_1:=ifelse(group_individual==1,1,0),by=ym] # port denotes portfolio
da_individual_m[,port_2:=ifelse(group_individual==2,1,0),by=ym] 
da_individual_m[,port_3:=ifelse(group_individual==3,1,0),by=ym] 
da_individual_m[,port_4:=ifelse(group_individual==4,1,0),by=ym] 
da_individual_m[,port_5:=ifelse(group_individual==5,1,0),by=ym] 

da_individual_m_4 <- da_individual_m

save(da_individual_m_4,file="C:/Users/Ding/Desktop/da_individual_m_4.RData")

#################################
#### Exclude Profit
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_inst_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_price_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_ivol_6m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_size_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_bm_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_beta_5y.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_beta_y.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_turnover_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_dividend_m.RData")

da_percentile_ivol_6m <- da_percentile_ivol_6m[,.(ym,SecCode,percentile_ivol)]

da_individual_m <- merge(da_percentile_inst_m,da_percentile_price_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_ivol_6m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_size_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_bm_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_beta_5y,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_turnover_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_dividend_m,by=c("ym","SecCode"),all=T)

da_na <- as.data.table(which(is.na(da_individual_m),arr.ind=T))
row_na <- da_na[,.N,by=row][(8-N)<5,row] # rows with more than 8-5 NAs

da_individual_m <- da_individual_m[!row_na,]
da_individual_m[,individual:=
                  mean(c(percentile_inst,percentile_price,percentile_ivol,
                         percentile_size,percentile_BM,
                         percentile_be,percentile_turnover,percentile_dividend),na.rm=T),
                by=.(ym,SecCode)]

da_individual_m <- da_individual_m[,.(ym,SecCode,individual)]
da_individual_m[,group_individual:=ifelse(individual<=quantile(individual,0.2),1,
                                          ifelse(individual<=quantile(individual,0.4),2,
                                                 ifelse(individual<=quantile(individual,0.6),3,
                                                        ifelse(individual<=quantile(individual,0.8),4,
                                                               5)))),by=ym]

da_individual_m[,port_1:=ifelse(group_individual==1,1,0),by=ym] # port denotes portfolio
da_individual_m[,port_2:=ifelse(group_individual==2,1,0),by=ym] 
da_individual_m[,port_3:=ifelse(group_individual==3,1,0),by=ym] 
da_individual_m[,port_4:=ifelse(group_individual==4,1,0),by=ym] 
da_individual_m[,port_5:=ifelse(group_individual==5,1,0),by=ym] 

da_individual_m_5 <- da_individual_m

save(da_individual_m_5,file="C:/Users/Ding/Desktop/da_individual_m_5.RData")

#################################
#### Exclude B/M Ratio
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_inst_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_price_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_ivol_6m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_size_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_profit_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_beta_5y.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_beta_y.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_turnover_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_dividend_m.RData")

da_percentile_ivol_6m <- da_percentile_ivol_6m[,.(ym,SecCode,percentile_ivol)]

da_individual_m <- merge(da_percentile_inst_m,da_percentile_price_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_ivol_6m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_size_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_profit_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_beta_5y,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_turnover_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_dividend_m,by=c("ym","SecCode"),all=T)

da_na <- as.data.table(which(is.na(da_individual_m),arr.ind=T))
row_na <- da_na[,.N,by=row][(8-N)<5,row] # rows with more than 8-5 NAs

da_individual_m <- da_individual_m[!row_na,]
da_individual_m[,individual:=
                  mean(c(percentile_inst,percentile_price,percentile_ivol,
                         percentile_size,percentile_profit,
                         percentile_be,percentile_turnover,percentile_dividend),na.rm=T),
                by=.(ym,SecCode)]

da_individual_m <- da_individual_m[,.(ym,SecCode,individual)]
da_individual_m[,group_individual:=ifelse(individual<=quantile(individual,0.2),1,
                                          ifelse(individual<=quantile(individual,0.4),2,
                                                 ifelse(individual<=quantile(individual,0.6),3,
                                                        ifelse(individual<=quantile(individual,0.8),4,
                                                               5)))),by=ym]

da_individual_m[,port_1:=ifelse(group_individual==1,1,0),by=ym] # port denotes portfolio
da_individual_m[,port_2:=ifelse(group_individual==2,1,0),by=ym] 
da_individual_m[,port_3:=ifelse(group_individual==3,1,0),by=ym] 
da_individual_m[,port_4:=ifelse(group_individual==4,1,0),by=ym] 
da_individual_m[,port_5:=ifelse(group_individual==5,1,0),by=ym] 

da_individual_m_6 <- da_individual_m

save(da_individual_m_6,file="C:/Users/Ding/Desktop/da_individual_m_6.RData")

#################################
#### Exclude Beta
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_inst_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_price_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_ivol_6m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_size_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_profit_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_bm_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_turnover_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_dividend_m.RData")

da_percentile_ivol_6m <- da_percentile_ivol_6m[,.(ym,SecCode,percentile_ivol)]

da_individual_m <- merge(da_percentile_inst_m,da_percentile_price_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_ivol_6m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_size_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_profit_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_bm_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_turnover_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_dividend_m,by=c("ym","SecCode"),all=T)

da_na <- as.data.table(which(is.na(da_individual_m),arr.ind=T))
row_na <- da_na[,.N,by=row][(8-N)<5,row] # rows with more than 8-5 NAs

da_individual_m <- da_individual_m[!row_na,]
da_individual_m[,individual:=
                  mean(c(percentile_inst,percentile_price,percentile_ivol,
                         percentile_size,percentile_profit,percentile_BM,
                         percentile_turnover,percentile_dividend),na.rm=T),
                by=.(ym,SecCode)]

da_individual_m <- da_individual_m[,.(ym,SecCode,individual)]
da_individual_m[,group_individual:=ifelse(individual<=quantile(individual,0.2),1,
                                          ifelse(individual<=quantile(individual,0.4),2,
                                                 ifelse(individual<=quantile(individual,0.6),3,
                                                        ifelse(individual<=quantile(individual,0.8),4,
                                                               5)))),by=ym]

da_individual_m[,port_1:=ifelse(group_individual==1,1,0),by=ym] # port denotes portfolio
da_individual_m[,port_2:=ifelse(group_individual==2,1,0),by=ym] 
da_individual_m[,port_3:=ifelse(group_individual==3,1,0),by=ym] 
da_individual_m[,port_4:=ifelse(group_individual==4,1,0),by=ym] 
da_individual_m[,port_5:=ifelse(group_individual==5,1,0),by=ym] 

da_individual_m_7 <- da_individual_m

save(da_individual_m_7,file="C:/Users/Ding/Desktop/da_individual_m_7.RData")

#################################
#### Exclude Turnover
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_inst_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_price_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_ivol_6m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_size_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_profit_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_bm_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_beta_5y.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_beta_y.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_dividend_m.RData")

da_percentile_ivol_6m <- da_percentile_ivol_6m[,.(ym,SecCode,percentile_ivol)]

da_individual_m <- merge(da_percentile_inst_m,da_percentile_price_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_ivol_6m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_size_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_profit_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_bm_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_beta_5y,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_dividend_m,by=c("ym","SecCode"),all=T)

da_na <- as.data.table(which(is.na(da_individual_m),arr.ind=T))
row_na <- da_na[,.N,by=row][(8-N)<5,row] # rows with more than 8-5 NAs

da_individual_m <- da_individual_m[!row_na,]
da_individual_m[,individual:=
                  mean(c(percentile_inst,percentile_price,percentile_ivol,
                         percentile_size,percentile_profit,percentile_BM,
                         percentile_be,percentile_dividend),na.rm=T),
                by=.(ym,SecCode)]

da_individual_m <- da_individual_m[,.(ym,SecCode,individual)]
da_individual_m[,group_individual:=ifelse(individual<=quantile(individual,0.2),1,
                                          ifelse(individual<=quantile(individual,0.4),2,
                                                 ifelse(individual<=quantile(individual,0.6),3,
                                                        ifelse(individual<=quantile(individual,0.8),4,
                                                               5)))),by=ym]

da_individual_m[,port_1:=ifelse(group_individual==1,1,0),by=ym] # port denotes portfolio
da_individual_m[,port_2:=ifelse(group_individual==2,1,0),by=ym] 
da_individual_m[,port_3:=ifelse(group_individual==3,1,0),by=ym] 
da_individual_m[,port_4:=ifelse(group_individual==4,1,0),by=ym] 
da_individual_m[,port_5:=ifelse(group_individual==5,1,0),by=ym] 

da_individual_m_8 <- da_individual_m

save(da_individual_m_8,file="C:/Users/Ding/Desktop/da_individual_m_8.RData")

#################################
#### Exclude Dividend
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_inst_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_price_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_ivol_6m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_size_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_profit_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_bm_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_beta_5y.RData")
#load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_beta_y.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_percentile_turnover_m.RData")

da_percentile_ivol_6m <- da_percentile_ivol_6m[,.(ym,SecCode,percentile_ivol)]

da_individual_m <- merge(da_percentile_inst_m,da_percentile_price_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_ivol_6m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_size_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_profit_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_bm_m,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_beta_5y,by=c("ym","SecCode"),all=T)
da_individual_m <- merge(da_individual_m,da_percentile_turnover_m,by=c("ym","SecCode"),all=T)

da_na <- as.data.table(which(is.na(da_individual_m),arr.ind=T))
row_na <- da_na[,.N,by=row][(8-N)<5,row] # rows with more than 8-5 NAs

da_individual_m <- da_individual_m[!row_na,]
da_individual_m[,individual:=
                  mean(c(percentile_inst,percentile_price,percentile_ivol,
                         percentile_size,percentile_profit,percentile_BM,
                         percentile_be,percentile_turnover),na.rm=T),
                by=.(ym,SecCode)]

da_individual_m <- da_individual_m[,.(ym,SecCode,individual)]
da_individual_m[,group_individual:=ifelse(individual<=quantile(individual,0.2),1,
                                          ifelse(individual<=quantile(individual,0.4),2,
                                                 ifelse(individual<=quantile(individual,0.6),3,
                                                        ifelse(individual<=quantile(individual,0.8),4,
                                                               5)))),by=ym]

da_individual_m[,port_1:=ifelse(group_individual==1,1,0),by=ym] # port denotes portfolio
da_individual_m[,port_2:=ifelse(group_individual==2,1,0),by=ym] 
da_individual_m[,port_3:=ifelse(group_individual==3,1,0),by=ym] 
da_individual_m[,port_4:=ifelse(group_individual==4,1,0),by=ym] 
da_individual_m[,port_5:=ifelse(group_individual==5,1,0),by=ym] 

da_individual_m_9 <- da_individual_m

save(da_individual_m_9,file="C:/Users/Ding/Desktop/da_individual_m_9.RData")

