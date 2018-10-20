#################################
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")

da_beta_5y <- da_beta_5y[ym>=ymd("2000-1-1"),]

da_beta_5y[,percentile_be:=percent_rank(be),by=ym]
# Note: percent_rank() is different from rank()/(.N-1) when there exists NA.

da_percentile_beta_5y <- da_beta_5y[,.(ym,SecCode,percentile_be)]

save(da_percentile_beta_5y,file="C:/Users/Ding/Desktop/da_percentile_beta_5y.RData")








