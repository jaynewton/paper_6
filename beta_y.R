#################################
load("F:/我的论文/第五篇/主代码/beta anomaly/daily data in one year/RData/da_beta_y.RData")

da_beta_y <- da_beta_y[ym>=ymd("2000-1-1"),]

da_beta_y[,percentile_be:=percent_rank(be),by=ym]
# Note: percent_rank() is different from rank()/(.N-1) when there exists NA.

da_percentile_beta_y <- da_beta_y[,.(ym,SecCode,percentile_be)]

save(da_percentile_beta_y,file="C:/Users/Ding/Desktop/da_percentile_beta_y.RData")
