#################################
load("F:/我的论文/第五篇/RData/da_inst_m.RData")

da_inst_m <- da_inst_m[(SecCode>=600000 & SecCode<604000) | SecCode<2000 | 
                                 (SecCode>2000 & SecCode<3000) | 
                                 (SecCode>300000 & SecCode<301000),]

da_inst_m[,percentile_inst:=1-percent_rank(inst),by=ym]
# Note: percent_rank() is different from rank()/(.N-1) when there exists NA.

da_percentile_inst_m <- da_inst_m[,.(ym,SecCode,percentile_inst)]

save(da_percentile_inst_m,file="C:/Users/Ding/Desktop/da_percentile_inst_m.RData")



