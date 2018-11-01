#################################
#### Single Sort
#### Not Adjusted by FF3F
load("F:/我的论文/第五篇/RData/da_all_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_dividend_m.RData")
load("F:/我的论文/第五篇/RData/FF3F_A_nm.RData")

da_m <- merge(da_all_m[,.(ym,SecCode,ret_e,size)],da_dividend_m,by=c("ym","SecCode"))

da_m_1 <- da_m[dividend==1,]
da_ret_m_1 <- da_m_1[,.(ret_e_m_1=mean(ret_e)),by=.(dividend,ym)]
da_ret_m_1[,dividend:=NULL]

da_m_2 <- da_m[dividend==0,]
da_ret_m_2 <- da_m_2[,.(ret_e_m_2=mean(ret_e)),by=.(dividend,ym)]
da_ret_m_2[,dividend:=NULL]

da_ret_m <- merge(da_ret_m_1,da_ret_m_2,by="ym")
da_ret_m[,ret_e_m_3:=ret_e_m_1-ret_e_m_2]
da_ret_m[,.(ret_dividend=mean(ret_e_m_1),
            ret_no_dividend=mean(ret_e_m_2),
            ret_diff=mean(ret_e_m_3))]

model_1 <- da_ret_m[,lm(ret_e_m_1~1)]
coeftest(model_1,vcov=NeweyWest(model_1))[1,3]

model_2 <- da_ret_m[,lm(ret_e_m_2~1)]
coeftest(model_2,vcov=NeweyWest(model_2))[1,3]

model_3 <- da_ret_m[,lm(ret_e_m_3~1)]
coeftest(model_3,vcov=NeweyWest(model_3))[1,3]

#### Adjusted by FF3F
da_ret_m <- merge(da_ret_m,FF3F_A_nm,by="ym")

model_1_FF3F <- da_ret_m[,lm(ret_e_m_1~mkt_e+smb+hml)]
coeftest(model_1_FF3F,vcov=NeweyWest(model_1_FF3F))[1,c(1,3)]

model_2_FF3F <- da_ret_m[,lm(ret_e_m_2~mkt_e+smb+hml)]
coeftest(model_2_FF3F,vcov=NeweyWest(model_2_FF3F))[1,c(1,3)]

model_3_FF3F <- da_ret_m[,lm(ret_e_m_3~mkt_e+smb+hml)]
coeftest(model_3_FF3F,vcov=NeweyWest(model_3_FF3F))[1,c(1,3)]
