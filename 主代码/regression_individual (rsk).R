#################################
load("F:/我的论文/第五篇/RData/da_all_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_individual_m.RData")
load("F:/我的论文/第五篇/RData/da_realized_m.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_ivol_6m.RData")
load("F:/我的论文/第五篇/RData/da_mom_m.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_cosk_5y.RData")

da_m_lm <- merge(da_all_m,da_individual_m,by=c("ym","SecCode"))
da_m_lm <- merge(da_m_lm,da_realized_m[,.(ym,SecCode,rsk)],by=c("ym","SecCode"))
da_m_lm <- merge(da_m_lm,da_beta_5y,by=c("ym","SecCode"))
da_m_lm <- merge(da_m_lm,da_ivol_6m,by=c("ym","SecCode"))
da_m_lm <- merge(da_m_lm,da_mom_m,by=c("ym","SecCode"))
da_m_lm <- merge(da_m_lm,da_cosk_5y,by=c("ym","SecCode"))

#names(da_m_lm)

da_m_lm[illiq!=0,`:=`(size=log(size),BM=log(BM),illiq=log(illiq),ivol=log(ivol))]
da_m_lm <- da_m_lm[order(ym,SecCode),]

#### The Regression Part
lm_fit_nw <- function(model,nv) { # nw denotes Newey-West HAC t statistic
  # nv denotes numbers of variables
  lm_sta <- matrix(NA,nrow=nv+1,ncol=2) # sta denotes statistics
  for (i in 1:(nv+1)) {
    model_nw <- lm(summary(model)$coefficients[,,i][,1] ~ 1)
    lm_sta[i,1] <- coeftest(model_nw,vcov = NeweyWest(model_nw))[1,1]
    lm_sta[i,2] <- coeftest(model_nw,vcov = NeweyWest(model_nw))[1,3]
  }
  print(lm_sta)
  print(mean(as.numeric(unlist(summary(model)$adj.r.squared))))
}

####  
lm_1 <- lmList(ret_e ~ I(rsk*port_1)+I(rsk*port_2)+I(rsk*port_3)
               +I(rsk*port_4)+I(rsk*port_5)| ym , data=da_m_lm)
lm_fit_nw(lm_1,5)

lm_2 <- lmList(ret_e ~ individual| ym , data=da_m_lm)
lm_fit_nw(lm_2,1)

lm_3 <- lmList(ret_e ~ I(rsk*port_1)+I(rsk*port_2)+I(rsk*port_3)
               +I(rsk*port_4)+I(rsk*port_5)+individual| ym , data=da_m_lm)
lm_fit_nw(lm_3,6)

lm_4 <- lmList(ret_e ~ I(rsk*port_1)+I(rsk*port_2)+I(rsk*port_3)
               +I(rsk*port_4)+I(rsk*port_5)+individual+be+size+BM| ym , data=da_m_lm)
lm_fit_nw(lm_4,9)

lm_5 <- lmList(ret_e ~ I(rsk*port_1)+I(rsk*port_2)+I(rsk*port_3)
               +I(rsk*port_4)+I(rsk*port_5)+individual+be+size+BM+mom| ym , data=da_m_lm)
lm_fit_nw(lm_5,10)

lm_6 <- lmList(ret_e ~ I(rsk*port_1)+I(rsk*port_2)+I(rsk*port_3)
               +I(rsk*port_4)+I(rsk*port_5)+individual+be+size+BM+mom+ret_tm+illiq| ym , data=da_m_lm)
lm_fit_nw(lm_6,12)

lm_7 <- lmList(ret_e ~ I(rsk*port_1)+I(rsk*port_2)+I(rsk*port_3)
               +I(rsk*port_4)+I(rsk*port_5)+individual+be+size+BM+mom+ret_tm+illiq+ivol+isk+cosk_2| ym , data=da_m_lm)
lm_fit_nw(lm_7,15)



