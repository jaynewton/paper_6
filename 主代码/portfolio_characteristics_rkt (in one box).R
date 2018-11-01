#################################
load("F:/我的论文/第五篇/RData/da_all_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_individual_m.RData")
load("F:/我的论文/第五篇/RData/da_inst_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_price_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_ivol_6m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_size_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_profit_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_bm_m.RData")
load("F:/我的论文/第五篇/主代码/beta anomaly/monthly data in five years/RData/da_beta_5y.RData")
#load("F:/我的论文/第五篇/主代码/beta anomaly/daily data in one year/RData/da_beta_y.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_turnover_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_dividend_m.RData")

da_all_m <- da_all_m[,.(ym,SecCode,max_ret)]
da_individual_m <- da_individual_m[,.(ym,SecCode,individual)]
da_ivol_6m <- da_ivol_6m[,.(ym,SecCode,ivol)]
da_profit_m <- na.omit(da_profit_m)

da_m <- merge(da_all_m,da_individual_m,by=c("ym","SecCode"))
da_m <- merge(da_m,da_inst_m,by=c("ym","SecCode"))
da_m <- merge(da_m,da_price_m,by=c("ym","SecCode"))
da_m <- merge(da_m,da_ivol_6m,by=c("ym","SecCode"))
da_m <- merge(da_m,da_size_m,by=c("ym","SecCode"))
da_m <- merge(da_m,da_profit_m,by=c("ym","SecCode"))
da_m <- merge(da_m,da_bm_m,by=c("ym","SecCode"))
da_m <- merge(da_m,da_beta_5y,by=c("ym","SecCode"))
da_m <- merge(da_m,da_turnover_m,by=c("ym","SecCode"))
da_m <- merge(da_m,da_dividend_m,by=c("ym","SecCode"))

####
ym_index <- sort(unique(da_m$ym))
k <- 5
y <- 13  # number of porfolio characteristics variables
ret_p <- matrix(NA,nrow=length(ym_index),ncol=k) # p denotes portfolio
colnames(ret_p) <- paste0("p",1:k)
vari_level <- array(NA,c(length(ym_index),k,y)) # vari_level denotes variable level
for (i in 1:length(ym_index)) {
  da_sub <- da_m[ym==ym_index[i],]
  da_sub <- da_sub[order(max_ret),]
  n_mid <- floor(nrow(da_sub)/k)
  if ((nrow(da_sub)-n_mid*(k-2))%%2==0){
    n_f <- (nrow(da_sub)-n_mid*(k-2))/2 # f denotes first, l denotes last
    n_l <- n_f
  } else {
    n_f <- (nrow(da_sub)-n_mid*(k-2)-1)/2
    n_l <- n_f+1
  }
  x <- seq(from=n_f,to=nrow(da_sub),by=n_mid)[1:(k-1)]
  x <- c(x,nrow(da_sub))
  da_sub$group_n <- cut(1:nrow(da_sub), c(0,x),labels = 1:k)
  for (j in 1:k) {
    vari_level[i,j,1] <- da_sub[group_n==j,mean(max_ret)]
    vari_level[i,j,2] <- da_sub[group_n==j,mean(individual)]
    vari_level[i,j,3] <- da_sub[group_n==j,mean(inst)]
    vari_level[i,j,4] <- da_sub[group_n==j,mean(price)]
    vari_level[i,j,5] <- da_sub[group_n==j,mean(ivol)]
    vari_level[i,j,6] <- da_sub[group_n==j,mean(size)]
    vari_level[i,j,7] <- da_sub[group_n==j,mean(eps)]
    vari_level[i,j,8] <- da_sub[group_n==j,mean(roe)]
    vari_level[i,j,9] <- da_sub[group_n==j,mean(opps)]
    vari_level[i,j,10] <- da_sub[group_n==j,mean(BM)]
    vari_level[i,j,11] <- da_sub[group_n==j,mean(be)]
    vari_level[i,j,12] <- da_sub[group_n==j,mean(turnover)]
    vari_level[i,j,13] <- da_sub[group_n==j,mean(dividend)]
  }
}
vari_level_m <- matrix(NA,nrow=k,ncol=y) # m denotes mean 
for (j in 1:k) {
  for (p in 1:y) {
    vari_level_m[j,p] <- mean(vari_level[,j,p],na.rm=T)
  }
}
colnames(vari_level_m) <- c("max_ret","individual","inst","price","ivol","size","eps",
                            "roe","opps","BM","be","turnover","dividend")
vari_level_m

