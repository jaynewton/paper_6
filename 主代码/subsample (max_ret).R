#################################
#### Independent Double Sort
load("F:/我的论文/第五篇/RData/da_all_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_individual_m.RData")
load("F:/我的论文/第五篇/RData/FF3F_A_nm.RData")
load("F:/我的论文/第五篇/RData/da_umd_nm.RData")
load("F:/我的论文/第五篇/RData/da_iml_nm.RData")

da_individual_m <- da_individual_m[,.(ym,SecCode,individual)]

FUN_TOP <- function(vari,top_percentile) {
  group_top <- ifelse(vari>=quantile(vari,(100-top_percentile)/100),1,0)
  return(group_top)
}

for (i in seq(5,95,by=5)) {
  da_individual_m[,`:=`(paste0("top_",i),FUN_TOP(individual,i)),by=ym]
}

da_all_m <- da_all_m[,.(ym,SecCode,max_ret,ret_e,size)]
da_individual_m <- merge(da_all_m,da_individual_m,by=c("ym","SecCode")) 

####
for (i in seq(5,95,by=5)) {
  print(i)
  print(da_individual_m[,.(percentage_size=1-sum(size*get(paste0("top_",i)))/sum(size)),by=ym]
        [,mean(percentage_size)])
  cat('\n')
}

####
da_m <- da_individual_m[top_5!=1,]
da_m <- da_individual_m[top_10!=1,]
da_m <- da_individual_m[top_15!=1,]
da_m <- da_individual_m[top_20!=1,]
da_m <- da_individual_m[top_25!=1,]
da_m <- da_individual_m[top_30!=1,]
da_m <- da_individual_m[top_35!=1,]
da_m <- da_individual_m[top_40!=1,]
da_m <- da_individual_m[top_45!=1,]
da_m <- da_individual_m[top_50!=1,]
da_m <- da_individual_m[top_55!=1,]
da_m <- da_individual_m[top_60!=1,]
da_m <- da_individual_m[top_65!=1,]
da_m <- da_individual_m[top_70!=1,]
da_m <- da_individual_m[top_75!=1,]
da_m <- da_individual_m[top_80!=1,]
da_m <- da_individual_m[top_85!=1,]
da_m <- da_individual_m[top_90!=1,]
da_m <- da_individual_m[top_95!=1,]

####
ym_index <- sort(unique(da_m$ym))
k <- 5
ret_p <- matrix(NA,nrow=length(ym_index),ncol=k) # p denotes portfolio
colnames(ret_p) <- paste0("p",1:k)
vari_level <- matrix(NA,nrow=length(ym_index),ncol=k) # vari_level denotes variable level
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
    ret_p[i,j] <- da_sub[group_n==j,mean(ret_e)]
    # This part we do not need to take risk-free rate into consideration 
    #ret_p[i,j] <- da_sub[group_n==j,weighted.mean(ret_e,size)]
    vari_level[i,j] <- da_sub[group_n==j,mean(max_ret)]
    #vari_level[i,j] <- da_sub[group_n==j,weighted.mean(max_ret,size)]
  }
}

#### the part below calculate the performance of the whole sample period 
ret_p_m <- colMeans(ret_p,na.rm=T) # m denotes mean # full sample 
vari_level_m <- colMeans(vari_level,na.rm=T)
output <- cbind(vari_level_m,ret_p_m) 
output
result <- c(output[,2],hml=output[k,2]-output[1,2])
result

## Newey-West t statistic
ret_p_hl <- ret_p[,k]-ret_p[,1] # ret_p_hl denotes high minus low portfolio returns
model_nw <- lm(ret_p_hl ~ 1)
coeftest(model_nw,vcov = NeweyWest(model_nw))[1,3]

#################################
#### Adjusted by FF3F
ret_p <- as.data.table(na.omit(ret_p))
ret_p$ym <- sort(unique(da_m$ym))
ret_p <- merge(ret_p,FF3F_A_nm,by="ym") 

## Newey-West t statistic
ret_p_FF3F <- matrix(NA,nrow=2,ncol=k+1) 
for (i in 1:k) { # the first column is the ym
  model_FF3F <- lm(ret_p[[i+1]]~ret_p[,mkt_e]+ret_p[,smb]+ret_p[,hml]) 
  # ret[,i+1] is wrong. See 1.5 of Frequently Asked Questions about data.table
  ret_p_FF3F[1,i] <- coeftest(model_FF3F,vcov=NeweyWest(model_FF3F))[1,1]
  ret_p_FF3F[2,i] <- coeftest(model_FF3F,vcov=NeweyWest(model_FF3F))[1,3]
}
ret_p_hl <- ret_p[[k+1]]-ret_p[[2]] 
model_FF3F <- lm(ret_p_hl~ret_p[,mkt_e]+ret_p[,smb]+ret_p[,hml])
ret_p_FF3F[1,k+1] <- coeftest(model_FF3F,vcov=NeweyWest(model_FF3F))[1,1]
ret_p_FF3F[2,k+1] <- coeftest(model_FF3F,vcov=NeweyWest(model_FF3F))[1,3]
ret_p_FF3F

#################################
#### Adjusted by FF3F+UMD+IML
#ret_p <- as.data.table(na.omit(ret_p))
#ret_p$ym <- sort(unique(da_m$ym))
#ret_p <- merge(ret_p,FF3F_A_nm,by="ym") 
ret_p <- merge(ret_p,da_umd_nm,by="ym")
ret_p <- merge(ret_p,da_iml_nm,by="ym")

## Newey-West t statistic
ret_p_adj <- matrix(NA,nrow=2,ncol=k+1) 
for (i in 1:k) { # the first column is the ym
  model_adj <- lm(ret_p[[i+1]]~ret_p[,mkt_e]+ret_p[,smb]+ret_p[,hml]+ret_p[,umd]+ret_p[,iml]) 
  # ret[,i+1] is wrong. See 1.5 of Frequently Asked Questions about data.table
  ret_p_adj[1,i] <- coeftest(model_adj,vcov=NeweyWest(model_adj))[1,1]
  ret_p_adj[2,i] <- coeftest(model_adj,vcov=NeweyWest(model_adj))[1,3]
}
ret_p_hl <- ret_p[[k+1]]-ret_p[[2]] 
model_adj <- lm(ret_p_hl~ret_p[,mkt_e]+ret_p[,smb]+ret_p[,hml]+ret_p[,umd]+ret_p[,iml])
ret_p_adj[1,k+1] <- coeftest(model_adj,vcov=NeweyWest(model_adj))[1,1]
ret_p_adj[2,k+1] <- coeftest(model_adj,vcov=NeweyWest(model_adj))[1,3]
ret_p_adj

