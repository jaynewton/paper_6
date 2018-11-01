#################################
#### Independent Double Sort

#### Case1: k_1 is equal to k_2 
load("F:/我的论文/第五篇/RData/da_all_m.RData")
load("F:/我的论文/第五篇/主代码/individual investor preference/RData/da_individual_m.RData")
load("F:/我的论文/第五篇/RData/da_realized_m.RData")
load("F:/我的论文/第五篇/RData/FF3F_A_nm.RData")

da_all_m <- da_all_m[,.(ym,SecCode,ret_e,size)]
da_individual_m <- da_individual_m[,.(ym,SecCode,group_individual)]
da_m <- merge(da_all_m,da_individual_m,by=c("ym","SecCode")) 
da_m <- merge(da_m,da_realized_m,by=c("ym","SecCode")) 

da_m[,group_rsk:=ifelse(rsk<=quantile(rsk,0.2),1,
                        ifelse(rsk<=quantile(rsk,0.4),2,
                               ifelse(rsk<=quantile(rsk,0.6),3,
                                      ifelse(rsk<=quantile(rsk,0.8),4,
                                             5)))),by=ym]

####
ym_index <- sort(unique(da_m$ym))
k <- 5 # 5*5 portfolios
ret_p <- array(NA,c(length(ym_index),k,k)) # p denotes portfolio
# the first k corresponds to the number of groups of variable of interest
# the second k corresponds to the number of groups of control variable
# i,p and j corresponds to 1:length(ym_index), the first k and the second k below

#### Method 1: use data.table
# the speed is quite fast
for (i in 1:length(ym_index)) {
  da_ret_e <- da_m[ym==ym_index[i],.(ret_e=mean(ret_e)),keyby=.(group_rsk,group_individual)]
  #da_ret_e <- da_m[ym==ym_index[i],.(ret_e=weighted.mean(ret_e,size)),keyby=.(group_rsk,group_individual)]
  ret_p[i,,] <- as.matrix(dcast(da_ret_e, group_rsk ~ group_individual, value.var="ret_e")[,-"group_rsk"])
}

#### Method 2: use three-layer loop structure
# direct but low-speed
for (i in 1:length(ym_index)) {
  for (p in 1:k) {
    for (j in 1:k) {
      ret_p[i,p,j] <- da_m[ym==ym_index[i] & group_rsk==p & group_individual==j,mean(ret_e)]
      #ret_p[i,p,j] <- da_m[ym==ym_index[i] & group_rsk==p & group_individual==j,weighted.mean(ret_e,size)]
    }
  }
}

ret_p_m <- matrix(NA,nrow=k+1,ncol=k+1)
colnames(ret_p_m) <- c(paste0("cv",1:k),"average") # cv denotes control variables
rownames(ret_p_m) <- c(paste0("voi",1:k),"h-l") # voi denotes variables of interest, here it's rsj
for (p in 1:k) {
  for (j in 1:k) {
    ret_p_m[p,j] <- mean(ret_p[,p,j],na.rm=T) # full sample
  }
}

ret_p_m[1:k,k+1] <- rowMeans(ret_p_m[1:k,1:k])
ret_p_m[k+1,] <- ret_p_m[k,]-ret_p_m[1,]
ret_p_m

#### Newey-West t statistic
t_nm <- vector(length=k+1)

## For the first five t values
ret_p_hl <- matrix(NA,nrow=length(ym_index),ncol=k)
ret_p_hl_sd <- vector(length=k)
for (j in 1:k) {
  ret_p_hl[,j] <- ret_p[,k,j]-ret_p[,1,j]
  ret_p_hl_sd[j] <- sd(ret_p_hl[,j],na.rm=T)
}
for (j in 1:k) {
  model_nm <- lm(ret_p_hl[,j] ~ 1)
  t_nm[j] <- coeftest(model_nm,vcov = NeweyWest(model_nm))[1,3]
}

## For the sixth t value
ret_p_hl_average <- rowMeans(ret_p[,k,1:k],na.rm=T)-rowMeans(ret_p[,1,1:k],na.rm=T)
model_nm <- lm(ret_p_hl_average ~ 1)
t_nm[k+1] <- coeftest(model_nm,vcov = NeweyWest(model_nm))[1,3]
t_nm

#### adjusted by FF3F
ret_p_hl <- cbind(ret_p_hl,ret_p_hl_average)
ret_p_hl <- as.data.table(ret_p_hl)
names(ret_p_hl) <- c(paste0("p",1:k),"average")
ret_p_hl$ym <- sort(unique(da_m$ym))
ret_p_hl <- merge(ret_p_hl,FF3F_A_nm,by="ym")

## Newey-West t statistic
ret_p_hl_FF3F <- matrix(NA,nrow=2,ncol=k+1) 
for (j in 1:(k+1)) { # the first column is the corresponding ym
  model_FF3F <- lm(ret_p_hl[[j+1]]~ret_p_hl[["mkt_e"]]+ret_p_hl[["smb"]]+ret_p_hl[["hml"]])
  ret_p_hl_FF3F[1,j] <- coeftest(model_FF3F,vcov=NeweyWest(model_FF3F))[1,1]
  ret_p_hl_FF3F[2,j] <- coeftest(model_FF3F,vcov=NeweyWest(model_FF3F))[1,3]
}
ret_p_hl_FF3F

