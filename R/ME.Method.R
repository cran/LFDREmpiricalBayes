
##### 26-11-2016

########################################################
log.likelihood<-function(x,p0.hat,ncp.hat){
  (sum(log(p0.hat*dchisq(x,df=1,ncp=0)+(1-p0.hat)*dchisq(x,df=1,ncp=ncp.hat))))
}
########################################################
log.likelihood1<-function(p0,ncp,stat){
  log.sum<-0
  for ( i in 1:length(stat)){
    log1<-log(p0*dchisq(stat[i],df=1,ncp=0)+(1-p0)*dchisq(stat[i],df=1,ncp=ncp))
    log.sum<-log.sum+log1
  }
  return(log.sum)
}
########################################################
true.lfdr1<-function(stat,pi0,ncp){
  true.lfdrs<-c()
  for (i in 1:length(pi0)){
    f0<-dchisq(stat, df=1, ncp = 0)
    f1<-dchisq(stat, df=1, ncp = ncp[i])
    true<-(pi0[i]*f0)/((pi0[i]*f0)+(1-pi0[i])*f1)
    true.lfdrs<-rbind(true.lfdrs,true)
  }
  return(true.lfdrs)
}
########################################################
ME.log<-function(stat,lfdr.C,p0.C,ncp.C,p0.S,ncp.S,a=3,lower.p0=0,upper.p0=1,lower.ncp=0.1,upper.ncp=50,length.p0=200,length.ncp=200){
  if (log.likelihood(stat,p0.C,ncp.C)-log.likelihood(stat,p0.S,ncp.S) > -(a*log(2))) {
    LFDR.ME<-lfdr.C
    opt.vect<-cbind(p0.C,ncp.C)
    colnames(opt.vect)<-c('p0','ncp')
    results<-list(p0.hat=opt.vect[,'p0'],ncp.hat=opt.vect[,'ncp'],LFDR.hat=LFDR.ME)
  }else{
    p0<-seq(lower.p0,upper.p0,length=length.p0)
    ncp<-seq(lower.ncp,upper.ncp,length=length.ncp)
    #outer(p0,ncp)
    like1<- outer(p0,ncp, function(x,y) log.likelihood1(x,y,stat))
    plausible<-which(like1-log.likelihood1(p0.S,ncp.S,stat)+a*log(2)>0, arr.ind = TRUE)
    #like1[plausible]
    p0<-p0[plausible[,1]]
    ncp<-ncp[plausible[,2]]
    #plot(1:1000,p0[1:1000])
    #plot(ncp)
    #y<-true.lfdr1(stat,p0,ncp)
    #cbind(colMins(y),colMaxs(y))
    ls<-length(stat)
    ME<-rep(NA,ls)
    p0.ME<-rep(NA,ls)
    ncp.ME<-rep(NA,ls)
    y<-true.lfdr1(stat,p0,ncp)
    #dim(y)
    cminy<-colMins(y)
    cmaxy<-colMaxs(y)
    for ( i in 1:ls){
      if (lfdr.C[i]<cminy[i]) {
        ME[i]<-cminy[i]
        wpc1<-which(y[,i]==cminy[i])
        p0.ME[i]<-p0[wpc1]
        ncp.ME[i]<-ncp[wpc1]
      }else if (lfdr.C[i]>cmaxy[i]) {
        ME[i]<-cmaxy[i]
        wpc2<-which(y[,i]==cmaxy[i])
        p0.ME[i]<-p0[wpc2]
        ncp.ME[i]<-ncp[wpc2]
      }else{
        ME[i]<-lfdr.C[i]
        p0.ME[i]<-p0.C
        ncp.ME[i]<-ncp.C
      }
    }

    results<-list(p0.hat=p0.ME,ncp.hat=ncp.ME,LFDR.hat=ME)
  }
  return(results)
}
########################################################
