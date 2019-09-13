#################################################################################
##
## Author:  Nat Goodman
## Created: 19-09-09
##          from misig/data_confi.R created 19-07-16
##          from misig/confi.R created 19-07-04
##
## Copyright (C) 2019 Nat Goodman.
## 
## Generate data for effit document
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
library(nor1mix);
dat_effit=function(...) {
  init(doc='effit',...);
  param(n,m,prop.true,d0,mean.true,sd.true,mean.false,sd.false,tol,m1,mmax);
  n.lo=if(length(n)>2) sort(n)[1:2] else min(n); n.hi=max(n);
  prop.lo=min(prop.true); prop.hi=max(prop.true);
  ## normals - use norMix so same code will work for norm and mixture
  sim_effit('lo_norm',n=n.lo[1],m=m,d0=d0,1,mean.true,sd.true,tol,m1,mmax);
  if (length(n.lo)>1) sim_effit('lo_norm',n=n.lo[2],m=m,d0=d0,1,mean.true,sd.true,tol,m1,mmax);
  sim_effit('hi_norm',n=n.hi,m=m,d0=d0,1,mean.true,sd.true,tol,m1,mmax);
  ## mixtures
  mean.mix=c(mean.true,mean.false);
  sd.mix=c(sd.true,sd.false);
  sim_effit('lo_lo',n=n.lo[1],m=m,d0=d0,prop.lo,mean.mix,sd.mix,tol,m1,mmax);
  if (length(n.lo)>1) sim_effit('lo_lo',n=n.lo[2],m=m,d0=d0,prop.lo,mean.mix,sd.mix,tol,m1,mmax);
  sim_effit('hi_lo',n=n.hi,m=m,d0=d0,prop.lo,mean.mix,sd.mix,tol,m1,mmax);
  sim_effit('lo_hi',n=n.lo[1],m=m,d0=d0,prop.hi,mean.mix,sd.mix,tol,m1,mmax);
  if (length(n.lo)>1) sim_effit('lo_hi',n=n.lo[2],m=m,d0=d0,prop.hi,mean.mix,sd.mix,tol,m1,mmax);
  sim_effit('hi_hi',n=n.hi,m=m,d0=d0,prop.hi,mean.mix,sd.mix,tol,m1,mmax);
  invisible();
}
## use norMix so same code will work for norm and mixture
sim_effit=function(id,n,m,d0,prop.true,mean.mix,sd.mix,tol,m1,mmax) {
  w=if(prop.true==1) 1 else c(prop.true,1-prop.true);
  mix=norMix(mu=mean.mix,sigma=sd.mix,w=w);
  sim=dosim(id,n,m,d0,d.gen=rnorMix,d.args=list(obj=mix),tol,m1,mmax);
  invisible(sim);
}
vrnorm=Vectorize(rnorm,"mean");
dosim=function(id,n,m,d0,d.gen,d.args,tol,m1,mmax) {
  param(verbose,debug);
  if (verbose) {
    if (identical(d.gen,rnorMix))
      expect=paste('expect',
                   round(emix(mix=d.args$obj,n=n,m=m,d0=d0,tol=tol,mmax=mmax)/m1),'iters')
    else expect=NULL;
    print(paste(
      collapse=' ',c('>>> dosim:',nvq(id,n,m,d0),expect,format(Sys.time(),"%b %d %X"))));
  }
  sim=data.frame(row.names=NULL);
  m1.sum=0; i=0;
  while(nrow(sim)<m&&m1.sum<mmax) {
    m1=min(m1,mmax-m1.sum);           # overly cautious, but why not?
    group0=replicate(m1,rnorm(n,mean=0));
    d=do.call(d.gen,c(n=m1,d.args));
    group1=vrnorm(n,mean=d);
    mean0=colMeans(group0);
    mean1=colMeans(group1);
    d.raw=mean1-mean0;
    sd0=apply(group0,2,sd);
    sd1=apply(group1,2,sd);
    sd=pooled_sd(sd0,sd1);
    d.sdz=d.raw/sd;
    sim1=data.frame(n,d.pop=d,d.sdz,sd,d.raw,mean0,mean1,sd0,sd1,row.names=NULL);
    sim=rbind(sim,subset(sim1,subset=near(d.sdz,d0,tol)));
    m1.sum=m1.sum+m1; 
    if (debug) {
      i=i+1;
      print(paste(sep=' ','+++ dosim:',nvq(i),paste_nv('nrow',nrow(sim)),expect));
    }
  }
  if (nrow(sim)<m)
    warning(paste('dosim failed to generate enough rows. wanted',m,'got',nrow(sim)))
  else sim=sim[1:m,];
  save_sim(sim,n,m,d0,id);
  invisible(sim);
}
## for debug output
emix=function(mix,n,m,d0,tol,mmax) {
  f=function()
    integrate(function(d.pop) dnorMix(d.pop,mix)*d_d2t(n=n,d=d0,d0=d.pop),-Inf,Inf)$value*2*tol;
  uniroot(function(m.need) m.need*f()-m,interval=c(1,mmax))$root;
}
## file functions
filename_sim=function(n,m,d0,id) 
  filename(param(simdir),base=paste(sep='_','sim',id),
           tail=paste(sep=',',paste_nv(n),paste_nv(m,m_pretty(m)),paste_nv(d0,d_pretty(d0))),
           suffix='RData');
save_sim=function(sim,n,m,d0,id) save(sim,file=filename_sim(n,m,d0,id));

load_sim=get_sim=function(n,m,d0,id,tol=param(tol),prune=F) {
  sim=load_(filename_sim(n,m,d0,id),'sim')
  if (prune) sim=subset(sim,subset=near(d.sdz,d0,tol));
  invisible(sim);
}



