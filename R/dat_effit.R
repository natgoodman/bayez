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
  ## NG 19-09-16: still undecided what cases to run. might as well run 'em all
  ## normals - use norMix so same code will work for norm and mixture
  cases=expand.grid(n=n,d0=d0,mean.true=mean.true,sd.true=sd.true);
  withrows(cases,case,{
    sim_effit('norm',n,m,d0,1,mean.true,sd.true,tol=tol,m1=m1,mmax=mmax);
  });
  ## mixtures
  cases=expand.grid(n=n,prop.true=prop.true,d0=d0,mean.true=mean.true,sd.true=sd.true,
                    mean.false=mean.false,sd.false=sd.false);
  withrows(cases,case,{
    mean.mix=c(mean.true,mean.false);
    sd.mix=c(sd.true,sd.false);
    sim_effit('mix',n,m,d0,prop.true,mean.true,sd.true,mean.false,sd.false,
              tol=tol,m1=m1,mmax=mmax);
  });
  invisible();
}
## use norMix so same code will work for norm and mixture
sim_effit=function(id,n,m,d0,prop.true,mean.true,sd.true,mean.false=NULL,sd.false=NULL,
                   tol,m1,mmax) {
  mean.mix=c(mean.true,mean.false);
  sd.mix=c(sd.true,sd.false);
  if(prop.true==1) {
    ## norm
    w=1;
    file=filename_norm(n,m,d0,mean.true,sd.true);
  } else {
    ## mixture
    w=c(prop.true,1-prop.true);
    file=filename_mix(n,m,d0,prop.true,mean.true,sd.true,mean.false,sd.false);
  }
  mix=norMix(mu=mean.mix,sigma=sd.mix,w=w);
  sim=dosim(file,n,m,d0,d.gen=rnorMix,d.args=list(obj=mix),tol,m1,mmax);
  invisible(sim);
}
vrnorm=Vectorize(rnorm,"mean");
dosim=function(file,n,m,d0,d.gen,d.args,tol,m1,mmax,
               save=param(save.sim),save.txt=param(save.txt.sim)) {
  param(verbose,debug);
  ## if file exists and we aren't saving, return saved sim
  if (file.exists(file)&&(is.na(save)||!save)) {
    if (verbose) print(paste('>>> dosim:',shortname_sim(file),'skipping'));
    sim=load_(file,'sim')
  } else {
    if (verbose) {
      if (identical(d.gen,rnorMix))
        expect=paste('expect',
                     round(emix(mix=d.args$obj,n=n,m=m,d0=d0,tol=tol,mmax=mmax)/m1),'iters')
      else expect=NULL;
      print(paste(
        collapse=' ',c('>>> dosim:',shortname_sim(file),expect,format(Sys.time(),"%b %d %X"))));
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
    save_(sim,file,save,save.txt);
  }
  invisible(sim);
}
## file functions
filename_norm=function(n,m,d0,mean,sd) 
  filename(param(simdir),base='sim_norm',
           tail=paste(sep=',',paste_nv(n),paste_nv(m,m_pretty(m)),paste_nv(d0,d_pretty(d0)),
                      paste_nv(mean,d_pretty(mean)),paste_nv(sd,sd_pretty(sd))),
           suffix='RData');
filename_mix=function(n,m,d0,prop.true,mean.true,sd.true,mean.false,sd.false) 
  filename(param(simdir),base='sim_mix',
           tail=paste(sep=',',paste_nv(n),paste_nv(m,m_pretty(m)),paste_nv(d0,d_pretty(d0)),
                      paste_nv(pt,prop.true),
                      paste_nv(mt,d_pretty(mean.true)),paste_nv(sdt,sd_pretty(sd.true)),
                      paste_nv(mf,d_pretty(mean.false)),paste_nv(sdf,sd_pretty(sd.false))),
           suffix='RData');

## filename_sim=function(n,m,d0,id) 
##   filename(param(simdir),base=paste(sep='_','sim',id),
##            tail=paste(sep=',',paste_nv(n),paste_nv(m,m_pretty(m)),paste_nv(d0,d_pretty(d0))),
##            suffix='RData');
## save_sim=function(sim,n,m,d0,id) save(sim,file=filename_sim(n,m,d0,id));

load_sim=get_sim=function(file,tol=param(tol),prune=F) {
  sim=load_(file,'sim')
  if (prune) sim=subset(sim,subset=near(d.sdz,d0,tol));
  invisible(sim);
}

## for verbose or debug output
emix=function(mix,n,m,d0,tol,mmax) {
  f=function()
    integrate(function(d.pop) dnorMix(d.pop,mix)*d_d2t(n=n,d=d0,d0=d.pop),-Inf,Inf)$value*2*tol;
  uniroot(function(m.need) m.need*f()-m,interval=c(1,mmax))$root;
}
shortname_sim=function(file) sub('^sim_','',desuffix(basename(file)));

           


