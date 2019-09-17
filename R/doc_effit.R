#################################################################################
##
## Author:  Nat Goodman
## Created: 19-09-10
##          from misig/doc_confi.R created 19-07-16
##          from misig/confi.R created 19-07-04
##
## Copyright (C) 2019 Nat Goodman.
## 
## Generate figures and tables for confi document
##
## This software is open source, distributed under the MIT License. See LICENSE
## file at https://github.com/natgoodman/NewPro/FDR/LICENSE 
##
#################################################################################
## --- Generate Figures and Tables for effit Blog Post ---
library(nor1mix);
## no sections. only 4 figures
doc_effit=function(sect=NULL,...) {
  init(doc='effit',...);
  param(n,m,prop.true,d0,mean.true,sd.true,mean.false,sd.false);
  ## define params for norm cases
  n.lo=10; n.hi=200;
  mean.lo=0.3; mean.hi=0.7; mean.f=0;   # lo, hi - norm and true mix cases. f false mix cases
  sd.norm=sd.t=0.1; sd.f=0.05;          # sd.t norm and true mix cases. sd.f false mix cases
  ## prop.lo=min(prop.true); prop.hi=max(prop.true);
  prop.mid=0.5;
  ## draw the figures  
  ## NG 19-09-03: REAL HACK. hardcode xlim,ylim to put all figures on same scale
  xlim=c(-0.2,1.2);
  ylim=c(0,6);
  ## normals - use norMix so same code will work for norm and mixture
  figblk_start();
  fig_effit('norm_lo_lo',n.lo,1,m,d0,mean.lo,sd.norm,xlim=xlim,ylim=ylim);
  fig_effit('norm_hi_lo',n.hi,1,m,d0,mean.lo,sd.norm,xlim=xlim,ylim=ylim);
  fig_effit('norm_lo_hi',n.lo,1,m,d0,mean.hi,sd.norm,xlim=xlim,ylim=ylim);
  fig_effit('norm_hi_hi',n.hi,1,m,d0,mean.hi,sd.norm,xlim=xlim,ylim=ylim);
  ## mixtures. now only doing 50:50s
  figblk_start();
  fig_effit('mix_lo_lo',n.lo,prop.mid,m,d0,mean.lo,sd.t,mean.f,sd.f,xlim,ylim);
  fig_effit('mix_hi_lo',n.hi,prop.mid,m,d0,mean.lo,sd.t,mean.f,sd.f,xlim,ylim);
  fig_effit('mix_lo_hi',n.lo,prop.mid,m,d0,mean.hi,sd.t,mean.f,sd.f,xlim,ylim);
  fig_effit('mix_hi_hi',n.hi,prop.mid,m,d0,mean.hi,sd.t,mean.f,sd.f,xlim,ylim);
  ##
  invisible();
}
## do one figure of effit doc. shows results for one simulation id
fig_effit=function(id,n,prop.true,m,d0,mean.true,sd.true,mean.false=NULL,sd.false=NULL,xlim,ylim) {
  mean.mix=c(mean.true,mean.false);
  sd.mix=c(sd.true,sd.false);
  if(prop.true==1) {
    ## norm
    w=1;
    title=figtitle('Normal prior',n=n,mean=mean.true,d.obs=d0);
    file=filename_norm(n,m,d0,mean.true,sd.true);
  } else {
    ## mixture
    w=c(prop.true,1-prop.true);
    title=figtitle('Mixture prior',n=n,mean.true=mean.true,prop.true=prop.true,d.obs=d0)
    file=filename_mix(n,m,d0,prop.true,mean.true,sd.true,mean.false,sd.false);
  }
  mix=norMix(mu=mean.mix,sigma=sd.mix,w=w);
  sim=load_sim(file);
  dofig(plot_effit,id,sim=sim,title=title,n=n,d0=d0,mix=mix, 
         breaks=25,xlim=xlim,ylim=ylim);
}
## plot histogram, bayesian distributions, medians
## adapted from plothist_dpop
plot_effit=
  function(sim,n,d0,mix,
           title=NULL,cex.title='auto',legend='right',
           xlab='d.pop',ylab='probability density',xlim=NULL,ylim=NULL,
           col.hist='grey90',border.hist='grey80',breaks='Sturges',
           col.distr=RColorBrewer::brewer.pal(3,'Set1'),lwd.distr=2,lty.distr='solid',
           vlty='dashed',vlwd=1,vdigits=2,vcol.dobs='grey50',
           ...){
    distr=cq(bayes,prior);
    ld=length(distr);
    prior=prior_mix(mix);
    median_prior=median_mix(mix);
    sim=sim[sim$n==n,];
    col=setNames(col.distr[1:ld],distr);
    lwd=setNames(rep(lwd.distr,len=ld),distr);
    lty=setNames(rep(lty.distr,len=ld),distr);
    d.pop=sim$d.pop;
    init_bayes(n=n,d0=d0,prior=prior);
    hist.obj=hist(d.pop,breaks=breaks,plot=FALSE);
    if (is.null(xlim)) xlim=range(hist.obj$breaks); # default per R refman
    if (is.null(ylim)) {
      ## compute max y value across hist and all distributions
      ymax=max(hist.obj$density,
               optimize(d_bayes,c(-10,10),maximum=T)$objective,
               optimize(prior,c(-10,10),maximum=T)$objective);
      ylim=c(0,ymax);
    }
    if (is.null(cex.title)|cex.title=='auto') cex.title=cex_title(title);
    ## plot histogram
    plot(hist.obj,freq=F,main=title,cex.main=cex.title,
         col=col.hist,border=border.hist,xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim,...);
    ## add distributions
    x=seq(xlim[1],xlim[2],len=1000);
    y=cbind(bayes=d_bayes(x),prior=prior(x));
    matlines(x,y,col=col,lty=lty,lwd=lwd);
    ## plot medians and d0
    ## TODO: be smarter about label
    vline=c(d0=d0,bayes=median_bayes(),prior=median_prior());
    vcol=c(vcol.dobs,col);
    vlab=c(TRUE,FALSE,FALSE);
    vhline(vline=vline,vlab=vlab,vhdigits=vdigits,lty=vlty,col=vcol,lwd=vlwd);
    ## add grid and legend
    grid();
    legend(legend,legend=cq(posterior,prior),title=NULL,col=col,lwd=lwd,lty=lty,cex=0.8,bty='n');
  }

##### helper functions from misig/plot.R
## plot horizontal and vertical line segments
vhline=function(vline=NULL,hline=NULL,vlab=TRUE,hlab=TRUE,vhdigits=2,col=NA,...) {
  xylim=par('usr');
  vline=vline[which(between(vline,xylim[1],xylim[2]))];
  hline=hline[which(between(hline,xylim[3],xylim[4]))];
  abline(v=vline,h=hline,col=col,...);
  ## write vhline values along axes
  vline=vline[vlab];
  if (length(vline)>0)
    mtext(round(vline,vhdigits),side=1,at=vline,col=col,line=0.25,cex=0.75);
  line=hline[hlab];
  if (length(hline)>0)
    mtext(round(hline,vhdigits),side=2,at=hline,col=col,line=0.25,cex=0.75);
}
## auto-scale title
cex_title=function(title) {
  xyplt=par('plt');                     # dimensions of plot region
  xplt=xyplt[2]-xyplt[1];               # width of plot region
  min(1,xplt/strwidth(title,units='fig'));
}

